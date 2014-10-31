(ns kinect.usb
  (:require [clojure.core.async :refer :all
             :exclude [map into reduce merge take partition partition-by]]
            [clojure.core.async.impl.protocols :as impl])
  (:import (org.usb4java LibUsb Device DeviceList DeviceHandle)
           (org.usb4java HotplugCallback HotplugCallbackHandle)
           (org.usb4java DeviceDescriptor)))

(defprotocol UsbDevice
  (open [device] [device interface]))

(defn init
  []
  (assert (== (LibUsb/init nil) LibUsb/SUCCESS) "Unable to initialize libusb"))

(defn exit
  []
  (LibUsb/exit nil))

(defonce ctx (init))

(defn must-detach?
  [handle interface-num]
  (and (LibUsb/hasCapability LibUsb/CAP_SUPPORTS_DETACH_KERNEL_DRIVER)
       (LibUsb/kernelDriverActive handle interface-num)))

(defn ensure-kernel-detached
  [handle interface-num]
  (when (and handle interface-num (must-detach? handle interface-num))
    (assert (== (LibUsb/detachKernelDriver handle interface-num) LibUsb/SUCCESS)
            "Unable to detach USB kernel driver")))

(defn ensure-kernel-reattached
  [handle interface-num]
  (when (and handle interface-num (must-detach? handle interface-num))
    (assert (== (LibUsb/attachKernelDriver handle interface-num) LibUsb/SUCCESS)
            "Unable to re-attach USB kernel driver")))

(declare device-descriptor decode-bcd usb-version)

(defn usb-device
  [device]
  (let [handle (volatile! nil)
        interface (volatile! nil)]
    (reify 
      clojure.lang.IDeref
      (deref [_] device)

      UsbDevice
      (open [this]
        (.close this)
        (assert (== (LibUsb/open device (vreset! handle (DeviceHandle.)))
                    LibUsb/SUCCESS) "Unable to open USB device handle")
        this)
      (open [this interface-num]
        (let [handle (do (open this) @handle)
              interface-num (vreset! interface interface-num)]
          (ensure-kernel-detached handle interface-num)
          (assert (== (LibUsb/claimInterface handle interface-num)
                      LibUsb/SUCCESS) "Unable to claim USB interface")
          this))

      java.io.Closeable
      (close [_]
        (when-let [i @interface]
          (LibUsb/releaseInterface @handle i)
          (ensure-kernel-reattached @handle i)
          (vreset! interface nil))
        (when-let [h @handle]
          (LibUsb/close h)
          (vreset! handle nil)))

      clojure.lang.ILookup
      (valAt [this key] (.valAt this key nil))
      (valAt [this key not-found]
        (let [d (device-descriptor this)]
          (case key
            :spec (decode-bcd (.bcdDevice d))
            :device (decode-bcd (.bcdUSB d))
            :device-class (.bDeviceClass d)
            :device-subclass (.bDeviceSubClass d)
            :vendor-id (.idVendor d)
            :product-id (.idProduct d)
            not-found))))))

(defn devices
  []
  (let [device-list (DeviceList.)
        result (assert (>= (LibUsb/getDeviceList nil device-list) 0)
                       "Unable to load USB devices")
        usb-devices (into [] (map usb-device) device-list)]
    (LibUsb/freeDeviceList device-list true)
    usb-devices))

(defn device-descriptor
  [device]
  (let [descriptor (org.usb4java.DeviceDescriptor.)]
    (assert (== (LibUsb/getDeviceDescriptor device descriptor) LibUsb/SUCCESS)
            "Unable to read device descriptor")
    descriptor))

(defmethod print-method org.usb4java.Device
  [device writer]
  (.write writer (.dump (device-descriptor device))))

(defn decode-bcd
  [bcd]
  (org.usb4java.DescriptorUtils/decodeBCD bcd))

(defn usb-version
  [device]
  (decode-bcd (.bcdUSB (device-descriptor @device))))

(defn usb3-devices
  []
  (into [] (filter #(= (usb-version %) "3.00")) (devices)))

(defn hotplug-event-stream
  [xform]
  (assert (LibUsb/hasCapability LibUsb/CAP_HAS_HOTPLUG)
          "The current machine does not support hotplugged USB devices")

  (let [out (chan 1 xform)]
    (future
      (let [cb (reify HotplugCallback
                 (processEvent [this ctx device event user-data]
                   (put! out (usb-device device))
                   (int 0)))
            cb-handle (HotplugCallbackHandle.)
            result (LibUsb/hotplugRegisterCallback
                    ctx
                    (bit-or LibUsb/HOTPLUG_EVENT_DEVICE_ARRIVED
                            LibUsb/HOTPLUG_EVENT_DEVICE_LEFT)
                    LibUsb/HOTPLUG_ENUMERATE
                    LibUsb/HOTPLUG_MATCH_ANY
                    LibUsb/HOTPLUG_MATCH_ANY
                    LibUsb/HOTPLUG_MATCH_ANY
                    cb nil cb-handle)]
        (assert (== result LibUsb/SUCCESS) "Unable to configure hotplug")
        (while (not (impl/closed? out))
          (assert (== (LibUsb/handleEventsTimeout ctx 1000000)
                      LibUsb/SUCCESS) "libusb event handler failed"))
        (println "Deregistering libusb event handler...")
        (LibUsb/hotplugDeregisterCallback ctx cb-handle)))
    out))
