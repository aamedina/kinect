(ns kinect.usb
  (:import (org.usb4java LibUsb Device DeviceList DeviceHandle)
           (org.usb4java DescriptorUtils)))

(defprotocol UsbDevice
  (open [device] [device interface]))

(defn init
  []
  (assert (== (LibUsb/init nil) LibUsb/SUCCESS) "Unable to initialize libusb"))

(defn exit
  []
  (LibUsb/exit nil))

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
          (vreset! handle nil))))))

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
  (DescriptorUtils/decodeBCD bcd))

(defn usb-version
  [device]
  (decode-bcd (.bcdUSB (device-descriptor @device))))

(defn usb3-devices
  []
  (into [] (filter #(= (usb-version %) "3.00")) (devices)))


