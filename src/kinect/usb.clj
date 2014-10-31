(ns kinect.usb
  (:import (org.usb4java LibUsb Device DeviceList DeviceHandle)
           (org.usb4java DescriptorUtils)))

(defn init
  []
  (assert (== (LibUsb/init nil) LibUsb/SUCCESS) "Unable to initialize libusb"))

(defn exit
  []
  (LibUsb/exit nil))

(defn devices
  []
  (let [device-list (DeviceList.)
        result (assert (>= (LibUsb/getDeviceList nil device-list) 0)
                       "Unable to load USB devices")
        usb-devices (into [] device-list)]
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
  (decode-bcd (.bcdUSB (device-descriptor device))))

(defn usb3-devices
  []
  (into [] (filter #(= (usb-version %) "3.00")) (devices)))
