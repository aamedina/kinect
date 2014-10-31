(ns kinect.usb
  (:import (org.usb4java LibUsb Device DeviceList DeviceHandle)))

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

(defmethod print-method org.usb4java.Device
  [device writer]
  (let [descriptor (org.usb4java.DeviceDescriptor.)]
    (assert (== (LibUsb/getDeviceDescriptor device descriptor) LibUsb/SUCCESS)
            "Unable to read device descriptor")
    (.write writer (.dump descriptor))))


