(ns kinect.core
  (:gen-class)
  (:require [clojure.core.async :refer :all
             :exclude [map into reduce merge take partition partition-by]]
            [kinect.usb :as usb :refer [*kinect*]])
  (:import org.usb4java.LibUsb))

(defn -main
  [& args]
  (usb/with-kinect
    (let [address (LibUsb/getDeviceAddress *kinect*)
          bus-number (LibUsb/getBusNumber *kinect*)
          descriptor (org.usb4java.DeviceDescriptor.)
          _ (LibUsb/getDeviceDescriptor *kinect* descriptor)
          vendor-id (.idVendor descriptor)
          product-id (.idProduct descriptor)
          fmt "Bus %03d, Address %03d: Vendor %04x, Product %04x%n"]
      (printf fmt bus-number address vendor-id product-id))))
