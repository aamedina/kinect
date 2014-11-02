(ns kinect.usb
  (:require [clojure.core.async :refer :all
             :exclude [map into reduce merge take partition partition-by]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.tools.logging :as log])
  (:import (java.nio ByteBuffer ByteOrder)
           (io.netty.buffer ByteBuf Unpooled ByteBufUtil)
           (org.usb4java LibUsb Device DeviceList DeviceHandle
                         HotplugCallback HotplugCallbackHandle
                         DeviceDescriptor Transfer TransferCallback)))

(def ^:dynamic *kinect* nil)
(def ^:dynamic *context* nil)
(def ^:dynamic *handle* nil)
(def ^:dynamic *interface* nil)

(def kinect-usb-device
  {:vendor-id 0x045e
   :product-id 0x02c4
   :max-packet-size 0x40
   :control-in-interface 0x81
   :control-out-interface 0x02
   :rgb-transfer-in-interface 0x83
   :unknown-interrupt-interface 0x82
   :isochronous-ir-transfer-in-interface 0x84})

(defn assert-success
  [code]
  (assert (zero? code) (org.usb4java.LibUsbException. code)))

(defmacro with-context
  [& body]
  `(binding [*context* (org.usb4java.Context.)]
     (LibUsb/init *context*)
     (LibUsb/setDebug *context* LibUsb/LOG_LEVEL_INFO)
     (let [ret# (do ~@body)]
       (LibUsb/exit *context*)
       ret#)))

(defmacro with-devices
  [binding & body]
  `(let [~binding (org.usb4java.DeviceList.)
         code# (LibUsb/getDeviceList *context* ~binding)
         _# (assert (>= code# 0) (org.usb4java.LibUsbException. code#))
         ret# (do ~@body)]
     (LibUsb/freeDeviceList ~binding true)
     ret#))

(defn descriptor
  [device]
  (let [d (org.usb4java.DeviceDescriptor.)]
    (LibUsb/getDeviceDescriptor device d)
    d))

(defmethod print-method org.usb4java.Device
  [x writer]
  (.write writer (.dump (descriptor x))))

(defn kinect?
  [device]
  (let [d (descriptor device)]
    (and (== (:vendor-id kinect-usb-device) (.idVendor d))
         (== (:product-id kinect-usb-device) (.idProduct d)))))

(defn max-iso-packet-size
  ([] (max-iso-packet-size *kinect*))
  ([device] (max-iso-packet-size device (unchecked-byte 0x81)))
  ([device endpoint]
     (LibUsb/getMaxIsoPacketSize device endpoint)))

(defmacro with-handle
  [device & body]
  `(binding [*handle* (org.usb4java.DeviceHandle.)]
     (LibUsb/open ~device *handle*)
     (let [ret# (do ~@body)]
       (LibUsb/close *handle*)
       ret#)))

(definline detach?
  [interface]
  `(and (LibUsb/hasCapability LibUsb/CAP_SUPPORTS_DETACH_KERNEL_DRIVER)
        (LibUsb/kernelDriverActive *handle* ~interface)))

(defmacro with-interface
  [interface & body]
  `(let [bool# ~(detach? interface)]
     (when bool#
       (assert-success (LibUsb/detachKernelDriver *handle* ~interface)))
     (assert-success (LibUsb/claimInterface *handle* ~interface))
     (let [ret# (do ~@body)]
       (when bool#
         (assert-success (LibUsb/attachKernelDriver *handle* ~interface)))
       (assert-success (LibUsb/releaseInterface *handle* ~interface))
       ret#)))

(defmacro with-kinect
  [& body]
  `(with-context
     (with-devices devices#
       (binding [*kinect* (first (filter kinect? devices#))]
         (with-handle *kinect*
           (with-interface 0
             (with-interface 1
               (let [ret# (do ~@body)]
                 ret#))))))))
