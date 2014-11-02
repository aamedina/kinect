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
        (pos? (LibUsb/kernelDriverActive *handle* ~interface))))

(defmacro with-config-descriptor
  [[binding config] & body]
  `(let [~binding (org.usb4java.ConfigDescriptor.)
         code# (LibUsb/getConfigDescriptorByValue *kinect* ~config ~binding)
         _# (assert-success code#)
         ret# (do ~@body)]
     (LibUsb/freeConfigDescriptor ~binding)
     ret#))

(defmacro with-detached-kernel-driver
  [interface & body]
  `(let [bool# ~(detach? interface)
         code# (when bool#
                 (LibUsb/detachKernelDriver *handle* ~interface))
         _# (when bool#
              (assert (>= code# 0) (org.usb4java.LibUsbException. code#)))
         ret# (do ~@body)]
     (when bool#
       (assert-success (LibUsb/attachKernelDriver *handle* ~interface)))
     ret#))

(defmacro with-interface
  [interface & body]
  `(do (assert-success (LibUsb/claimInterface *handle* ~interface))
       (let [ret# (do ~@body)]
         (assert-success (LibUsb/releaseInterface *handle* ~interface))
         ret#)))

(defn set-isochronous-delay
  []
  (let [code (LibUsb/controlTransfer *handle*
                                     LibUsb/RECIPIENT_DEVICE
                                     LibUsb/SET_ISOCH_DELAY
                                     40 0 (ByteBuffer/allocateDirect 0) 1000)]
    (assert (>= code 0) "Failed to set isochronous delay")))

(defn enable-power-states
  []
  (let [code1 (LibUsb/controlTransfer *handle*
                                      LibUsb/RECIPIENT_DEVICE
                                      LibUsb/REQUEST_SET_FEATURE
                                      48 0 (ByteBuffer/allocateDirect 0) 1000)
        code2 (LibUsb/controlTransfer *handle*
                                      LibUsb/RECIPIENT_DEVICE
                                      LibUsb/REQUEST_SET_FEATURE
                                      49 0 (ByteBuffer/allocateDirect 0) 1000)]
    (assert (>= code1 0) "Failed to set first power state")
    (assert (>= code2 0) "Failed to set second power state")))

(defn set-video-transfer-function-enabled
  [enabled?]
  (let [opts (bit-or 0 (if-not enabled? 1 0) (if-not enabled? 2 0))
        code (LibUsb/controlTransfer *handle*
                                     LibUsb/RECIPIENT_INTERFACE
                                     LibUsb/REQUEST_SET_FEATURE
                                     0 (bit-or (bit-shift-left opts 8) 0)
                                     (ByteBuffer/allocateDirect 0) 1000)]
    (assert (>= code 0) "Failed to set video transfer function state")))

(defn reset-kinect
  []
  (doto (LibUsb/openDeviceWithVidPid nil 0x045e 0x02c4)
    (LibUsb/resetDevice)))

(defmacro with-companion-descriptor
  [[binding endpoint-desc] & body]
  `(let [~binding (org.usb4java.SsEndpointCompanionDescriptor.)
         code# (LibUsb/getSsEndpointCompanionDescriptor *context*
                                                        ~endpoint-desc
                                                        ~binding)
         _# (assert-success code#)
         ret# (do ~@body)]
     (LibUsb/freeSsEndpointCompanionDescriptor ~binding)
     ret#))

(defn matches-endpoint?
  [endpoint-descriptor endpoint]
  (and (== (.bEndpointAddress endpoint-descriptor)
           (unchecked-byte endpoint))
       (== (bit-and (.bmAttributes endpoint-descriptor) 0x3)
           LibUsb/TRANSFER_TYPE_ISOCHRONOUS)))

(defn bytes-per-interval
  [iface-d endpoint]
  (when-let [e (first (into [] (comp (map #(aget (.endpoint iface-d) %))
                                     (filter #(matches-endpoint? % endpoint)))
                            (range (.bNumEndpoints iface-d))))]
    (with-companion-descriptor [d e]
      (.wBytesPerInterval d))))

(defn ir-max-iso-packet-size
  []
  (with-config-descriptor [d 1]
    (let [xform (comp (map (fn [i] (aget (.iface d) i)))
                      (filter (fn [iface] (> (.numAltsetting iface) 1)))
                      (map (fn [iface] (aget (.altsetting iface) 1)))
                      (map (fn [iface-d] (bytes-per-interval iface-d 0x84)))
                      (remove nil?))
          size (first (into [] xform (range (.bNumInterfaces d))))]
      (assert (>= size (unchecked-short 0x8400))
              "IR bytes per interval too small")
      size)))

(defn set-ir-interface-enabled
  [enabled?]
  (assert-success (LibUsb/setInterfaceAltSetting *handle* 1 (if enabled? 1 0))))

(defmacro with-kinect
  [& body]
  `(with-context
     (binding [*handle* (reset-kinect)]
       (binding [*kinect* (LibUsb/getDevice *handle*)]
         (let [ret# (do (with-interface 0
                          (with-interface 1
                            (set-isochronous-delay)
                            (set-ir-interface-enabled false)
                            (enable-power-states)
                            (set-video-transfer-function-enabled false)
                            (ir-max-iso-packet-size)
                            (set-video-transfer-function-enabled true)
                            ;; (set-ir-interface-enabled true)
                            ~@body)))]
           ;; (set-ir-interface-enabled false)
           (set-video-transfer-function-enabled false)
           (LibUsb/close *handle*)
           ret#)))))
