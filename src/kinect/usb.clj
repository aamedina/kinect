(ns kinect.usb
  (:import org.usb4java.LibUsb))

(defprotocol Describable
  (descriptor [o]))

(defmethod print-method org.usb4java.Device
  [x writer]
  (.write writer (.dump (descriptor x))))

(defn assert-success
  [code]
  (assert (zero? code) (org.usb4java.LibUsbException. code)))

(extend-protocol Describable
  org.usb4java.Device
  (descriptor [device]
    (let [d (org.usb4java.DeviceDescriptor.)]
      (assert-success (LibUsb/getDeviceDescriptor device d))
      d)))

(defmacro with-config-descriptor
  [[binding device] & body]
  `(let [~binding (org.usb4java.ConfigDescriptor.)
         code# (LibUsb/getActiveConfigDescriptor ~device ~binding)
         _# (assert-success code#)
         ret# (do ~@body)]
     (LibUsb/freeConfigDescriptor ~binding)
     ret#))

(defmacro with-bos-descriptor
  [[binding handle] & body]
  `(let [~binding (org.usb4java.BosDescriptor.)
         code# (LibUsb/getBosDescriptor ~handle ~binding)
         _# (assert-success code#)
         ret# (do ~@body)]
     (LibUsb/freeBosDescriptor ~binding)
     ret#))

(defmacro with-device-list
  [binding & body]
  `(let [~binding (org.usb4java.DeviceList.)
         code# (LibUsb/getDeviceList nil ~binding)
         _# (assert (pos? code#) (org.usb4java.LibUsbException. code#))
         ret# (do ~@body)]
     (LibUsb/freeDeviceList ~binding true)
     ret#))

(defn reset
  [handle]
  (LibUsb/resetDevice handle))

(defn device
  [vendor-id product-id]
  (with-device-list device-list
    (->> device-list
         (filter #(let [d (descriptor %)]
                    (and (== (.idVendor d) vendor-id)
                         (== (.idProduct d) product-id))))
         first)))

(defn open
  ([device]
     (let [handle (org.usb4java.DeviceHandle.)]
       (assert-success (LibUsb/open device handle))
       (try
         (doto handle
           (LibUsb/resetDevice))
         (catch Throwable t
           (LibUsb/close handle)
           (throw t)))))
  ([vendor-id product-id]
     (let [handle (LibUsb/openDeviceWithVidPid nil vendor-id product-id)]
       (try
         (doto handle
           (LibUsb/resetDevice))
         (catch Throwable t
           (LibUsb/close handle)
           (throw t))))))

(defmacro with-handle
  [[binding device] & body]
  `(let [~binding (open ~device)
         ret# (do ~@body)]
     (LibUsb/close ~binding)
     ret#))
