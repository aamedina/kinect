(ns kinect.core
  (:gen-class)
  (:require [clojure.tools.logging :as log]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.java.io :as io]
            [com.stuartsierra.component :as c :refer [Lifecycle]]
            [kinect.usb :as usb])
  (:import (org.usb4java LibUsb)
           (io.netty.buffer ByteBuf ByteBufUtil Unpooled)
           (java.nio ByteBuffer ByteOrder IntBuffer)
           (java.util.concurrent.atomic AtomicInteger)))

(declare ^:dynamic *kinect*)

(def ^:const vendor-id 0x045e)
(def ^:const product-id 0x02c4)
(def ^:const control-in (unchecked-byte 0x81))
(def ^:const control-out 0x02)
(def ^:const unknown-in (unchecked-byte 0x82))
(def ^:const rgb-in (unchecked-byte 0x83))
(def ^:const infrared-in (unchecked-byte 0x84))

(defn configure-kinect
  [handle]
  (let [config-id (IntBuffer/allocate 1)]
    (usb/assert-success (LibUsb/getConfiguration handle config-id))
    (when-not (== (.get config-id) 1)
      (usb/assert-success (LibUsb/setConfiguration handle 1)))))

(defn deconfigure-kinect
  [handle]
  (let [config-id (IntBuffer/allocate 1)]
    (usb/assert-success (LibUsb/getConfiguration handle config-id))
    (when (== (.get config-id) 1)
      (usb/assert-success (LibUsb/setConfiguration handle 0)))))

(defn set-isochronous-delay
  [handle]
  (usb/assert-success
   (LibUsb/controlTransfer handle LibUsb/RECIPIENT_DEVICE LibUsb/SET_ISOCH_DELAY
                           40 0 (ByteBuffer/allocateDirect 0) 1000)))

(defn configure-infrared
  [handle enabled?]
  (usb/assert-success
   (LibUsb/setInterfaceAltSetting handle 1 (if enabled? 1 0))))

(defn enable-power-states
  [handle]
  (usb/assert-success
   (LibUsb/controlTransfer handle LibUsb/RECIPIENT_DEVICE
                           LibUsb/REQUEST_SET_FEATURE
                           48 0 (ByteBuffer/allocateDirect 0) 1000))
  (usb/assert-success
   (LibUsb/controlTransfer handle LibUsb/RECIPIENT_DEVICE
                           LibUsb/REQUEST_SET_FEATURE
                           49 0 (ByteBuffer/allocateDirect 0) 1000)))

(defn configure-rgb
  [handle enabled?]
  (let [idx (-> (unchecked-byte 0)
                (bit-or (if-not enabled? 1 0))
                unchecked-byte
                (bit-or (if-not enabled? 2 0))
                unchecked-byte
                (bit-shift-left 8)
                unchecked-byte
                (bit-or 0)
                unchecked-byte)]
    (usb/assert-success
     (LibUsb/controlTransfer handle LibUsb/RECIPIENT_INTERFACE
                             LibUsb/REQUEST_SET_FEATURE
                             0 idx (ByteBuffer/allocateDirect 0) 1000))))

(defn write!
  [handle data]
  (let [transferred (IntBuffer/allocate 1)]
    (usb/assert-success
     (LibUsb/bulkTransfer handle control-out (.nioBuffer data)
                          transferred 1000))))

(defn read!
  [handle endpoint size timeout]
  (let [buffer (.order (ByteBuffer/allocateDirect size) ByteOrder/LITTLE_ENDIAN)
        transferred (IntBuffer/allocate 1)]
    (usb/assert-success
     (LibUsb/bulkTransfer handle endpoint buffer transferred timeout))
    (-> (Unpooled/wrappedBuffer buffer)
        (.slice 0 (.get transferred))
        (.order ByteOrder/LITTLE_ENDIAN))))

(def ^:const rgb-packet-size (+ (* 1920 1080 3) 20))

(defn read-rgb!
  ([] (read-rgb! (:handle *kinect*)))
  ([handle] (read! handle rgb-in rgb-packet-size 1000)))

(def ^:const infrared-packet-size 0x8400)

(def ^:const threads (.availableProcessors (Runtime/getRuntime)))

(defn read-infrared!
  ([] (read-infrared! (:handle *kinect*)))
  ([handle]
     (let [buffer (ByteBuffer/allocateDirect (* 80 8 infrared-packet-size))
           cb (reify org.usb4java.TransferCallback
                (processTransfer [this transfer]
                  (LibUsb/freeTransfer transfer)))]
       (dotimes [i 80]
         (let [transfer (doto (LibUsb/allocTransfer 8)
                          (LibUsb/fillIsoTransfer
                           handle infrared-in buffer
                           8 cb nil 1000)
                          (.setType LibUsb/TRANSFER_TYPE_ISOCHRONOUS)
                          (LibUsb/setIsoPacketLengths infrared-packet-size))]
           (LibUsb/submitTransfer transfer)
           (LibUsb/controlTransfer handle )
           (while (not (== (.status transfer) LibUsb/TRANSFER_COMPLETED))
             )))
       #_(-> (Unpooled/wrappedBuffer buffer)
           (.slice 0 (.actualLength transfer))
           (.order ByteOrder/LITTLE_ENDIAN)))))

(defn stream-rgb!
  ([] (stream-rgb! (:handle *kinect*)))
  ([handle] (stream-rgb! handle 24))
  ([handle fps] (stream-rgb! handle fps 48))
  ([handle fps ^long max-frames]
     (let [agents (repeatedly threads #(agent []))]
       (loop [frame 0
              [a & more] (cycle agents)
              images []]
         (if (>= frame max-frames)
           (into [] (map deref) images)
           (let [image (promise)]
             (send a (fn [_]
                       (loop []
                         (if (zero? (LibUsb/tryLockEvents nil))
                           (if (zero? (LibUsb/eventHandlingOk nil))
                             (do (LibUsb/unlockEvents nil)
                                 (recur))
                             (do (deliver image (read-rgb! handle))
                                 (LibUsb/unlockEvents nil)))
                           (recur)))))
             (recur (inc frame) more (conj images image))))))))

(defn max-response-length
  [op]
  (case op
    :read-firmware-versions 0x200
    :init-streams 0x00
    :read-data 0x5c
    :read-status 0x04
    :read-data-page 0x22 
    :read-data-stream 0x10
    :set-streaming 0x00
    :set-mode 0x00
    :read-serial-number 0x80
    :read-p0-tables 0x1c0000
    :read-depth-camera-parameters 0x1c0000
    :read-rgb-camera-parameters 0x1c0000
    :unknown-one 0x00))

(defn opcode
  [op]
  (case op
    :read-firmware-versions 0x02 
    :init-streams 0x09
    :read-data 0x14 
    :read-status 0x16
    :read-data-page 0x22 
    :read-data-stream 0x26 
    :set-streaming 0x2b 
    :set-mode 0x4b 
    :read-serial-number 0x22
    :read-p0-tables 0x22
    :read-depth-camera-parameters 0x22
    :read-rgb-camera-parameters 0x22
    :unknown-one 0x0a))

(defn command
  [op sequence & parameters]
  (let [data (doto (.order (Unpooled/directBuffer) ByteOrder/LITTLE_ENDIAN)
               (.writeInt 0x06022009)
               (.writeInt sequence)
               (.writeInt (max-response-length op))
               (.writeInt (opcode op))
               (.writeInt 0))]
    (doseq [parameter parameters]
      (.writeInt data parameter))
    {:op op
     :size (max-response-length op)
     :sequence sequence
     :data data}))

(defn complete?
  [data sequence]
  (and (== (.readInt data) 0x0A6FE000)
       (== (.readInt data) sequence)))

(defn exec
  ([cmd] (exec (:handle *kinect*) cmd))
  ([handle {:keys [op size data sequence] :as cmd}]
     (write! handle data)
     (let [data (when (pos? size)
                  (read! handle control-in size 1000))
           result (read! handle control-in 16 1000)]
       (when (complete? result sequence)
         (log/info op "completed successfully")
         data))))

(defrecord Kinect [device handle sequence]
  Lifecycle
  (start [this]
    (LibUsb/init nil)
    (LibUsb/setDebug nil LibUsb/LOG_LEVEL_INFO)
    (let [handle (usb/open vendor-id product-id)
          device (LibUsb/getDevice handle)
          sequence (AtomicInteger.)]
      (log/info "Opening Kinect...")
      (LibUsb/setAutoDetachKernelDriver handle true)
      (configure-kinect handle)
      (usb/assert-success (LibUsb/claimInterface handle 0))
      (usb/assert-success (LibUsb/claimInterface handle 1))
      (set-isochronous-delay handle)
      (configure-infrared handle false)
      (enable-power-states handle)
      (configure-rgb handle false)
      (configure-rgb handle true)
      (log/info "Starting Kinect...")
      (exec handle (command :read-firmware-versions
                            (.getAndIncrement sequence)))
      (exec handle (command :read-data (.getAndIncrement sequence)))
      (exec handle (command :read-serial-number (.getAndIncrement sequence) 1))
      (exec handle (command :read-depth-camera-parameters
                            (.getAndIncrement sequence) 3))
      (exec handle (command :read-p0-tables (.getAndIncrement sequence) 2))
      (exec handle (command :read-rgb-camera-parameters
                            (.getAndIncrement sequence) 4))
      (exec handle (command :read-status (.getAndIncrement sequence) 0x090000))
      (exec handle (command :init-streams (.getAndIncrement sequence)))
      (configure-infrared handle true)
      (exec handle (command :read-status (.getAndIncrement sequence) 0x090000))
      (exec handle (command :set-streaming (.getAndIncrement sequence) 1))
      (assoc this
        :device device
        :handle handle
        :sequence sequence)))
  (stop [this]
    (log/info "Stopping Kinect...")
    (configure-infrared handle false)
    (exec handle (command :unknown-one (.getAndIncrement sequence)))
    (exec handle (command :set-streaming (.getAndIncrement sequence) 0))
    (configure-rgb handle false)
    (log/info "Closing Kinect...")
    (usb/assert-success (LibUsb/releaseInterface handle 0))
    (usb/assert-success (LibUsb/releaseInterface handle 1))
    (deconfigure-kinect handle)
    (LibUsb/close handle)
    (LibUsb/exit nil)
    (assoc this
      :device nil
      :handle nil
      :sequence nil)))

(defonce ^:dynamic *kinect* (Kinect. nil nil nil))

(defmacro with-kinect
  [& body]
  `(binding [*kinect* (c/start *kinect*)]
     (let [ret# (do ~@body)]
       (c/stop *kinect*)
       ret#)))

(defn start
  []
  (alter-var-root #'*kinect* c/start))

(defn stop
  []
  (alter-var-root #'*kinect* c/stop))

(defn reset
  []
  (stop)
  (refresh :after 'kinect.core/start))

(defn spit-infrared
  []
  (let [buf (read-infrared! (:handle *kinect*))
        bytes (byte-array (.readableBytes buf))]
    (.readBytes buf bytes)
    (when (pos? (alength bytes))
      (with-open [os (io/output-stream "target/infrared-image")]
        (.write os bytes)))))

(defn spit-rgb
  []
  (let [buf (read-rgb! (:handle *kinect*))
        bytes (byte-array (.readableBytes buf))]
    (.readBytes buf bytes)
    (when (pos? (alength bytes))
      (with-open [os (io/output-stream "target/rgb-image")]
        (.write os bytes)))))

(defn -main
  [& args]
  (with-kinect
    (let [handle (:handle *kinect*)]
      (try
        (let [buf (read-rgb! handle)
              bytes (byte-array (.readableBytes buf))]
          (.readBytes buf bytes)
          (when (pos? (alength bytes))
            (with-open [os (io/output-stream "target/rgb-image")]
              (.write os bytes))))
        (catch Throwable t
          (log/error t)))))
  (shutdown-agents))
