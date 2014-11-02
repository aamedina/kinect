(ns kinect.usb.commands
  (:require [clojure.tools.logging :as log])
  (:import java.util.concurrent.atomic.AtomicInteger))

(def ^:dynamic *sequence* nil)

(defmacro with-sequence
  [& body]
  `(binding [*sequence* (AtomicInteger.)]
     ~@body))

(defn op
  [opcode]
  (case opcode
    0x02 :read-firmware-versions
    0x09 :init-streams
    0x14 :read-data
    0x16 :read-status
    0x22 :read-data-page
    0x26 :read-data-stream
    0x2b :set-streaming
    0x4b :set-mode
    :unknown))

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
    :read-rgb-camera-parameters 0x22))

(defmulti max-response-length :op :default :unknown)

(defmethod max-response-length :read-firmware-versions
  [command]
  0x200)

(defmethod max-response-length :init-streams
  [command]
  0x00)

(defmethod max-response-length :read-data
  [command]
  0x5c)

(defmethod max-response-length :read-status
  [command]
  0x04)

(defmethod max-response-length :read-data-page
  [command]
  (case (:op command)
    :read-serial-number 0x80
    :read-p0-tables 0x1c0000
    :read-depth-camera-parameters 0x1c0000
    :read-rgb-camera-parameters 0x1c0000))

(defmethod max-response-length :read-data-stream
  [command]
  0x10)

(defmethod max-response-length :set-streaming
  [command]
  0x00)

(defmethod max-response-length :set-mode
  [command]
  0x00)

(defmethod max-response-length :unknown
  [command]
  (log/info "Unknown command: " command))
