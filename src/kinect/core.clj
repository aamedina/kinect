(ns kinect.core
  (:gen-class)
  (:require [clojure.core.async :refer :all
             :exclude [map into reduce merge take partition partition-by]]
            [kinect.usb :as usb]))

(def kinect-usb-device
  {:vendor-id 0x045E
   :product-id 0x02D8})

(defn kinect?
  [device]
  (and (= (:vendor-id device) 0x045E)
       (= (:product-id device) 0x02D8)))

(defn -main
  [& args]
  (usb/init)
  (let [in (usb/hotplug-event-stream (comp (filter kinect?)
                                           (map #(doto % println))))]
    (println "Press enter to exit the program")
    (read-line)
    (close! in)
    (usb/exit)))
