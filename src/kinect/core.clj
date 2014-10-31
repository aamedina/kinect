(ns kinect.core
  (:gen-class)
  (:require [clojure.core.async :refer :all
             :exclude [map into reduce merge take partition partition-by]]
            [kinect.usb :as usb]))

(defn -main
  [& args]
  (usb/init)
  (let [abort (usb/hotplug-chan)]
    (println "Press enter to exit the program")
    (read-line)
    (put! abort true)
    (usb/exit)))
