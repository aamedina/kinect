(defproject kinect "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "Eclipse Public License"
            :url "https://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"sonatype" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha3"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/tools.logging "0.3.1"]
                 [com.cognitect/transit-clj "0.8.259"]
                 [org.usb4java/usb4java "1.2.0"]
                 [com.stuartsierra/component "0.2.2"]
                 [aleph "0.4.0-alpha7"]]
  :jvm-opts ["-server" "-Djava.library.path=native/macosx"]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.7"]]
                   :source-paths ["dev"]}}
  :resource-paths ["resources"
                   "resources/opencv-249.jar"
                   "resources/lwjgl.jar"
                   "resources/lwjgl_util.jar"
                   "resources/lwjgl_util_applet.jar"]
  :main kinect.core)
