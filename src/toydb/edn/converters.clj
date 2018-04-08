(ns toydb.edn.converters
  (:refer-clojure :exclude [read-string])
  (:require [clojure.pprint :as pp])
  (:require [jfxutils.core :as jfxc])
  (:require [clojure.edn :as edn])
  (:require [toydb.edn.colors])
  (:require [toydb.units]))

;; Converters for javafx classes.
;; Print-methods for defrecords, deftypes, etc., go where they are defined

(defmethod print-dup javafx.scene.paint.Color [c writer]
  (let [color-string (toydb.edn.colors/reverse-color-map
                      (str c)                  ;; lookup by the hex part as key
                      (str c)
                      #_(format "%f %f %f %f"    ;; default value when key not found
                              (.getRed c) (.getGreen c) (.getBlue c) (.getOpacity c)))]
    (.write writer (format "#Color[%s]" color-string))))




(defn read-string
  "Forwards to clojure.edn/read-string with reader-map as options"
  [s]
  (let [default-reader (fn [t v]  (println "Default reader:" t v))
        readers {'Color toydb.edn.colors/color
                 'Distance toydb.units/distance}
        reader-map {:readers readers, :default default-reader}]
    (edn/read-string reader-map s)))



