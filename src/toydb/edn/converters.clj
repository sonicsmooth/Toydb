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


(defn circle
  ;; Create jfx Circle with key-value pairs from arg.
  ;; each key is a property of the circle, each value the value
  [arg]
  (let [circ (javafx.scene.shape.Circle.)]
    (doseq [[k v] arg]
      (jfxc/set-prop-val-from-symbol! circ k v))
    circ))





(def jfx-readers
  {'Color toydb.edn.colors/color
   'Circle circle})

(defn default-reader [tag value]
  (println "Default reader:" tag value)
  [tag value])

(defn read-string
  "Forwards to clojure.edn/read-string with reader-map as options.
  With one string argument s, any distance objects are returned as is.
  If the first argument is instead a function and the the second
  argument the string, then the function is presumed to be the unit
  function, in which case the distance object is passed through this
  first, from which the floating point value is extracted."
  ([s]
   (edn/read-string {:readers (merge (toydb.units/distance-readers->object) jfx-readers)
                     :default default-reader} s))
  ([unitfn s]
   (edn/read-string {:readers (merge (toydb.units/distance-readers->double unitfn) jfx-readers)
                     :default default-reader} s)))



