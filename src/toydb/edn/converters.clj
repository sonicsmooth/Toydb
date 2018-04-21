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

(defn- distance-readers->object []
  {'Distance toydb.units/distance
   'nm toydb.units/nm
   'um toydb.units/um
   'mm toydb.units/mm
   'cm toydb.units/cm
   'm  toydb.units/m
   'km toydb.units/km
   'mil 'toydb.units/mil
   'inch 'toydb.units/inch})

(defn- distance-readers->double [unitfn]
  {'Distance #(toydb.units/value (toydb.units/distance %))
   'nm #(toydb.units/value (unitfn (toydb.units/nm %)))
   'um #(toydb.units/value (unitfn (toydb.units/um %)))
   'mm #(toydb.units/value (unitfn (toydb.units/mm %)))
   'cm #(toydb.units/value (unitfn (toydb.units/cm %)))
   'm  #(toydb.units/value (unitfn (toydb.units/m %)))
   'km #(toydb.units/value (unitfn (toydb.units/km %)))
   'mil #(toydb.units/value (unitfn (toydb.units/mil %)))
   'inch #(toydb.units/value (unitfn (toydb.units/inch %)))})

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
   (edn/read-string {:readers (merge (distance-readers->object) jfx-readers)
                     :default default-reader} s))
  ([unitfn s]
   (edn/read-string {:readers (merge (distance-readers->double unitfn) jfx-readers)
                     :default default-reader} s)))



