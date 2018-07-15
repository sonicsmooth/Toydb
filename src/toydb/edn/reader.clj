(ns toydb.edn.reader
  (:refer-clojure :exclude [read-string])
  (:require clojure.edn
            [clojure.pprint :as pp]
            [jfxutils.core :as jfxc]
            [toydb.edn color shapes]
            toydb.units))

(def jfx-readers
  {'Color  toydb.edn.color/color
   'Circle toydb.edn.shapes/map->Circle})

(defn default-reader [tag value]
  (println "Default reader:" tag value)
  [tag value])

(defn read-string
  "Forwards to clojure.edn/read-string and optional unit-fn, with
  toydb.units/distance-readers and jfx-readers. With one string
  argument s, any distance objects are returned as is.  If optional
  unitfn is provided, then the distance object is passed through
  unitfn first, and then the floating point value is returned.
 
  For example, if no unit fn is provided and the distance encountered
  in the edn file is #cm(10), then a toydb.units.Centimeter{10.0}
  object is returned (which actually is represented internally as 100
  000 000 nanometers).

  If the toydb.units/um function is provided and #cm(10) is
  encountered, then the Double value 100 000.0 is returned; any notion
  of nanometers is lost."
  ([s]
   (clojure.edn/read-string {:readers (merge (toydb.units/distance-readers->object) jfx-readers)
                             :default default-reader} s))
  #_([s unitfn]
   (clojure.edn/read-string {:readers (merge (toydb.units/distance-readers->double unitfn) jfx-readers)
                             :default default-reader} s)))

(defn tp []
  (let [c (read-string (slurp "test/testshapes.edn"))]
    (jfxc/printexp *print-dup*)
    (prn c)
    (clojure.pprint/pprint c)
    (println)
    (binding [*print-dup* true]
      (jfxc/printexp *print-dup*)
      (println)
      (prn c)
      (def rstr (with-out-str (clojure.pprint/pprint c)))
      (clojure.pprint/pprint c))
    (def c c)))



(defn randmap [n]
  (letfn [(randletter []
            (char (+ (int \a)
                     (Math/round (rand 25)))))
          (randstr []
            (apply str (repeatedly n randletter)))]
    (let [keyz (map keyword (repeatedly n randstr))
          valz (repeatedly n randstr)]
      (apply hash-map (interleave keyz valz)))))
