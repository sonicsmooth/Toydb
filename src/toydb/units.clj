(ns toydb.units
  (:require [jfxutils.core :refer [printexp clip round-to-nearest]]
            [clojure.pprint :as pp]
            toydb.edn.finalize
   ))


;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(deftype Nanometer  [^long nm])
(deftype Micrometer [^long nm])
(deftype Millimeter [^long nm])
(deftype Centimeter [^long nm])
(deftype Meter      [^long nm])
(deftype Kilometer  [^long nm])
(deftype Mil        [^long nm])
(deftype Inch       [^long nm])
(deftype Parsec     [^long nm])

;; Hierarchy is used when finalizing an intermediate representation
(derive Nanometer ::Distance)
(derive Micrometer ::Distance)
(derive Millimeter ::Distance)
(derive Centimeter ::Distance)
(derive Meter ::Distance)
(derive Kilometer ::Distance)
(derive Mil ::Distance)
(derive Inch ::Distance)

(defprotocol DistanceProtocol
  (value [u]) ;; returns the distance as a double, eg 10.0 for mm
  (km [u])    ;; returns a new instance of km with u nanometers
  (m [u])     ;; etc...
  (cm [u])
  (mm [u])
  (um [u])
  (nm [u])
  (mil [u])
  (inch [u])
  (nearest [u eps]) ;; returns instance of nearest unit within eps
  (add [u x]) ;; Sums nanometers of u and x, returns new instance of u
  (sub [u x]) ;; Subtracts nanometers of u and x, returns new instance of u
  (incr [u])  ;; Returns new instance of u rounded up to the next integer
  (decr [u])  ;; Returns new instance of u rounded down to the next integer
  (suffix [u])) ;; Returns a string for the suffix, eg "nm", etc.


;; Used by viewdef and shape c'tors in toydb.edn.Shapes
;; Change this to scale the whole world

;; This var is intended to be changed to one of the distance functions
;; (eg cm, um, etc.).  If left nil, then each distance object is
;; writtent as is, eg a 10 centimeter object will be something like
;; #cm(10), but if *print-unit* is um, then all distances will be
;; written in terms of um, so 10 centimeters is written as #um(100000).
(def ^:dynamic *print-unit* nil)


;; Override print-method and print-dup
;; This way with *print-dup* false, it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-method for deftypes
;; With *print-dup* true it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-dup for units

(prefer-method print-method ::Distance java.lang.Object)
(defmethod print-method ::Distance [d, ^java.io.Writer writer]
  (.write writer (format "#%s{%s}" (pr-str (class d)) (value d))))

(prefer-method print-dup ::Distance java.lang.Object)
(defmethod print-dup ::Distance  [d, ^java.io.Writer writer]
  (if-let [ufn *print-unit*]
    (binding [*print-unit* nil]        ; temp unbind for the recursion
      (print-dup (ufn d) writer))
    (.write writer (format "#%s(%s)" (suffix d) (value d) ))))


(def INPNM (/ 1 25400000.0))
(def MILPNM (/ 1 25400.0))


;; The record is Mil, the function is mil
;; The record is Inch, the function is inch
(declare distance)
(declare increment)
(declare decrement)

(extend-protocol DistanceProtocol
  Kilometer
  (value [u] (* 1e-12 (.nm u)))
  (km [u] u)
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Kilometer. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Kilometer. (+ (.nm u) (.nm x))))
  (sub [u x] (Kilometer. (- (.nm u) (.nm x))))
  (incr [u] (km (increment (value u))))
  (decr [u] (km (decrement (value u))))
  (suffix [u] "km")

  Meter
  (value [u] (* 1e-9 (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] u)
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Meter. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Meter. (+ (.nm u) (.nm x))))
  (sub [u x] (Meter. (- (.nm u) (.nm x))))
  (incr [u] (m (increment (value u))))
  (decr [u] (m (decrement (value u))))
  (suffix [u] "m")

  Centimeter
  (value [u] (* 1e-7 (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] u)
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Centimeter. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Centimeter. (+ (.nm u) (.nm x))))
  (sub [u x] (Centimeter. (- (.nm u) (.nm x))))
  (incr [u] (cm (increment (value u))))
  (decr [u] (cm (decrement (value u))))
  (suffix [u] "cm")
  
  Millimeter
  (value [u] (* 1e-6 (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] u)
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Millimeter. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Millimeter. (+ (.nm u) (.nm x))))
  (sub [u x] (Millimeter. (- (.nm u) (.nm x))))
  (incr [u] (mm (increment (value u))))
  (decr [u] (mm (decrement (value u))))
  (suffix [u] "mm")
  
  Micrometer
  (value [u] (* 1e-3 (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] u)
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Micrometer. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Micrometer. (+ (.nm u) (.nm x))))
  (sub [u x] (Micrometer. (- (.nm u) (.nm x))))
  (incr [u] (um (increment (value u))))
  (decr [u] (um (decrement (value u))))
  (suffix [u] "um")
  
  Nanometer
  (value [u] (.nm u))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] u)
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Nanometer. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Nanometer. (+ (.nm u) (.nm x))))
  (sub [u x] (Nanometer. (- (.nm u) (.nm x))))
  (incr [u] (nm (increment (value u))))
  (decr [u] (nm (decrement (value u))))
  (suffix [u] "nm")

  Mil
  (value [u] (* MILPNM (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Mil. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Mil. (+ (.nm u) (.nm x))))
  (sub [u x] (Mil. (- (.nm u) (.nm x))))
  (incr [u] (mil (increment (value u))))
  (decr [u] (mil (decrement (value u))))
  (suffix [u] "mil")
  
  Inch
  (value [u] (* INPNM (.nm u)))
  (km [u] (Kilometer. (.nm u)))
  (m  [u] (Meter. (.nm u)))
  (cm [u] (Centimeter. (.nm u)))
  (mm [u] (Millimeter. (.nm u)))
  (um [u] (Micrometer. (.nm u)))
  (nm [u] (Nanometer. (.nm u)))
  (mil [u] (Mil. (.nm u)))
  (inch [u] (Inch. (.nm u)))
  (nearest [u eps] (Inch. (long (round-to-nearest (.nm u) (.nm eps)))))
  (add [u x] (Inch. (+ (.nm u) (.nm x))))
  (sub [u x] (Inch. (- (.nm u) (.nm x))))
  (incr [u] (inch (increment (value u))))
  (decr [u] (inch (decrement (value u))))
  (suffix [u] "in")

  java.lang.Long
  (value [u] u)
  (km [u] (Kilometer. (* 1e12 u)))
  (m [u] (Meter. (* 1e9 u)))
  (cm [u] (Centimeter. (* 1e7 u)))
  (mm [u] (Millimeter. (* 1e6 u)))
  (um [u] (Micrometer. (* 1e3 u)))
  (nm [u] (Nanometer. u))
  (mil [u] (Mil. (* 25.4e3 u)))
  (inch [u] (Inch. (* 25.4e6 u)))
  (nearest [u eps] (Long. (round-to-nearest (.double u) eps)))
  (incr [u] (long (increment (double u))))
  (decr [u] (long (decrement (double u))))
  (suffix [u] "")
  
  java.lang.Double
  (value [u] u)
  (km [u] (Kilometer. (* 1e12 u)))
  (m [u] (Meter. (* 1e9 u)))
  (cm [u] (Centimeter. (* 1e7 u)))
  (mm [u] (Centimeter. (* 1e6 u)))
  (um [u] (Micrometer. (* 1e3 u)))
  (nm [u] (Nanometer. u))
  (mil [u] (Mil. (* 25.4e3 u)))
  (inch [u] (Inch. (* 25.4e6 u)))
  (nearest [u eps] (Long. (round-to-nearest (.double u) eps)))
  (incr [u] (double (increment (double u))))
  (decr [u] (double (decrement (double u))))
  (suffix [u] "")
  
  java.lang.String
  (km [s] (km (distance s km)))
  (m [s] (m (distance s m)))
  (cm [s] (cm (distance s cm)))
  (mm [s] (mm (distance s mm)))
  (um [s] (um (distance s um)))
  (mil [s] (mil (distance s mil)))
  (inch [s] (inch (distance s inch)))
  (suffix [u] "")
  
  clojure.lang.PersistentList ;; So you can do #km(5) in edn file
  (km [n]   (km   (do (assert (= (count n) 1) "Only one value can be passed to km")   (km (first n)))))
  (m [n]    (m    (do (assert (= (count n) 1) "Only one value can be passed to m")    (m (first n)))))
  (cm [n]   (cm   (do (assert (= (count n) 1) "Only one value can be passed to cm")   (cm (first n)))))
  (mm [n]   (mm   (do (assert (= (count n) 1) "Only one value can be passed to mm")   (mm (first n)))))
  (um [n]   (um   (do (assert (= (count n) 1) "Only one value can be passed to um")   (um (first n)))))
  (nm [n]   (nm   (do (assert (= (count n) 1) "Only one value can be passed to nm")   (nm (first n)))))
  (mil [n]  (mil  (do (assert (= (count n) 1) "Only one value can be passed to mil")  (mil (first n)))))
  (inch [n] (inch (do (assert (= (count n) 1) "Only one value can be passed to inch") (inch (first n)))))
  (suffix [u] "")
  
  nil
  (km [n] nil)
  (m [n] nil)
  (cm [n] nil)
  (mm [n] nil)
  (um [n] nil)
  (mil [n] nil)
  (inch [n] nil)
  (nearest [n e] nil)
  (add [n x] nil)
  (sub [n x] nil)
  (incr [n] nil)
  (decr [n] nil)
  (suffix [u] nil))


;; We put this as a global var because we don't want to pass it around
;; everywhere, but it's not intended to be modified, despite the
;; dynamic.  With earmuffs it just looks global, but Clojure warns
;; when earmuffs are used without dynamic.  We put this down here
;; after all the fns are defined, because putting it up before the fns
;; were defined caused the fn definition in viewdef to fail.
(def ^:dynamic *unitfn* um) 

(extend-protocol toydb.edn.finalize/FinalizeProtocol
  toydb.units.Nanometer  (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Micrometer (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Millimeter (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Centimeter (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Meter      (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Kilometer  (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Mil        (final [u] (toydb.units/value (toydb.units/*unitfn* u)))
  toydb.units.Inch       (final [u] (toydb.units/value (toydb.units/*unitfn* u))))

(defn distance
  "Converts numerical string to units.  eg '2.54cm' becomes a
  Centimeter object with value 2.54.  If unit and hint-fn are both
  omitted, then a Micrometer object is returned with the given value.
  If unit is omitted but hint-fn is provided, then the object from
  hint-fn is returned with the given value.  If both unit and hint-fn
  are provided, then the unit takes precedence.  Inch can be
  abbreviated 'in' or 'inch' or 'inches'.  Thousanths of an inch can
  be abbreviated 'mil' or 'mils'."
  ([str hint-fn]
   ;; Called directly from above functions or from single-arity
   (let [reg-nm #"(.*)(nm)[ ]*"
         reg-um #"(.*)(um)[ ]*"
         reg-mm #"(.*)(mm)[ ]*"
         reg-cm #"(.*)(cm)[ ]*"
         reg-m #"([^k]*)(m)[ ]*"
         reg-km #"(.*)(km)[ ]*"
         reg-mil #"(.*)(mil|mils)[ ]*"
         reg-inch #"(.*)(in|inch|inches)[ ]*"
         reg-generic #"(.*)[ ]*"]
     (try
       (condp re-matches str
         reg-nm :>> #(nm (Double/parseDouble (second %)))
         reg-um :>> #(um (Double/parseDouble (second %)))
         reg-mm :>> #(mm (Double/parseDouble (second %)))
         reg-cm :>> #(cm (Double/parseDouble (second %)))
         reg-m  :>> #(m (Double/parseDouble (second %)))
         reg-km :>> #(km (Double/parseDouble (second %)))
         reg-mil :>> #(mil (Double/parseDouble (second %)))
         reg-inch :>> #(inch (Double/parseDouble (second %)))
         reg-generic :>> #(hint-fn (Double/parseDouble (second %)))
         nil)
       (catch java.lang.NumberFormatException e
         nil))))
  ([s]
   ;; This arity expects simple strings or a vector of symbols from read-string
   (if (instance? java.lang.String s)
     (distance s nm)
     (distance (apply str s) nm))))

(defn distance-string-converter
  "Returns proxy of StringConverter, using function unit, which must be
  one of nm, um, mm, cm, m, km, mil, inch."
  ([unit min max]
   (proxy [javafx.util.StringConverter] []
     (toString
       ([] "custom string converter")
       ([distance] (if distance
                     (try
                       (format "%.7g" (value (unit distance)))
                       (catch NullPointerException e
                         nil))
                     "")))
     (fromString
       ([s]
        ;; Without inner hint and without outer hint, takes on either microns or user-defined units (both incorrect)
        ;; With inner hint and without outer hint, takes on either proper units (correct) or user-defined units
        ;; Without inner hint and with outer hint, takes on either microns converted to units (incorrect), or user-defined units converted to units (correct)
        ;; With inner hint and with outer hint, takes on correct units with or without user-defined units
        (let [new-unit (unit (distance s unit))
              new-val (if new-unit (clip (value new-unit) min max) nil)]
          (unit new-val))))))
  ([unit]
     (distance-string-converter unit Double/MIN_VALUE Double/MAX_VALUE )))

(defn distance-text-formatter
  "Returns new TextFormatter using a distance converter which returns
  unit.  Unit must be one of nm, um, mm, cm, m, km, mil, inch."
  [unit]
  (javafx.scene.control.TextFormatter. (distance-string-converter unit)))
  
(defn increment
  "Find next highest integer"
  [u]
  (if (< (- u (long u)) 1e-12) ;; because of rounding error, the diff might not actually be zero
    (inc u )
    (Math/ceil u )))

(defn decrement
  "Find next lowest integer"
  [u]
  (if (< (- u (long u)) 1e-12) ;; because of rounding error, the diff might not actually be zero
    ( dec u )
    (Math/floor u )))

(defn distance-readers->object []
  {'Distance toydb.units/distance
   'nm toydb.units/nm
   'um toydb.units/um
   'mm toydb.units/mm
   'cm toydb.units/cm
   'm  toydb.units/m
   'km toydb.units/km
   'mil 'toydb.units/mil
   'inch 'toydb.units/inch})

#_(defn distance-readers->double [unitfn]
  {'Distance #(toydb.units/value (toydb.units/distance %))
   'nm #(toydb.units/value (unitfn (toydb.units/nm %)))
   'um #(toydb.units/value (unitfn (toydb.units/um %)))
   'mm #(toydb.units/value (unitfn (toydb.units/mm %)))
   'cm #(toydb.units/value (unitfn (toydb.units/cm %)))
   'm  #(toydb.units/value (unitfn (toydb.units/m %)))
   'km #(toydb.units/value (unitfn (toydb.units/km %)))
   'mil #(toydb.units/value (unitfn (toydb.units/mil %)))
   'inch #(toydb.units/value (unitfn (toydb.units/inch %)))})

