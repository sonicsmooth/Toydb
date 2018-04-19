(ns toydb.units
  (require [jfxutils.core :refer [printexp clip round-to-nearest]]
           [clojure.pprint :as pp]))


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
  (decr [u])) ;; Returns new instance of u rounded down to the next integer

;; Override simple-dispatch and print-dup
;; This way with *print-dup* false, it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-method for deftypes
;; With *print-dup* true it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-dup for units

(defn dopm [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d)) (value d))))
(defmethod print-method Nanometer  [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Micrometer [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Millimeter [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Centimeter [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Meter      [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Kilometer  [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Mil        [d, ^java.io.Writer writer] (dopm d writer))
(defmethod print-method Inch       [d, ^java.io.Writer writer] (dopm d writer))

(defn dopd [d, ^java.io.Writer writer, ustr] (.write writer (format "#Distance[%s %s]"   (value d) ustr)))
(defmethod print-dup Nanometer  [d, ^java.io.Writer writer] (dopd d writer "nm"))
(defmethod print-dup Micrometer [d, ^java.io.Writer writer] (dopd d writer "um"))
(defmethod print-dup Millimeter [d, ^java.io.Writer writer] (dopd d writer "mm"))
(defmethod print-dup Centimeter [d, ^java.io.Writer writer] (dopd d writer "cm"))
(defmethod print-dup Meter      [d, ^java.io.Writer writer] (dopd d writer "m"))
(defmethod print-dup Kilometer  [d, ^java.io.Writer writer] (dopd d writer "km"))
(defmethod print-dup Mil        [d, ^java.io.Writer writer] (dopd d writer "mil"))
(defmethod print-dup Inch       [d, ^java.io.Writer writer] (dopd d writer "inch"))

(def KMPIN (/ 254.0 10000000))
(def MPIN  (* KMPIN 1000))
(def CMPIN (* KMPIN 100000))
(def MMPIN (* KMPIN 1000000))
(def UMPIN (* KMPIN 1000000000))
(def NMPIN (* KMPIN 1000000000000))

(def KMPMIL (/ 254.0 10000000000))
(def MPMIL  (* KMPMIL 1000))
(def CMPMIL (* KMPMIL 100000))
(def MMPMIL (* KMPMIL 1000000))
(def UMPMIL (* KMPMIL 1000000000))
(def NMPMIL (* KMPMIL 1000000000000))

(def INPKM (/ 1 KMPIN))
(def INPM  (/ 1 MPIN))
(def INPCM (/ 1 CMPIN))
(def INPMM (/ 1 MMPIN))
(def INPUM (/ 1 UMPIN))
(def INPNM (/ 1 NMPIN))

(def MILPKM (/ 1 KMPMIL))
(def MILPM  (/ 1 MPMIL))
(def MILPCM (/ 1 CMPMIL))
(def MILPMM (/ 1 MMPMIL))
(def MILPUM (/ 1 UMPMIL))
(def MILPNM (/ 1 NMPMIL))



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

  java.lang.String
  (km [s] (km (distance s km)))
  (m [s] (m (distance s m)))
  (cm [s] (cm (distance s cm)))
  (mm [s] (mm (distance s mm)))
  (um [s] (um (distance s um)))
  (mil [s] (mil (distance s mil)))
  (inch [s] (inch (distance s inch)))
  
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
  (decr [n] nil))

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
   ;; Called directly from above functions o from single-arity
   (let [reg-nm #"(.*)(nm)"
         reg-um #"(.*)(um)"
         reg-mm #"(.*)(mm)"
         reg-cm #"(.*)(cm)"
         reg-m #"([^k]*)(m)"
         reg-km #"(.*)(km)"
         reg-mil #"(.*)(mil|mils)"
         reg-inch #"(.*)(in|inch|inches)"
         reg-generic #"(.*)"]
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

(defn- fn-name [unit]
  (condp = unit
    nm "nm"
    um "um"
    mm "mm"
    cm "cm"
    m "m"
    km "km"
    mil "mil"
    inch "inch"))

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

(defn test-inch
  "Tests conversion from inch to cm and back."
  [^long n]
  (loop [n n
         i (inch 1)]
    (if (zero? n)
      i
      (recur (dec n) (inch (cm i))))))




