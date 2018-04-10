(ns toydb.units2
  (require [jfxutils.core :refer [printexp clip round-to-nearest]]
           [clojure.pprint :as pp]))


;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(deftype nanometer  [^long nm])
(deftype micrometer [^long nm])
(deftype millimeter [^long nm])
(deftype centimeter [^long nm])
(deftype meter      [^long nm])
(deftype kilometer  [^long nm])
(deftype mils       [^long nm])
(deftype inches     [^long nm])


;; Override simple-dispatch and print-dup
;; This way with *print-dup* false, it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-method for deftypes
;; With *print-dup* true it'll go:
;;   pprint->default simple-dispatch->pr-str->custom print-dup for units

(declare value)
(defmethod print-method nanometer  [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method micrometer [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method millimeter [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method centimeter [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method meter      [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method kilometer  [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method mils       [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))
(defmethod print-method inches     [d, ^java.io.Writer writer] (.write writer (format "#%s{%s}" (pr-str (class d))  (value d))))

(defmethod print-dup nanometer  [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s nm]"   (value d))))
(defmethod print-dup micrometer [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s um]"   (value d))))
(defmethod print-dup millimeter [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s mm]"   (value d))))
(defmethod print-dup centimeter [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s cm]"   (value d))))
(defmethod print-dup meter      [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s m]"    (value d))))
(defmethod print-dup kilometer  [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s km]"   (value d))))
(defmethod print-dup mils       [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s mil]"  (value d))))
(defmethod print-dup inches     [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s inch]" (value d))))

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

(defprotocol DistanceProtocol
  (value [u])
  (km [u])
  (m [u])
  (cm [u])
  (mm [u])
  (um [u])
  (nm [u])
  (mil [u])
  (inch [u])
  (nearest [u eps])
  (add [u x])
  (sub [u x])
  (incr [u])
  (decr [u]))

;; The record is mils, the function is mil
;; The record is inches, the function is inch
(declare distance)
(declare increment)
(declare decrement)

(extend-protocol DistanceProtocol
  kilometer
  (value [u] (* 1e-12 (.nm u)))
  (km [u] u)
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (kilometer. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (kilometer. (+ (.nm u) (.nm x))))
  (sub [u x] (kilometer. (- (.nm u) (.nm x))))
  (incr [u] (kilometer. (increment (nanometer. (value u)))))
  (decr [u] (kilometer. (decrement (nanometer. (value u)))))

  meter
  (value [u] (* 1e-9 (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] u)
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (meter. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (meter. (+ (.nm u) (.nm x))))
  (sub [u x] (meter. (- (.nm u) (.nm x))))
  (incr [u] (meter. (increment (nanometer. (value u)))))
  (decr [u] (meter. (decrement (nanometer. (value u)))))

  centimeter
  (value [u] (* 1e-7 (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] u)
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (centimeter. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (centimeter. (+ (.nm u) (.nm x))))
  (sub [u x] (centimeter. (- (.nm u) (.nm x))))
  (incr [u] (centimeter. (increment (nanometer. (value u)))))
  (decr [u] (centimeter. (decrement (nanometer. (value u)))))

  millimeter
  (value [u] (* 1e-6 (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] u)
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (millimeter. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (millimeter. (+ (.nm u) (.nm x))))
  (sub [u x] (millimeter. (- (.nm u) (.nm x))))
  (incr [u] (millimeter. (increment (nanometer. (value u)))))
  (decr [u] (millimeter. (decrement (nanometer. (value u)))))

  micrometer
  (value [u] (* 1e-3 (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] u)
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (micrometer. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (micrometer. (+ (.nm u) (.nm x))))
  (sub [u x] (micrometer. (- (.nm u) (.nm x))))
  (incr [u] (micrometer. (increment (nanometer. (value u)))))
  (decr [u] (micrometer. (decrement (nanometer. (value u)))))

  nanometer
  (value [u] (.nm u))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] u)
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (nanometer. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (nanometer. (+ (.nm u) (.nm x))))
  (sub [u x] (nanometer. (- (.nm u) (.nm x))))
  (incr [u] (nanometer. (increment (nanometer. (value u)))))
  (decr [u] (nanometer. (decrement (nanometer. (value u)))))

  mils
  (value [u] (* MILPNM (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (mils. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (mils. (+ (.nm u) (.nm x))))
  (sub [u x] (mils. (- (.nm u) (.nm x))))
  (incr [u] (mils. (increment (nanometer. (value u)))))
  (decr [u] (mils. (decrement (nanometer. (value u)))))

  inches
  (value [u] (* INPNM (.nm u)))
  (km [u] (kilometer. (.nm u)))
  (m  [u] (meter. (.nm u)))
  (cm [u] (centimeter. (.nm u)))
  (mm [u] (millimeter. (.nm u)))
  (um [u] (micrometer. (.nm u)))
  (nm [u] (nanometer. (.nm u)))
  (mil [u] (mils. (.nm u)))
  (inch [u] (inches. (.nm u)))
  (nearest [u eps] (inches. (long (round-to-nearest (.nm u) (value eps)))))
  (add [u x] (inches. (+ (.nm u) (.nm x))))
  (sub [u x] (inches. (- (.nm u) (.nm x))))
  (incr [u] (inches. (increment (nanometer. (value u)))))
  (decr [u] (inches. (decrement (nanometer. (value u)))))

  java.lang.Long
  (value [u] u)
  (km [u] (kilometer. (* 1e12 u)))
  (m [u] (meter. (* 1e9 u)))
  (cm [u] (centimeter. (* 1e7 u)))
  (mm [u] (millimeter. (* 1e6 u)))
  (um [u] (micrometer. (* 1e3 u)))
  (nm [u] (nanometer. u))
  (mil [u] (mils. (* 25.4e3 u)))
  (inch [u] (inches. (* 25.4e6 u)))
  (nearest [u eps] (Long. (round-to-nearest (.double u) eps)))
  (incr [u] (long (increment (double u))))
  (decr [u] (long (decrement (double u))))

  java.lang.Double
  (value [u] u)
  (km [u] (kilometer. (* 1e12 u)))
  (m [u] (meter. (* 1e9 u)))
  (cm [u] (centimeter. (* 1e7 u)))
  (mm [u] (centimeter. (* 1e6 u)))
  (um [u] (micrometer. (* 1e3 u)))
  (nm [u] (nanometer. u))
  (mil [u] (mils. (* 25.4e3 u)))
  (inch [u] (mils. (* 25.4e6 u)))
  (nearest [u eps] (Long. (round-to-nearest (.double u) eps)))
  (incr [u] (long (increment (double u))))
  (decr [u] (long (decrement (double u))))

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
  centimeter object with value 2.54.  If unit and hint-fn are both
  omitted, then a micrometer object is returned with the given value.
  If unit is omitted but hint-fn is provided, then the object from
  hint-fn is returned with the given value.  If both unit and hint-fn
  are provided, then the unit takes precedence.  Inches can be
  abbreviated 'in' or 'inch' or 'inches'.  Thousanths of an inch can
  be abbreviated 'mil' or 'mils'."
  ([str hint-fn]
   ;; Called directly from above functions o from single-arity
   (let [reg-um #"(.*)(um)"
         reg-mm #"(.*)(mm)"
         reg-cm #"(.*)(cm)"
         reg-m #"([^k]*)(m)"
         reg-km #"(.*)(km)"
         reg-mil #"(.*)(mil|mils)"
         reg-inch #"(.*)(in|inch|inches)"
         reg-generic #"(.*)"]
     (try
       (condp re-matches str
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
     (distance s um)
     (distance (apply str s) um)
     
     )
   ))

(defn- fn-name [unit]
  (condp = unit
    um "um"
    mm "mm"
    cm "cm"
    m "m"
    km "km"
    mil "mil"
    inch "inch"))

(defn distance-string-converter
  "Returns proxy of StringConverter, using function unit, which must be
  one of um, mm, cm, m, km, mil, inch."
  ([unit min max]
   (proxy [javafx.util.StringConverter] []
     (toString
       ([] "custom string converter")
       ([distance] (if distance
                     (try
                       (format "%.7g" (.value (unit distance)))
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
              new-val (if new-unit (clip (.value new-unit) min max) nil)]
          (unit new-val))))))
  ([unit]
     (distance-string-converter unit Double/MIN_VALUE Double/MAX_VALUE )))

(defn distance-text-formatter
  "Returns new TextFormatter using a distance converter which returns
  unit.  Unit must be one of um, mm, cm, m, km, mil, inch."
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




