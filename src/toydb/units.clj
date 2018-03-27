(ns toydb.units
  (require [jfxutils.core :refer [printexp clip round-to-nearest]]
           [clojure.pprint :as pp]))


;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(defprotocol DistanceProtocol
  (km [u])
  (m [u])
  (cm [u])
  (mm [u])
  (um [u])
  (mil [u])
  (inch [u])
  (nearest [u eps])
  (add [u x])
  (sub [u x])
  (incr [u])
  (decr [u]))


;; For defrecords, toString does not appear to be used by either pr[n][-str] or pprint
;; We override simple-dispatch for pprint


(defrecord micrometer [^double value])
(defrecord millimeter [^double value])
(defrecord centimeter [^double value])
(defrecord meter      [^double value])
(defrecord kilometer  [^double value])
(defrecord mils       [^double value])
(defrecord inches     [^double value])


;; Override simple-dispatch and print-dup]
;; This way with *print-dup* false, it'll go:
;;   pprint->simple-dispatch->pr-str->default print-method for defrecords
;; With *print-dup* true it'll go:
;;   pprint->simple-dispatch->pr-str->custom print-dup for units

(defmethod pp/simple-dispatch micrometer [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch millimeter [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch centimeter [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch meter      [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch kilometer  [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch mils       [d] (.write *out* (pr-str d)))
(defmethod pp/simple-dispatch inches     [d] (.write *out* (pr-str d)))

(defmethod print-dup micrometer [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s um]"   (.value d))))
(defmethod print-dup millimeter [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s mm]"   (.value d))))
(defmethod print-dup centimeter [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s cm]"   (.value d))))
(defmethod print-dup meter      [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s m]"    (.value d))))
(defmethod print-dup kilometer  [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s km]"   (.value d))))
(defmethod print-dup mils       [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s mil]"  (.value d))))
(defmethod print-dup inches     [d, ^java.io.Writer writer] (.write writer (format "#Distance[%s inch]" (.value d))))

(def KMPIN (/ 254 10000000))
(def MPIN  (* KMPIN 1000))
(def CMPIN (* KMPIN 100000))
(def MMPIN (* KMPIN 1000000))
(def UMPIN (* KMPIN 1000000000))

(def KMPMIL (/ 254 10000000000))
(def MPMIL  (* KMPMIL 1000))
(def CMPMIL (* KMPMIL 100000))
(def MMPMIL (* KMPMIL 1000000))
(def UMPMIL (* KMPMIL 1000000000))

(def INPKM (/ 1 KMPIN))
(def INPM  (/ 1 MPIN))
(def INPCM (/ 1 CMPIN))
(def INPMM (/ 1 MMPIN))
(def INPUM (/ 1 UMPIN))

(def MILPKM (/ 1 KMPMIL))
(def MILPM  (/ 1 MPMIL))
(def MILPCM (/ 1 CMPMIL))
(def MILPMM (/ 1 MMPMIL))
(def MILPUM (/ 1 UMPMIL))



;; The record is mils, the function is mil
;; The record is inches, the function is inch
(declare distance)
(declare increment)
(declare decrement)

(extend-protocol DistanceProtocol
  kilometer
  (km [u] u)
  (m  [u] (->meter       (* 1e3 (.value u))))
  (cm [u] (->centimeter  (* 1e5 (.value u))))
  (mm [u] (->millimeter  (* 1e6 (.value u))))
  (um [u] (->micrometer  (* 1e9 (.value u))))
  (mil [u] (->mils (* MILPKM (.value u))))
  (inch [u] (->inches (* INPKM (.value u))))
  (nearest [u eps] (->kilometer (round-to-nearest (.value u) eps)))
  (add [u x] (->kilometer (+ (.value u) x)))
  (sub [u x] (->kilometer (- (.value u) x)))
  (incr [u] (->kilometer (increment (.value u))))
  (decr [u] (->kilometer (decrement (.value u))))
  
  meter
  (km [u] (->kilometer   (* 1e-3 (.value u))))
  (m  [u] u)
  (cm [u] (->centimeter  (* 1e2 (.value u))))
  (mm [u] (->millimeter  (* 1e3 (.value u))))
  (um [u] (->micrometer  (* 1e6 (.value u))))
  (mil [u] (->mils (* MILPM (.value u))))
  (inch [u] (->inches (* INPM  (.value u))))
  (nearest [u eps] (->meter (round-to-nearest (.value u) eps)))
  (add [u x] (->meter (+ (.value u) x)))
  (sub [u x] (->meter (- (.value u) x)))
  (incr [u] (->meter (increment (.value u))))
  (decr [u] (->meter (decrement (.value u))))

  centimeter
  (km [u] (->kilometer   (* 1e-5 (.value u))))
  (m  [u] (->meter       (* 1e-2 (.value u))))
  (cm [u] u)
  (mm [u] (->millimeter  (* 1e1 (.value u))))
  (um [u] (->micrometer  (* 1e4 (.value u))))
  (mil [u] (->mils (* MILPCM (.value u))))
  (inch [u] (->inches (* INPCM (.value u))))
  (nearest [u eps] (->centimeter (round-to-nearest (.value u) eps)))
  (add [u x] (->centimeter (+ (.value u) x)))
  (sub [u x] (->centimeter (- (.value u) x)))
  (incr [u] (->centimeter (increment (.value u))))
  (decr [u] (->centimeter (decrement (.value u))))
  
  millimeter
  (km [u] (->kilometer   (* 1e-6 (.value u))))
  (m  [u] (->meter       (* 1e-3 (.value u))))
  (cm [u] (->centimeter  (* 1e-1 (.value u))))
  (mm [u] u)
  (um [u] (->micrometer  (* 1e3  (.value u))))
  (mil [u] (->mils (* MILPMM (.value u))))
  (inch [u] (->inches (* INPMM (.value u))))
  (nearest [u eps] (->millimeter (round-to-nearest (.value u) eps)))
  (add [u x] (->millimeter (+ (.value u) x)))
  (sub [u x] (->millimeter (- (.value u) x)))
  (incr [u] (->millimeter (increment (.value u))))
  (decr [u] (->millimeter (decrement (.value u))))
  
  micrometer
  (km [u] (->kilometer   (* 1e-9 (.value u))))
  (m  [u] (->meter       (* 1e-6 (.value u))))
  (cm [u] (->centimeter  (* 1e-4 (.value u))))
  (mm [u] (->millimeter  (* 1e-3 (.value u))))
  (um [u] u)
  (mil [u] (->mils (* MILPUM (.value u))))
  (inch [u] (->inches (* INPUM (.value u))))
  (nearest [u eps] (->micrometer (round-to-nearest (.value u) eps)))
  (add [u x] (->micrometer (+ (.value u) x)))
  (sub [u x] (->micrometer (- (.value u) x)))
  (incr [u] (->micrometer (increment (.value u))))
  (decr [u] (->micrometer (decrement (.value u))))
  
  inches
  (km [u] (->kilometer (* KMPIN (.value u))))
  (m [u] (->meter (* MPIN (.value u))))
  (cm [u] (->centimeter (* CMPIN (.value u))))
  (mm [u] (->millimeter (* MMPIN (.value u))))
  (um [u] (->micrometer (* UMPIN (.value u))))
  (mil [u] (->mils (* 1e3 (.value u))))
  (inch [u] u)
  (nearest [u eps] (->inches (* eps (Math/round (/ (.value u) eps)))))
  (add [u x] (->inches (+ (.value u) x)))
  (sub [u x] (->inches (- (.value u) x)))
  (incr [u] (->inches (increment (.value u))))
  (decr [u] (->inches (decrement (.value u))))
  
  mils
  (km [u] (->kilometer (* KMPMIL (.value u))))
  (m [u] (->meter (* MPMIL (.value u))))
  (cm [u] (->centimeter (* CMPMIL (.value u))))
  (mm [u] (->millimeter (* MMPMIL (.value u))))
  (um [u] (->micrometer (* UMPMIL (.value u))))
  (mil [u] u)
  (inch [u] (->inches (* 1e-3 (.value u))))
  (nearest [u eps] (->mils (round-to-nearest (.value u) eps)))
  (add [u x] (->mils (+ (.value u) x)))
  (sub [u x] (->mils (- (.value u) x)))
  (incr [u] (->mils (increment (.value u))))
  (decr [u] (->mils (decrement (.value u))))
  
  java.lang.Long
  (km [u] (->kilometer u))
  (m  [u] (->meter u))
  (cm [u] (->centimeter u))
  (mm [u] (->millimeter u))
  (um [u] (->micrometer u))
  (mil [u] (->mils u))
  (inch [u] (->inches u))
  (nearest [u eps] (Long. (round-to-nearest (.double u) eps)))
  (incr [u] (long (increment (double u))))
  (decr [u] (long (decrement (double u))))

  java.lang.Double
  (km [u] (->kilometer u))
  (m  [u] (->meter u))
  (cm [u] (->centimeter u))
  (mm [u] (->millimeter u))
  (um [u] (->micrometer u))
  (mil [u] (->mils u))
  (inch [u] (->inches u))
  (nearest [u eps] (round-to-nearest u eps))
  (incr [u] (increment u))
  (decr [u] (decrement u))

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




