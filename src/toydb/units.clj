(ns toydb.units)


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
  (nearest [u eps]))

(defrecord kilometer [^double value])
(defrecord meter [^double value])
(defrecord centimeter [^double value])
(defrecord millimeter [^double value])
(defrecord micrometer [^double value])
(defrecord mils [^double value]) ;; thousandth of an inch
(defrecord inches [^double value])

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
(extend-protocol DistanceProtocol
  kilometer
  (km [u] u)
  (m  [u] (->meter       (* 1e3 (.value u))))
  (cm [u] (->centimeter  (* 1e5 (.value u))))
  (mm [u] (->millimeter  (* 1e6 (.value u))))
  (um [u] (->micrometer  (* 1e9 (.value u))))
  (mil [u] (->mils (* MILPKM (.value u))))
  (inch [u] (->inches (* INPKM (.value u))))
  (nearest [u eps] (->kilometer (* eps (Math/round (/ (.value u) eps)))))

  meter
  (km [u] (->kilometer   (* 1e-3 (.value u))))
  (m  [u] u)
  (cm [u] (->centimeter  (* 1e2 (.value u))))
  (mm [u] (->millimeter  (* 1e3 (.value u))))
  (um [u] (->micrometer  (* 1e6 (.value u))))
  (mil [u] (->mils (* MILPM (.value u))))
  (inch [u] (->inches (* INPM  (.value u))))
  (nearest [u eps] (->meter (* eps (Math/round (/ (.value u) eps)))))

  centimeter
  (km [u] (->kilometer   (* 1e-5 (.value u))))
  (m  [u] (->meter       (* 1e-2 (.value u))))
  (cm [u] u)
  (mm [u] (->millimeter  (* 1e1 (.value u))))
  (um [u] (->micrometer  (* 1e4 (.value u))))
  (mil [u] (->mils (* MILPCM (.value u))))
  (inch [u] (->inches (* INPCM (.value u))))
  (nearest [u eps] (->centimeter (* eps (Math/round (/ (.value u) eps)))))

  millimeter
  (km [u] (->kilometer   (* 1e-6 (.value u))))
  (m  [u] (->meter       (* 1e-3 (.value u))))
  (cm [u] (->centimeter  (* 1e-1 (.value u))))
  (mm [u] u)
  (um [u] (->micrometer  (* 1e3  (.value u))))
  (mil [u] (->mils (* MILPMM (.value u))))
  (inch [u] (->inches (* INPMM (.value u))))
  (nearest [u eps] (->millimeter (* eps (Math/round (/ (.value u) eps)))))

  micrometer
  (km [u] (->kilometer   (* 1e-9 (.value u))))
  (m  [u] (->meter       (* 1e-6 (.value u))))
  (cm [u] (->centimeter  (* 1e-4 (.value u))))
  (mm [u] (->millimeter  (* 1e-3 (.value u))))
  (um [u] u)
  (mil [u] (->mils (* MILPUM (.value u))))
  (inch [u] (->inches (* INPUM (.value u))))
  (nearest [u eps] (->micrometer (* eps (Math/round (/ (.value u) eps)))))
  
  inches
  (km [u] (->kilometer (* KMPIN (.value u))))
  (m [u] (->meter (* MPIN (.value u))))
  (cm [u] (->centimeter (* CMPIN (.value u))))
  (mm [u] (->millimeter (* MMPIN (.value u))))
  (um [u] (->micrometer (* UMPIN (.value u))))
  (mil [u] (->mils (* 1e3 (.value u))))
  (inch [u] u)
  (nearest [u eps] (->inches (* eps (Math/round (/ (.value u) eps)))))
  
  mils
  (km [u] (->kilometer (* KMPMIL (.value u))))
  (m [u] (->meter (* MPMIL (.value u))))
  (cm [u] (->centimeter (* CMPMIL (.value u))))
  (mm [u] (->millimeter (* MMPMIL (.value u))))
  (um [u] (->micrometer (* UMPMIL (.value u))))
  (mil [u] u)
  (inch [u] (->inches (* 1e-3 (.value u))))
  (nearest [u eps] (->mils (* eps (Math/round (/ (.value u) eps)))))
  
  java.lang.Long
  (km [u] (->kilometer u))
  (m  [u] (->meter u))
  (cm [u] (->centimeter u))
  (mm [u] (->millimeter u))
  (um [u] (->micrometer u))
  (mil [u] (->mils u))
  (inch [u] (->inches u))
  (nearest [u eps] (Long. (* eps (Math/round (/ (double u) eps)))))

  java.lang.Double
  (km [u] (->kilometer u))
  (m  [u] (->meter u))
  (cm [u] (->centimeter u))
  (mm [u] (->millimeter u))
  (um [u] (->micrometer u))
  (mil [u] (->mils u))
  (inch [u] (->inches u))
  (nearest [u eps] (* eps (Math/round (/ u eps))))

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

  )


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
   (let [reg-um #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*um"
         reg-mm #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*mm"
         reg-cm #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*cm"
         reg-m  #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*m"
         reg-km  #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*km"
         reg-mil  #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*(?:mil|mils)"
         reg-inch  #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)\s*(?:in|inch|inches)"
         reg-generic #"([-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)"

         ;;reg-um #"(.*)(um)"



         ]
     (condp re-matches str
       reg-um :>> #(um (Double/parseDouble (second %)))
       reg-mm :>> #(mm (Double/parseDouble (second %)))
       reg-cm :>> #(cm (Double/parseDouble (second %)))
       reg-m  :>> #(m (Double/parseDouble (second %)))
       reg-km :>> #(km (Double/parseDouble (second %)))
       reg-mil :>> #(mil (Double/parseDouble (second %)))
       reg-inch :>> #(inch (Double/parseDouble (second %)))
       reg-generic :>> #(hint-fn (Double/parseDouble (second %)))
       nil)))
  ([str]
   (distance str um)))


(defn test-inch
  "Tests conversion from inch to cm and back."
  [^long n]
  (loop [n n
         i (inch 1)]
    (if (= n 0)
      i
      (recur (dec n) (inch (cm i))))))











