(ns print-test
  (:require [clojure.pprint :as pp]))


(defrecord tr000 [val])
(defrecord tr001 [val])
(defrecord tr010 [val])
(defrecord tr011 [val])
(defrecord tr100 [val])
(defrecord tr101 [val])
(defrecord tr110 [val])
(defrecord tr111 [val])


;;(defmethod print-method       tr000 [obj writer]  (.write writer "tr000 print-method"))
;;(defmethod print-dup          tr000 [obj writer]  (.write writer "tr000 print-dup"))
;;(defmethod pp/simple-dispatch tr000 [obj]         (.write *out* "tr000 simple-dispatch"))

;;(defmethod print-method       tr001 [obj writer]  (.write writer "tr001 print-method"))
;;(defmethod print-dup          tr001 [obj writer]  (.write writer "tr001 print-dup"))
(defmethod pp/simple-dispatch tr001 [obj]         (.write *out* "tr001 simple-dispatch"))

;;(defmethod print-method       tr010 [obj writer]  (.write writer "tr010 print-method"))
(defmethod print-dup          tr010 [obj writer]  (.write writer "tr010 print-dup"))
;;(defmethod pp/simple-dispatch tr010 [obj]         (.write *out* "tr010 simple-dispatch"))

;;(defmethod print-method       tr011 [obj writer]  (.write writer "tr011 print-method"))
(defmethod print-dup          tr011 [obj writer]  (.write writer "tr011 print-dup"))
(defmethod pp/simple-dispatch tr011 [obj]         (.write *out* "tr011 simple-dispatch"))

(defmethod print-method       tr100 [obj writer]  (.write writer "tr100 print-method"))
;;(defmethod print-dup          tr100 [obj writer]  (.write writer "tr100 print-dup"))
;;(defmethod pp/simple-dispatch tr100 [obj]         (.write *out* "tr100 simple-dispatch"))

(defmethod print-method       tr101 [obj writer]  (.write writer "tr101 print-method"))
;;(defmethod print-dup          tr101 [obj writer]  (.write writer "tr101 print-dup"))
(defmethod pp/simple-dispatch tr101 [obj]         (.write *out* "tr101 simple-dispatch"))

(defmethod print-method       tr110 [obj writer]  (.write writer "tr110 print-method"))
(defmethod print-dup          tr110 [obj writer]  (.write writer "tr110 print-dup"))
;;(defmethod pp/simple-dispatch tr110 [obj]         (.write *out* "tr110 simple-dispatch"))

(defmethod print-method       tr111 [obj writer]  (.write writer "tr111 print-method"))
(defmethod print-dup          tr111 [obj writer]  (.write writer "tr111 print-dup"))
(defmethod pp/simple-dispatch tr111 [obj]         (.write *out* "tr111 simple-dispatch"))


(def t000 (->tr000 10))
(def t001 (->tr001 20))
(def t010 (->tr010 30))
(def t011 (->tr011 40))
(def t100 (->tr100 50))
(def t101 (->tr101 60))
(def t110 (->tr110 70))
(def t111 (->tr111 80))
(def recs [t000 t001 t010 t011 t100 t101 t110 t111])


(def dt (java.time.LocalTime/now))
(defmethod print-dup java.time.LocalTime [obj writer] (.write writer "datetime_dup"))

(println "(pr ...) outputs the following")
(doseq [rec recs]
  (doseq [prppd [nil #(.write *out* "pprint-dispatch with" %)]]
    (binding [pp/*print-pprint-dispatch*
              (if prppd #(.write *out* (str "pprint-dipatch-with" %))
                  pp/*print-pprint-dispatch*)]
      (doseq [pd [false true]]
        (binding [*print-dup* pd]
          (let [ppdstr (format "%6s" (boolean pp/*print-pprint-dispatch*))
                dupstr (format "%6s" *print-dup*)
                outstr (pr-str rec)]
            (binding [*print-dup* false]
              (println ppdstr dupstr ":" outstr))))))))

(println "\n(pprint ...) outputs the following")
(doseq [rec recs]
  (doseq [prppd [false true]]
    (binding [pp/*print-pprint-dispatch*
              (if prppd #(.write *out* (str "pprint-dipatch-with" %))
                  pp/*print-pprint-dispatch*)]
      (doseq [pd [false true]]
        (binding [*print-dup* pd]
          (let [outstr
                (with-out-str
                  (pp/pprint {:ppdstr prppd
                              :dupstr pd
                              :dt dt
                              :strobj rec}))]
            (binding [*print-dup* false]
              (println outstr "\n"))))))))




















