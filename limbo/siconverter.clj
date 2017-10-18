(ns toydb.siconverter
  (:require [toydb.testvals :as test])
  (:use [clojure.pprint]))


;;; The general policy for conversions is you get nil when trying to
;;; convert an empty string to a number, and an exception if the
;;; string you're trying to convert doesn't match what you're trying
;;; to convert to.  In the very general case of string-to-num, it'll
;;; pretty much convert anything to the closest native type.


(defn string-to-num
  "Parses string and returns either Long or Double.  Accepts
  engineering notation also. Empty string returns nil.  Errors throw exception."
  [s]
  (if (empty? s) nil
      (let [match (zipmap [:fullmatch :sci :base :integer :dotdec :decdotdec :pow :full-suffix :si-suffix :remain-suffix ]
                          (re-matches #"([-+]?((\d+)|(\.\d+)|(\d+\.?\d*))([Ee][+-]?\d+)?)(([yzafpnumkMGTPEZY])([a-zA-Z]*))?"
                                      (clojure.string/trim s)))
            suffix-multiplier (zipmap (map str (vec "yzafpnum kMGTPEZY"))
                                      (map #(Math/pow 10 %) (range -24 25 3)))]
        (cond (:si-suffix match) (* (Double/parseDouble (:sci match)) (suffix-multiplier (:si-suffix match) ))
              (or (:dotdec match) (:decdotdec match) (:pow match)) (Double/parseDouble (:sci match))
              :else (Long/parseLong (:fullmatch match))))))

(defn test-match-float
"Loads test strings and values, and tests parser to convert strings to
  Doubles.  Should move this to the test suite."
  []
  (let [strs test/test-strings
        vals test/test-vals
        results (map (fn [actual-val s]
                       (let [converted-val (string-to-num s)
                             passfail (= actual-val converted-val)]
                         {:passfail passfail
                          :output (str "\"" s "\"->" converted-val " " actual-val " "(= actual-val converted-val))}))
                     vals strs)]
    (doseq [{pf :passfail out :output} results] ;; destructure
      (when (not pf) (println "Failed match:" out))))) ;; only print the failures

(defn calc-mantissa-exp
  "Given a number n, returns equivalent [mantissa exponent] for
  scientific notation, where exponent is a multiple of three."
  [n]
  (if (= 0 n)
    [0.0 0] ;; otherwise we get -Infinity when n is 0
    (let [exponent (Math/floor (Math/log10 n))
          exp3div (* 3 (int (Math/floor (/ exponent 3.0))))
          exp3mod (mod (int exponent) 3)
          mantissa (* n (Math/pow 10 (+ exp3mod (- exponent))))]
      [mantissa exp3div])))

(defn num-to-engstr
  "Given a number n, returns a string in engineering notation, ie
  scientific notation in which the exponent is a multiple of three,
  and the fractional part (mantissa) is >= 1.  For example if
  n=0.0001, returns \"100e-6\" "
  [n]
  (let [[mantissa exp] (calc-mantissa-exp n)]
    (if (= exp 0)
      (str mantissa)
      (str mantissa "e" exp))))

(defn num-to-sistr
  "Given a number n, returns a string with SI suffix.  For example if
  n = 0.0001, returns \"100u\" "
  [n]
  (let [[mantissa exp] (calc-mantissa-exp n)
        suffix (zipmap(range -24 25 3)
                      (map #(.trim (str %)) (vec "yzafpnum kMGTPEZY")))]
    (if (= exp 0)
      (str mantissa)
      (str mantissa (suffix exp)))))


(defprotocol IConverter  
  (to-boolean [this])   ;; returns Boolean for anything 
  (to-double  [this])   ;; returns Double for any number string
  (to-long    [this])   ;; returns Long for any number string
  (to-normstr [this])   ;; returns standard string representation of number or string
  (to-engstr  [this])   ;; returns string with exponent power of three
  (to-sistr   [this])   ;; returns string with y,z,a,f,p,n,u,m,k,M,G,T,P,E,Z,Y etc.
  (to-kw      [this])   ;) ;; handles strings-to-keywords
  )


;; Okay to convert from nil to something else
(extend-protocol IConverter
  nil
  (to-double  [n] nil)
  (to-long    [n] nil)
  (to-normstr [n] "nil")
  (to-engstr  [n] "nil")
  (to-sistr   [n] "nil")
  (to-kw      [n] nil)
  Object ;; used for default implementation
  (to-boolean [o] (boolean o))
  (to-double  [o] (double o))
  (to-long    [o] (long o))
  (to-normstr [o] (str o))
  (to-engstr  [o] (throw (IllegalArgumentException. (str "Cannot cast " (type o) " as engineering string"))))
  (to-sistr   [o] (throw (IllegalArgumentException. (str "Cannot cast " (type o) " as si string"))))
  (to-kw      [o] (keyword o))
  String ;; Philosophical question: does "" equal nil, or throw exception?
  (to-double  [s] (if (empty? s) nil  (double (string-to-num s)))) ;; string-to-num returns nil on empty string, but (double ...) throws exception on nil
  (to-long    [s] (if (empty? s) nil (long (string-to-num s)))) ;; Nil means no data, truncate floats
  (to-normstr [s] s)
  (to-engstr  [s] (num-to-engstr (string-to-num s)))
  (to-sistr   [s] (num-to-sistr (string-to-num s)))
  (to-kw      [s] (keyword s))
  Long
  (to-double  [el] (double el))
  (to-long    [el] el)
  (to-normstr [el] (str el))
  (to-engstr  [el] (num-to-engstr el))
  (to-sistr   [el] (num-to-sistr el))
  (to-kw      [el] nil)
  Double
  (to-double  [dub] dub)
  (to-long    [dub] (long dub))
  (to-normstr [dub] (str dub))
  (to-engstr  [dub] (num-to-engstr dub))
  (to-sistr   [dub] (num-to-sistr dub))
  (to-kw      [dub] nil)
  clojure.lang.Keyword
  (to-double  [kw] nil)
  (to-long    [kw] nil)
  (to-normstr [kw] (name kw))
  (to-engstr  [kw] nil)
  (to-sistr   [kw] nil)
  (to-kw      [kw] kw))


(defn convert-to
  "Converts value to totype if possible.  If totype is nil, will
  convert to number, and failing that, a string.  If string fails an
  exception is raised.  This is typically used when the existing cell
  type is nil, and a new value is passed in, that is, trying to
  convert some string to nil."
  [totype value]
  (if (nil? totype)
    (try (string-to-num value) ;; Can we make this a number?
         (catch Exception e ;; nope, just make it a string, if possible
           (str value)))    ;; any exceptions here are bounced up

    ;; Otherwise, convert like normal
    (let [funcmap {Boolean to-boolean
                   Long to-long
                   Double to-double
                   String to-normstr
                   clojure.lang.Keyword to-kw
                   :boolean to-boolean
                   :long to-long
                   :double to-double
                   :normstr to-normstr
                   :engstr to-engstr
                   :sistr to-sistr
                   :kw to-kw}
          func (funcmap totype)]
      (func value)))) 














