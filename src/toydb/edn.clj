(ns toydb.edn
  ;;  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure.pprint])
  (:use [clojure.repl :exclude [root-cause]])
  (:require [clojure.java.io])
  (:require [clojure.edn :as edn]))

;; Maybe want https://github.com/miner/tagged

(defrecord SchGroup [name children])
(defrecord SchLine [x0 y0 x1 y1])
(defrecord SchCircle [x y r])
(defrecord SchText [x y text])

;; From http://www.compoundtheory.com/clojure-edn-walkthrough/
(comment (def edn-readers {'toydb.edn.SchGroup map->SchGroup
                           'toydb.edn.SchLine map->SchLine
                           'toydb.edn.SchCircle map->SchCircle}))
(def edn-readers {'SchGroup map->SchGroup
                  'SchLine map->SchLine
                  'SchCircle map->SchCircle
                  'SchText map->SchText})
(defn default-reader [tag value]
  (println "Unrecognized:" tag "=" value))

;; From http://stackoverflow.com/questions/15234880/how-to-use-clojure-edn-read-to-get-a-sequence-of-objects-in-a-file
(defn edn-seq
  "Returns the objects from stream as a lazy sequence."
  ([]            (edn-seq *in*))
  ([stream]      (edn-seq {} stream))
  ([opts stream] (lazy-seq (cons (clojure.edn/read opts stream) (edn-seq opts stream)))))

(defn swallow-eof
  "Ignore an EOF exception raised when consuming seq."
  [seq]
  (lazy-seq (try
              (cons (first seq) (swallow-eof (rest seq)))
              (catch java.lang.RuntimeException e
                (when-not (= (.getMessage e) "EOF while reading")
                  (throw e))))))

(defn read-edn-file [filename]
  (with-open [stream (java.io.PushbackReader. (clojure.java.io/reader filename))]
    (vec (swallow-eof (edn-seq
                       {:readers edn-readers :default default-reader}
                       stream)))))
