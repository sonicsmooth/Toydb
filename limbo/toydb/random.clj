;; Tests O(n) properties of java's arraylist's indexOf method.
;; Overall it's pretyt fast.  Average lookup time of 3ms for a 100,000 size array
;; of random-sized clojure maps.

(ns toydb.random
  (:require [clojure.edn :as edn])
  (:require [clojure.reflect :as r])
  (:require [clojure.test :only [function?]])
  (:require [clojure.java.io :only [writer]])
  (:use [clojure repl pprint ]))


(defn random-keyword []
  (let [ascii-nums (range (int \0) (inc (int \9)))
        ascii-uppers (range (int \A) (inc (int \Z)))
        ascii-lowers (range (int \a) (inc (int \z)))
        ascii-available (concat ascii-nums ascii-uppers ascii-lowers)
        strlen (+ 3 (rand-int 10))
        randints (for [_ (range strlen)] (char (rand-nth ascii-available)))]
    (keyword (apply str randints))))

(defn make-random-map  []
  (let [numkeys (+ 3 (rand-int 5))
        randkeys (for [_ (range numkeys)] (random-keyword))
        randvals (for [_ (range numkeys)] (rand))]
    (zipmap randkeys randvals)))


(defn runtest
  "Does iterations lookups into given list of maps.
  Each iteration consists of choosing a random map in the vector, then
  finding its index."
  [maps iterations]
  (let [starttime (System/nanoTime)]
    (dotimes [_ iterations]
      (let [random-index (rand-int (count maps))
            random-map (nth maps random-index)]
        (.indexOf maps random-map)))
    (* (- (System/nanoTime) starttime) 1e-9)))

(defn myprofile [num-maps]
  (let [maps (vec (repeatedly num-maps make-random-map))
        iterations 1000
        total-time-s (runtest maps iterations)
        avg-time-s (/ total-time-s iterations)]
    (println (str "Num-maps: " num-maps ", Average time per iteration:") avg-time-s)
    avg-time-s))

(defn logspace
  "Returns num log-spaced values from start to stop, inclusive"
  [start stop num]
  (let [k (/ (Math/log10 (/ stop start)) (dec num))
        i (range num)
        f #(* start (Math/pow 10 (* k %)))]
    (map f i)))

(defn longlogspace
  "Returns logspaced Long values, each exactly once"
  [start stop num]
  (distinct (map long (logspace start stop num))))

(defn big-profile []
  (let [num-mapses (longlogspace 10 100000 50)
        times (vec (map myprofile num-mapses))]
    (println num-mapses)
    [num-mapses times]))

(defn writedata [fname [xx yy]]
  (with-open [file (clojure.java.io/writer fname)]
    (doseq [[x y] (map vector xx yy)]
      (let [s (str x ", " y "\n") ]
        (print s)
        (.write file s )))))

(defn run []
  (def profiledata (big-profile))
  (writedata "c:\\junk.csv" profiledata))


















