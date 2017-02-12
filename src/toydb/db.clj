(ns toydb.db
  (:require [toydb.siconverter :as si])
  (:require [clojure.test])
  (:use [clojure.stacktrace]))


(defn mutable?
  "Returns true if x is atom, ref, var, or agent; false otherwise"
  [x]
  (or (instance? clojure.lang.Ref x)
      (instance? clojure.lang.Var x)
      (instance? clojure.lang.Atom x)
      (instance? clojure.lang.Agent x)))

(defprotocol DatabaseProtocol
  "db is atom, access is vector of keywords and indices, newvalf is
  either a function to be applied to the element accessed by get-in,
  or a constant value to be assigned to that element.  So you can't
  assign a function directly to an element, even if you wanted to, for
  some reason."
  (poke-db! [db])
  (write-db! [db accesspath newvalf])
  (read-db [db accesspath]))

(extend-protocol DatabaseProtocol
  clojure.lang.Atom
  (poke-db! [db] (swap! db identity))
  (write-db! [db accesspath newvalf]
    (let [oldval (get-in @db accesspath)
          newval (if (clojure.test/function? newvalf)
                   (newvalf oldval) newvalf)]
      (reset! db (assoc-in @db accesspath newval))
      nil))
  (read-db [db accesspath]
    (get-in @db accesspath))

  clojure.lang.Ref
  (poke-db! [db] (dosync (alter db identity)))
  (write-db! [db accesspath newvalf]
    (let [oldval (get-in @db accesspath)
          newval (if (clojure.test/function? newvalf)
                   (newvalf oldval) newvalf)]
      (dosync (ref-set db (assoc-in @db accesspath newval)))
      nil))
  (read-db [db accesspath]
    (get-in @db accesspath))

  clojure.lang.PersistentArrayMap
  (read-db [db accesspath]
    (get-in db accesspath)))

(defn greedy-commit!
  "Update the database with value, converting to correct type if
  necessary.  When extra-function, presumably with side-effects, is
  given, this is run with no arguments after updating db."
  [newval var full-accesspath & [extra-fn]]
  (let [existing-var-item (read-db var full-accesspath)
        existing-var-item-type (type existing-var-item)]
    (if (instance? existing-var-item-type newval) ;; No need to convert if types already match
      (write-db! var full-accesspath newval)
      (try ;; Convert from property type eg String to the underlying model type
        (let [converted-value (si/convert-to existing-var-item-type newval)]
          (if (and (nil? converted-value)
                   (or (= Long existing-var-item-type)
                       (= Double existing-var-item-type))) ;; won't accept empty strings for numerical types
            (throw (Exception. "Can't enter empty string")))
          (write-db! var full-accesspath converted-value)
          (when extra-fn (extra-fn)))
        (catch Exception e ;; when we can't convert
          (println "Cannot convert" newval (str  "(" (type newval) ")") "to" existing-var-item-type)
          (print-cause-trace e))))))

(defn -main [& args]
  (println "I'm a library.  Don't run me."))



