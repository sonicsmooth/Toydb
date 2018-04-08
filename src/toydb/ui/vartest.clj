(ns toydb.ui.vartest
  (:require [clojure.pprint :as pp]
            [clojure.repl :refer :all]
            ))

(def ^:dynamic *dvar*)



(defn varfn1 []
  (println "varfn1:" *dvar*)
  [*dvar*])

(defn varfn2 []
  (println "varfn2:" *dvar*)
  [*dvar*])












