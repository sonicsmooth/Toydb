(ns toydb.edn.shapes
  (:require [jfxutils.core :as jfxc]
            toydb.units
            toydb.edn.finalize))



;; All these shapes use toydb.units/*unitfn* to resolve final units In
;; general, we loop through the argument, which is a map, and set the
;; property referenced by the key to the value

(defrecord Circle []
  toydb.edn.finalize/FinalizeProtocol
  (final [inter-circ] ;; arg is intermediate circle
    (let [circ (javafx.scene.shape.Circle.)]
      (doseq [[k v] inter-circ]
        (jfxc/set-prop-val-from-symbol! circ k (toydb.edn.finalize/final v)))
      circ)))

(defmethod print-method Circle [^toydb.edn.shapes.Circle c, ^java.io.Writer writer]
  (.write writer (str "#"
                      (pr-str Circle)
                      (pr-str (into {} c)))))

(defmethod print-dup Circle [^toydb.edn.shapes.Circle c, ^java.io.Writer writer]
  (.write writer "#Circle")
  (print-method (into {} c) writer))

(defmethod clojure.pprint/simple-dispatch Circle [^toydb.edn.shapes.Circle c]
  (let [local-dispatch (jfxc/make-simple-dispatch Circle "#Circle")]
    (local-dispatch c)))


(defrecord Rectangle []
  toydb.edn.finalize/FinalizeProtocol
  (final [inter-rect]
    (let [rect (javafx.scene.shape.Rectangle.)]
      (doseq [[k v] inter-rect]
        (jfxc/set-prop-val-from-symbol! rect k (toydb.edn.finalize/final v)))
      rect)))

(defmethod print-method Rectangle [^toydb.edn.shapes.Rectangle r, ^java.io.Writer writer]
  (.write writer (str "#"
                      (pr-str Rectangle)
                      (pr-str (into {} r)))))

(defmethod print-dup Circle [^toydb.edn.shapes.Rectangle r, ^java.io.Writer writer]
  (.write writer "#Rectangle")
  (print-method (into {} r) writer))

(defmethod clojure.pprint/simple-dispatch Rectangle [^toydb.edn.shapes.Rectangle r]
  (let [local-dispatch (jfxc/make-simple-dispatch Rectangle "#Rectangle")]
    (local-dispatch r)))











