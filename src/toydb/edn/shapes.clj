(ns toydb.edn.shapes
  (:require [jfxutils.core :as jfxc]
            toydb.units
            toydb.edn.finalize))

(set! *warn-on-reflection* false)

;; All these shapes use toydb.units/*unitfn* to resolve final units In
;; general, we loop through the argument, which is a map, and set the
;; property referenced by the key to the value

#_(defrecord Circle []
  toydb.edn.finalize/FinalizeProtocol
  (final [inter-circ] ;; arg is intermediate circle
    (let [circ (javafx.scene.shape.Circle.)]
      (doseq [[k v] inter-circ]
        (jfxc/set-prop-val-from-symbol! circ k (toydb.edn.finalize/final v)))
      circ)))

#_(defmethod print-method Circle [^toydb.edn.shapes.Circle c, ^java.io.Writer writer]
  (.write writer (str "#"
                      (pr-str Circle)
                      (pr-str (into {} c)))))

#_(defmethod print-dup Circle [^toydb.edn.shapes.Circle c, ^java.io.Writer writer]
  (.write writer "#Circle")
  (print-method (into {} c) writer))

#_(defmethod clojure.pprint/simple-dispatch Circle [^toydb.edn.shapes.Circle c]
  (let [local-dispatch (jfxc/make-simple-dispatch Circle "#Circle")]
    (local-dispatch c)))


#_(defrecord Rectangle []
  toydb.edn.finalize/FinalizeProtocol
  (final [inter-rect]
    (let [rect (javafx.scene.shape.Rectangle.)]
      (doseq [[k v] inter-rect]
        (jfxc/set-prop-val-from-symbol! rect k (toydb.edn.finalize/final v)))
      rect)))

#_(defmethod print-method Rectangle [^toydb.edn.shapes.Rectangle r, ^java.io.Writer writer]
  (.write writer (str "#"
                      (pr-str Rectangle)
                      (pr-str (into {} r)))))

#_(defmethod print-dup Circle [^toydb.edn.shapes.Rectangle r, ^java.io.Writer writer]
  (.write writer "#Rectangle")
  (print-method (into {} r) writer))

#_(defmethod clojure.pprint/simple-dispatch Rectangle [^toydb.edn.shapes.Rectangle r]
  (let [local-dispatch (jfxc/make-simple-dispatch Rectangle "#Rectangle")]
    (local-dispatch r)))

(defmacro defshape [fullfxclass & [fields]]
  (let [fullfxclassstr (name fullfxclass)
        fxclasssegs (clojure.string/split fullfxclassstr #"\.")
        fxclassstr (last fxclasssegs)
        fxclass (symbol fxclassstr)
        tdbrec (symbol (str (name (ns-name *ns*)) "." fxclassstr))
        sharg (gensym)
        wrarg (gensym)
        hashclass (str "#" fxclassstr)]
    `(do
       (defrecord ~fxclass [~@fields]
         toydb.edn.finalize/FinalizeProtocol
         (final [~'inter-shape]
           (let [~'shape (new ~fullfxclass)]
             (doseq [[~'k ~'v] ~'inter-shape]
               (jfxutils.core/set-prop-val-from-symbol! ~'shape ~'k (toydb.edn.finalize/final ~'v)))
             ~'shape)))

       (defmethod print-method ~tdbrec [ ~sharg ~(with-meta wrarg {:tag java.io.Writer})]
         (.write ~wrarg (str "#"
                               (pr-str ~fxclass)
                               (pr-str (into {} ~sharg)))))
       
       (defmethod print-dup ~tdbrec [ ~sharg ~(with-meta wrarg {:tag java.io.Writer})]
         (.write ~wrarg ~hashclass)
         (print-method (into {} ~sharg) ~wrarg))

       (defmethod clojure.pprint/simple-dispatch ~tdbrec [~sharg]
         (let [~'local-dispatch (jfxc/make-simple-dispatch ~fxclass ~hashclass)]
           (~'local-dispatch ~sharg))))
    
    ))

(defshape javafx.scene.shape.Circle)
(defshape javafx.scene.shape.Rectangle)


    




















