(ns toydb.xxx
  (:import [javafx.geometry Insets VPos Point2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn snap-to-nearest
  "Rounds all elements of seq to closest integer multiqple of m, which
  can be non-integer."
  [seq ^double m ]
  (letfn [(f [^double x] (* m (Math/round (/ x m))))]
    (map f seq)))

(defn snap-pt-to-nearest [^Point2D p ^double m]
  (Point2D. (* m (Math/round (/ (.getY p) m)))
            (* m (Math/round (/ (.getX p) m)))))


(def x (snap-pt-to-nearest (Point2D. 1 2) 3.5))
(type x)

