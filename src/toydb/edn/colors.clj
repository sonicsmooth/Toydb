(ns toydb.edn.colors)

;; Set up reverse color lookup
(def color-names
  (->> javafx.scene.paint.Color
       clojure.reflect/reflect
       :members
       (filter #(= #{:public :static :final} (:flags %)))
       (map :name) ;; returns symbols
       (map str)))

(def color-hexstrings
  (->> color-names
       (map #(javafx.scene.paint.Color/valueOf %)) ;; returns color objects
       (map str))) ;; returns 0x11223344 color string

;; keys are hexstrings, vals are color names
(def reverse-color-map (apply hash-map (interleave color-hexstrings color-names)))

(defn parse-color-arg [arg]
  (condp instance? arg
    clojure.lang.Symbol (str arg) ;; just stringify the symbol, eg 'RED to "RED"
    java.lang.String arg
    java.lang.Long (format "#%08x" arg)))


(defn color
  "Create color object from a variety of args:
  1. (color ['RED']) or (color ['0x80334dff'])
  2. error
  3. (color [0.5 0.2 0.3])
  4. (color [0.5 0.2 0.3 0.9])"
  [args]
  (condp = (count args)
    ;; if hex string, use literally.  if Long, convert to hex string.  If symbol, convert to string
    1 (javafx.scene.paint.Color/web  (parse-color-arg (first args)))
    2 (throw (IllegalArgumentException. "Not enough arguments to Color; requires 1, 3, or 4"))
    3 (apply #(javafx.scene.paint.Color. %1 %2 %3 1.0) args)
    4 (apply #(javafx.scene.paint.Color. %1 %2 %3 %4) args)
    (throw (IllegalArgumentException. "Too many arguments to Color; requires 1, 3, or 4"))))










