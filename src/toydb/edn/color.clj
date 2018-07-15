(ns toydb.edn.color
  (:require [jfxutils.core :as jfxc]
            [toydb.edn.finalize]))

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


(defn- parse-color-str [s]
  "Takes a string, a symbol, or a Long, and returns a string suitable
  for constructing javafx.scene.paint.Color.  Typically called by colarity"
  (condp instance? s
    java.lang.String s ;; captures quoted hex, eg "0x1234" and everything else
    clojure.lang.Symbol (str s) ;; just stringify the symbol, eg 'RED to "RED"
    java.lang.Long (format "#%08x" s)) ;; If reader finds raw hex, it'll return a Long
  

  ) 

(defn- colarity
  "Create a JFX color based on one through four arguments, typically
  called by finalize/final
   Arity-1 assumes a 'web color' 
   Arity-2 assumes a 'web color' and opacity 0.0 - 1.0 
   Arity-3 assumes RGB from  0.0 - 1.0 Arity-4 assumes RGB and opacity 0.0 - 1.0"
  ([a] (javafx.scene.paint.Color/web (parse-color-str a)))
  ([a b] (javafx.scene.paint.Color/web (parse-color-str a) b))
  ([a b c] (javafx.scene.paint.Color. a b c 1.0))
  ([a b c d] (javafx.scene.paint.Color. a b c d)))

(defrecord Color []
  toydb.edn.finalize/FinalizeProtocol
  (final [col]
    "Converts toydb.edn.color.Color to javafx.scene.paint.Color"
    (let [colval (:value col)]
      ;; if hex string, use literally.  if Long, convert to hex string.  If symbol, convert to string
      (if (sequential? colval)
        (apply colarity colval)
        (colarity colval)))))

(defn color
  "Create intermediate representation of Color, typically called by edn reader.
  arg must be a list or vector, etc."
  [arg]
  (if (instance? javafx.scene.paint.Color arg)
    (let [colstr (.toString arg)
          newstr (reverse-color-map colstr colstr)
          newsym (if (= newstr colstr) newstr (symbol newstr))]
      (map->Color {:value newsym}))
    (map->Color {:value arg})))


(defmethod print-method Color [^toydb.edn.color.Color c, ^java.io.Writer writer]
  (let [colvalstr (str (:value c))
        color-string (reverse-color-map colvalstr colvalstr)] ;; lookup by the hex part as key
    (.write writer (str "#"
                        (pr-str Color)
                        (pr-str (into {} c))))))

(defmethod print-dup Color [^toydb.edn.color.Color c, ^java.io.Writer writer]
  (let [colvalstr (str (:value c))
        color-string (reverse-color-map colvalstr colvalstr)] ;; lookup by the hex part as key
    (.write writer (format "#Color %s" color-string))))

(defmethod clojure.pprint/simple-dispatch Color [^toydb.edn.color.Color c]
  (.write *out* (pr-str c)))














