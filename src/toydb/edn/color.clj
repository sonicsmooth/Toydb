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




(defrecord Color [name hexstring rgba]
  toydb.edn.finalize/FinalizeProtocol
  (final [col]
    "Converts toydb.edn.color.Color to javafx.scene.paint.Color"
    (javafx.scene.paint.Color/web (:hexstring col))))

(defn color
  "Create intermediate representation of Color.
  When called by edn reader, it receives the token/literal as an argument.
  Arg must be one of:
  * toydb.edn.color.Color object
  * JFX Color object
  * Color symbol (eg 'RED)
  * Hex string eg '0x123456ff' or 'red'
  * Seq of 3 or 4 doubles 0.0-1.0 presumed to be rgb or rgba.
  * Seq of 3 longs 0-255, or 3 longs 0-255 and a double 0.0-1.0, presumed to be rgb or rgba.
  This creates a JFXColor first, then extracts the various fields."
  [arg]
  (if 
      (instance? toydb.edn.color.Color arg) arg ;; identity, so just
      ;; return the arg Otherwise convert to temporary jfxcol
      ;; Generally these convert to string, then call Color.web(...)
      ;; Careful when arg is Long.  When converted to hex string prior
      ;; to being passed into Color.web, leading zeros are actually
      ;; relevant, eg "0x0a141eff" is different from "0xa141eff"; the
      ;; latter throws an error.  If you want to avoid ambiguity, then
      ;; use a string literal "0x0a141eff" rather than long literal
      ;; 0xa141eff or 0x0a141eff or 305419896 in your edn file and
      ;; when calling (color...)
      (let [jfxcol (cond (instance? java.lang.String arg) (javafx.scene.paint.Color/web arg)
                         (instance? clojure.lang.Symbol arg) (javafx.scene.paint.Color/web (name arg))
                         (instance? java.lang.Long arg) (javafx.scene.paint.Color/web (format "0x%x" arg))
                         (sequential? arg) (condp = (count arg)
                                             1 (color (first arg)) ;; okay so some recursion... messes things up a little down below
                                             2 (throw (java.lang.IllegalArgumentException. "Could not convert 2 args to a color"))
                                             3 (let [[r g b] arg]
                                                 (condp = (class r)
                                                   java.lang.Double (javafx.scene.paint.Color/color r g b)
                                                   java.lang.Long (javafx.scene.paint.Color/web (str "rgb(" (clojure.string/join "," arg) ")"))))
                                             4 (let [[r g b a] arg]
                                                 (condp = (class r)
                                                   java.lang.Double (javafx.scene.paint.Color/color r g b a)
                                                   java.lang.Long (javafx.scene.paint.Color/web (str "rgba(" (clojure.string/join "," arg) ")")))))
                         (instance? javafx.scene.paint.Color arg) arg
                         :default (throw (java.lang.IllegalArgumentException.
                                          (str "Could not convert " arg " to color"))))]
        (if (instance? toydb.edn.color.Color jfxcol)
          jfxcol ;; not really a jfxcol, but a tdbcol
          (let [newstr (reverse-color-map (.toString jfxcol))]
            (map->Color {:name (when newstr newstr)
                         :hexstring (.toString jfxcol)
                         :rgba [(.getRed jfxcol)
                                (.getGreen jfxcol)
                                (.getBlue jfxcol)
                                (.getOpacity jfxcol)]}))))))

(defn random-toydb-color []
  "Returns a random toydb color"
  (if (> (rand) 0.5)
    (let [lrnd #(long (rand 256))
          [r g b a] (repeatedly 4 lrnd)
          [rd gd bd ad] (map #(/ % 255.0) [r g b a])
          hexstring (format "0x%02x%02x%02x%02x" r g b a)
          name (reverse-color-map hexstring)]
      (map->Color {:name name :hexstring hexstring :rgba [rd gd bd ad]}))
    (let [[hexstring name] (nth (seq reverse-color-map) (rand-int (count reverse-color-map)))
          longval (Long/decode hexstring)
          r (bit-shift-right (bit-and longval 0xff000000) 24)
          g (bit-shift-right (bit-and longval 0x00ff0000) 16)
          b (bit-shift-right (bit-and longval 0x0000ff00)  8)
          a (bit-shift-right (bit-and longval 0x000000ff)  0)
          [rd gd bd ad] (map #(/ % 255.0) [r g b a])]
      (map->Color {:name name :hexstring hexstring :rgba [rd gd bd ad]}))))

(defn random-jfx-color
  "Returns a random JFX color"
  []
  (if (> (rand) 0.5)
    (javafx.scene.paint.Color/web (format "rgba(%d,%d,%d,%f)" (rand-int 256) (rand-int 256) (rand-int 256) (rand)))
    (let [[hexstring name] (nth (seq reverse-color-map) (rand-int (count reverse-color-map)))]
      (javafx.scene.paint.Color/web name))))

(defn color-equal?
  "Returns true if c1 and c2 names match, hex strings match, and rgba are close enough.
  c1 and c2 must both be a JFX color or toydb color"
  [c1 c2]
  (let [eps 0.02]
    (condp instance? c1
      javafx.scene.paint.Color (= (.toString c1) (.toString c2))
      toydb.edn.color.Color (and (= (:name c1) (:name c2))
                                 (= (:hexstring c1) (:hexstring c2))
                                 (< (Math/abs (- (nth (:rgba c1) 0) (nth (:rgba c2) 0))) eps)
                                 (< (Math/abs (- (nth (:rgba c1) 1) (nth (:rgba c2) 1))) eps)
                                 (< (Math/abs (- (nth (:rgba c1) 2) (nth (:rgba c2) 2))) eps)
                                 (< (Math/abs (- (nth (:rgba c1) 3) (nth (:rgba c2) 3))) eps)))))

(defmethod print-method Color [^toydb.edn.color.Color c, ^java.io.Writer writer]
  (.write writer (str "#"
                      (pr-str Color)
                      (pr-str (into {} c)))))

(defmethod print-dup Color [^toydb.edn.color.Color c, ^java.io.Writer writer]
  (if-let [colstr (:name c)] 
    (.write writer (format "#Color(%s)" colstr))
    (if (= (last (:rgba c)) 1.0)
      ;;(.write writer (format "#Color(%s) " (clojure.string/join "," (map pr-str (take 3 (:rgba c))))))
      ;;(.write writer (format "#Color(%s) " (clojure.string/join "," (map pr-str (take 3 (:rgba c))))))
      (.write writer (apply format "#Color(%.3f,%.3f,%.3f)" (take 3 (:rgba c))))
      (.write writer (apply format "#Color(%.3f,%.3f,%.3f,%.3f)" (:rgba c))))))

(defmethod clojure.pprint/simple-dispatch Color [^toydb.edn.color.Color c]
  (.write *out* (pr-str c)))














