(ns toydb.core-test
  (:require [clojure.test :refer :all]
            [toydb.core :refer :all]
            [jfxutils.core :refer [printexp]]))

;; Ways to test color:
;; Javafx color <-> toydb.color <-> string <-> file

(deftest make-toydbcol
  (testing "Creation of toydb color via various methods"
    (is (= (toydb.edn.color/color "rgba(127,127,127,0.5)")
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x7f7f7f80", :rgba [0.49803921580314636 0.49803921580314636 0.49803921580314636 0.5]})))
    (is (= (toydb.edn.color/color 'RED) 
           (toydb.edn.color/map->Color {:name "RED", :hexstring "0xff0000ff", :rgba [1.0 0.0 0.0 1.0]})))
    (is (= (toydb.edn.color/color 305419896)
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x12345678", :rgba [0.07058823853731155 0.20392157137393951 0.33725491166114807 0.47058823704719543]})))
    (is (= (toydb.edn.color/color '(GREEN))
           (toydb.edn.color/map->Color {:name "GREEN", :hexstring "0x008000ff", :rgba [0.0 0.501960813999176 0.0 1.0]})))
    (is (= (toydb.edn.color/color [10 20 30])
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x0a141eff", :rgba [0.03921568766236305 0.0784313753247261 0.11764705926179886 1.0]})))
    (is (= (toydb.edn.color/color [10 20 30 0.4])
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x0a141e66", :rgba [0.03921568766236305 0.0784313753247261 0.11764705926179886 0.4000000059604645]})))
    (is (= (toydb.edn.color/color [0.1 0.2 0.3])
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x1a334dff", :rgba [0.10000000149011612 0.20000000298023224 0.30000001192092896 1.0]})))
    (is (= (toydb.edn.color/color [0.1 0.2 0.3 0.4])
           (toydb.edn.color/map->Color {:name nil, :hexstring "0x1a334d66", :rgba [0.10000000149011612 0.20000000298023224 0.30000001192092896 0.4000000059604645]})))))

(deftest jfxcol-to-toydbcol
  (testing "Conversion of JFX Color to toydb color and back"
    (dotimes [_ 1000]
      (let [jfxcol1 (toydb.edn.color/random-jfx-color)
            tdbcol (toydb.edn.color/color jfxcol1)
            jfxcol2 (toydb.edn.finalize/final tdbcol)]
        (is (toydb.edn.color/color-equal? jfxcol1 jfxcol2))))))

(deftest toydbcol-to-jfxcol
  (testing "Conversion of toydb color to JFX Color and back"
    (dotimes [_ 10000]
      (let [tdbcol1 (toydb.edn.color/random-toydb-color)
            jfxcol (toydb.edn.finalize/final tdbcol1)
            tdbcol2 (toydb.edn.color/color jfxcol)]
        (is (toydb.edn.color/color-equal? tdbcol1 tdbcol2))))))

(deftest toydbcol-to-string
  (testing "Conversion of toydb color to serializable string and back"
    (binding [*print-dup* true]
      (dotimes [_ 10000]
        (let [tdbcol1 (toydb.edn.color/random-toydb-color)
              tdbstr (with-out-str (print-dup tdbcol1 *out*))
              tdbcol2 (toydb.edn.reader/read-string tdbstr)]
          (is (toydb.edn.color/color-equal? tdbcol1 tdbcol2)))))))

(deftest toydbcol-seq-to-string
  (testing "Conversion of toydb color to serializable string and back"
    (binding [*print-dup* true]
      (doseq [tdbcol1 (toydb.edn.color/seq-toydb-color 0 0xfffff)]
        (let [tdbstr (with-out-str (print-dup tdbcol1 *out*))
              tdbcol2 (toydb.edn.reader/read-string tdbstr)]
          (is (toydb.edn.color/color-equal? tdbcol1 tdbcol2)))))))

(deftest toydbcol-to-file
  (testing "Conversion of toydb colors to file and back"
    (let [numtimes 10000
          tdbcols1 (take numtimes (repeatedly toydb.edn.color/random-toydb-color))]
      (binding [*print-dup* true
                *print-length* numtimes]
        (with-open [wr (clojure.java.io/writer "toydb-to-file.edn")]
          (clojure.pprint/pprint tdbcols1 wr)))
      (let [data (slurp "toydb-to-file.edn")
            tdbcols2 (toydb.edn.reader/read-string data)]
        (doseq [[left right] (map vector tdbcols1 tdbcols2)]
          (is (toydb.edn.color/color-equal? left right)))))))

(deftest file-to-toydbcol
  (testing "Conversion of various color entries in file to toydb colors"
    (with-open [wr (clojure.java.io/writer "file-to-toydbcol.edn")]
      (binding [*out* wr]
        (println "[")
        (println "#Color RED")
        (println "#Color \"RED\"")
        (println "#Color \"red\"")
        (println "#Color(RED)")
        (println "#Color 0x7f7f7f80")
        (println "#Color(0x7f7f7f80)")
        (println "#Color(0xa141eff)  ;; leading zero assumed...") 
        (println "#Color(0x0a141eff)")
        (println "#Color \"0x0a141eff\"   ;; works because leading zero is captured as string")
        (println "#Color (\"0x0a141eff\")")
        (println "#Color(0xf0f8ff)")
        (println "#Color 2139062144")
        (println "#Color(2139062144)")
        (println "#Color((2139062144))")
        ;;;(println "#Color(127,127,127)")
        (println "#Color(127,127,127,0.50)")
        (println "#Color \"hsl(270,50%,50%)\"")
        (println "#Color \"hsla(270,50%,50%,0.5)\"")
        (println "#Color \"rgb(10,20,30)\"")
        (println "#Color \"rgba(10,20,30,250)\"")
        (println "]")))
    (let [data (slurp "file-to-toydbcol.edn")
          tdbcols1 (toydb.edn.reader/read-string data)
          tdbcols2 [(toydb.edn.color/color 0xff0000ff)
                    (toydb.edn.color/color 0xff0000ff)
                    (toydb.edn.color/color 0xff0000ff)
                    (toydb.edn.color/color 0xff0000ff)
                    (toydb.edn.color/color 0x7f7f7f80)
                    (toydb.edn.color/color 0x7f7f7f80)
                    (toydb.edn.color/color "0x0a141eff")
                    (toydb.edn.color/color "0x0a141eff")
                    (toydb.edn.color/color "0x0a141eff")
                    (toydb.edn.color/color "0x0a141eff")
                    (toydb.edn.color/color 0xf0f8ff)
                    (toydb.edn.color/color 2139062144)
                    (toydb.edn.color/color 2139062144)
                    (toydb.edn.color/color 2139062144)
                    ;;(toydb.edn.color/color 0x7f7f7f)
                    (toydb.edn.color/color 0x7f7f7f80)
                    (toydb.edn.color/color 0x604080ff)
                    (toydb.edn.color/color 0x60408080)
                    (toydb.edn.color/color "0x0a141eff")
                    (toydb.edn.color/color "0x0a141eff")]]
      (doseq [[left right] (map vector tdbcols1 tdbcols2)]
        (is (toydb.edn.color/color-equal? left right))))))

