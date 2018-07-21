(ns toydb.core-test
  (:require [clojure.test :refer :all]
            [toydb.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


;; Ways to test color:
;; Javafx color <-> toydb.color <-> string <-> file


(deftest jfxcol-to-toydbcol
  (testing "JFX Color to toydb color"
    (let [jfxcol1 (javafx.scene.paint.Color/RED)
          jfxcol2 (javafx.scene.paint.Color/web "0x123456ff")
          tdbcol1 (toydb.color/color jfxcol1)]
      )))



#_(defn test-ns-hook []
  (println "in hook")
  (a-test)
  (b-test))
