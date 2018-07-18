(ns toydb.core-test
  (:require [clojure.test :refer :all]
            [toydb.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest b-test
  (testing "What!!"
    (is (= 2 (+ 1 1)))))

(deftest c-test
  (testing "What!!"
    (is (= 3 (+ 1 1)))))

(deftest create-color
  (testing "Creating color"
    (let )))



#_(defn test-ns-hook []
  (println "in hook")
  (a-test)
  (b-test))
