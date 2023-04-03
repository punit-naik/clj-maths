(ns org.clojars.punit-naik.class-10.chapter-05-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.class-10.chapter-05 :as ch-05]))

(deftest arithmetic-progression-test
  (is (= 1 (ch-05/arithmetic-progression [1 2 3 4 5])))
  (is (= 2 (ch-05/arithmetic-progression [1 3 5 7 9 11])))
  (is (= -2 (ch-05/arithmetic-progression [5 3 1 -1 -3 -5 -7])))
  (is (nil? (ch-05/arithmetic-progression [1 2 3 4 6]))))

(deftest nth-arithmetic-progression-test
  (is (= 10000 (ch-05/nth-arithmetic-progression 8000 500 5))))

(deftest find-n-test
  (is (= 30.0 (ch-05/find-n [12 15 18] 99))))

(deftest exists-in-arithmetic-progression?-test
  (is (false? (ch-05/exists-in-arithmetic-progression? [5 11 17 23] 301))))

(deftest sum-n-test
  (is (= 15.0 (ch-05/sum-n [1 2] 5))))

(deftest generate-arithmetic-progression-test
  (is (= [3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0]
         (ch-05/generate-arithmetic-progression [3 5] [7 9]))))