(ns org.clojars.punit-naik.class-10.chapter-02-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.class-10.chapter-02 :as ch-02]))

(deftest solve-eq-test
  (is (= 5 (ch-02/solve-eq [1 -5])))
  (is (= [2.0 -2.0] (ch-02/solve-eq [1 0 -4]))))

(deftest relationship-between-zeros-and-coefficients-test
  (is (true? (ch-02/relationship-between-zeros-and-coefficients [1 0 -4] [2 -2])))
  (is (true? (ch-02/relationship-between-zeros-and-coefficients [1 0 -2] [1.414 -1.414])))
  (is (true? (ch-02/relationship-between-zeros-and-coefficients [2 -5 -14 8] [4 -2 0.5]))))

(deftest divide-polynomial
  (is (= {:quotient [2.0 -1.0] :remainder [3.0]} (ch-02/divide-polynomial [2 3 1] [1 2])))
  (is (= {:quotient [3.0 -5.0] :remainder [9.0 10.0]} (ch-02/divide-polynomial [3 1 2 5] [1 2 1]))))