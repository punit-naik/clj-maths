(ns org.clojars.punit-naik.class-10.chapter-03-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.class-10.chapter-03 :as ch-03]))

(deftest solution-test
  (is (= [2 1.0] (ch-03/solution [1 -2 0] 2))))

(deftest solutions-count-test
  (is (= 1 (ch-03/solutions-count [1 -2 0] [3 4 -20])))
  (is (= (ch-03/infinity) (ch-03/solutions-count [2 3 -9] [4 6 -18])))
  (is (zero? (ch-03/solutions-count [1 2 -4] [2 4 -12]))))

(deftest solve-eq-pair
  (is (= [4.0 2.0] (ch-03/solve-eq-pair [1 -2 0] [3 4 -20])))
  (is (= "Equations 2x+3x-9=0 and 4x+6x-18=0 are coincident" (ch-03/solve-eq-pair [2 3 -9] [4 6 -18])))
  (is (= "Equations 1x+2x-4=0 and 2x+4x-12=0 are parallel" (ch-03/solve-eq-pair [1 2 -4] [2 4 -12]))))