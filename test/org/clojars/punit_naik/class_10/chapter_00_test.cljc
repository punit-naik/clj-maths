(ns org.clojars.punit-naik.class-10.chapter-00-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.clojars.punit-naik.class-10.chapter-00 :as ch-00]))

(deftest absolute-test
  (is (= 0.5 (ch-00/absolute (- (/ 1 2))))))

(deftest cartesian-product-test
  (is (= [[1 3] [1 4] [2 3] [2 4]] (ch-00/cartesian-product [1 2] [3 4]))))

(deftest in-test
  (is (= true (ch-00/in [2 5] 2)))
  (is (= true (ch-00/in [2 5] 5)))
  (is (= false (ch-00/in [2 5] 1))))

(deftest not-in-test
  (is (= false (ch-00/not-in [2 5] 2)))
  (is (= false (ch-00/not-in [2 5] 5)))
  (is (= true (ch-00/not-in [2 5] 1))))

(deftest factors-test
  (is (= "Assert failed: (pos-int? num)"
         (try (ch-00/factors 0)
              (catch #?(:clj AssertionError :cljs js/Error)
                     e (.getMessage e)))))
  (is (= [1] (ch-00/factors 1)))
  (is (= [17] (ch-00/factors 17)))
  (is (= [2 4 8] (ch-00/factors 8))))

(deftest prime-number?-test
  (is (ch-00/prime-number? 1))
  (is (ch-00/prime-number? 17))
  (is (ch-00/prime-number? 2))
  (is (not (ch-00/prime-number? 4)))
  (is (not (ch-00/prime-number? 8))))

(deftest lowest-prime-factor-test
  (is (= 2 (ch-00/lowest-prime-factor 8)))
  (is (= 2 (ch-00/lowest-prime-factor 34)))
  (is (= 17 (ch-00/lowest-prime-factor 17)))
  (is (= 5 (ch-00/lowest-prime-factor 5))))

(deftest lcm-divide-test
  (is (= [2 5] (ch-00/lcm-divide [4 5] 2)))
  (is (= [1 3] (ch-00/lcm-divide [3 9] 3)))
  (is (= [5 5] (ch-00/lcm-divide [15 5] 3)))
  (is (= [10 4] (ch-00/lcm-divide [10 12] 3))))

(deftest least-common-multiple-test
  (testing "LCM By Division Method"
    (let [method :division]
      (is (= 45 (ch-00/least-common-multiple [15 9] method)))
      (is (= 15 (ch-00/least-common-multiple [3 15] method)))
      (is (= 45 (ch-00/least-common-multiple [45 5] method)))
      (is (= 20 (ch-00/least-common-multiple [4 5] method))))))