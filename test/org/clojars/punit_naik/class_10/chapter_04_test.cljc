(ns org.clojars.punit-naik.class-10.chapter-04-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.class-10.chapter-04 :as ch-04]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(deftest root-combo?-test
  (is (ch-04/root-combo? [1.41 1.41]))
  (is (not (ch-04/root-combo? [1.41 1]))))

(deftest middle-term-splittable?-test
  (is (not (ch-04/middle-term-splittable? [6 -1 -2] [2 -3])))
  (is (ch-04/middle-term-splittable? [6 -1 -2] [3 -4]))
  (is (ch-04/middle-term-splittable? [3 (- (* 2 (Math/sqrt 6))) 2]
                                     [(* -1 (Math/sqrt 6)) (* -1 (Math/sqrt 6))])))

(deftest adjust-combo-sign-test
  (is (= [[2 -3] [-2 -3]] (ch-04/adjust-combo-sign -5 [2 3]))))

(deftest split-middle-term-test
  (is (= [-2 -3] (ch-04/split-middle-term [2 -5 3])))
  (is (= [3 -4] (ch-04/split-middle-term [6 -1 -2])))
  (is (= [-2.449489742783178 -2.449489742783178] (ch-04/split-middle-term [3 (- (* 2 (Math/sqrt 6))) 2]))))

(deftest same-roots?-test
  (is (ch-04/same-roots? [0.8164965809277261 0.8164965809277259]))
  (is (not (ch-04/same-roots? [0.8164965809277261 1]))))

(deftest adjust-roots-test
  (is (= [0.8164965809277261 0.8164965809277261] (ch-04/adjust-roots [0.8164965809277261 0.8164965809277259])))
  (is (= [1 2] (ch-04/adjust-roots [1 2]))))

(deftest solve-quadratic-eqation-test
  (is (= [3/2 1] (ch-04/solve-quadratic-eqation [2 -5 3] :factorisation)))
  (is (= [2/3 -1/2] (ch-04/solve-quadratic-eqation [6 -1 -2] :factorisation)))
  (is (= [0.8164965809277261 0.8164965809277261] (ch-04/solve-quadratic-eqation [3 (- (* 2 (Math/sqrt 6))) 2] :factorisation))))

(deftest discriminant-test
  (is (= 16.0 (ch-04/discriminant [1 0 -4]))))

(deftest nature-of-roots-test
  (is (= :distinct-real-roots (ch-04/nature-of-roots [1 0 -4])))
  (is (= :no-real-roots (ch-04/nature-of-roots [2 -4 3]))))