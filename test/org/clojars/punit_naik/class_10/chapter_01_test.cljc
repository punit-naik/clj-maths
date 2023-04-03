(ns org.clojars.punit-naik.class-10.chapter-01-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.clojars.punit-naik.class-10.chapter-00 :as ch-00]
            [org.clojars.punit-naik.class-10.chapter-01 :as ch-01]))

(deftest theorem-1-1-test
  (is (= [2 5] (ch-01/theorem-1-1 [17 6])))
  (is (= [0 1] (ch-01/theorem-1-1 [1 5])))
  (is (= [5 0] (ch-01/theorem-1-1 [20 4])))
  (is (= [3 1] (ch-01/theorem-1-1 [10 3])))
  (is (= [0 4] (ch-01/theorem-1-1 [4 19])))
  (is (= [27 0] (ch-01/theorem-1-1 [81 3]))))

(deftest folk-puzzle-test
  (testing "The only correct answer to the folk puzzle"
    (is (= 119 (ch-01/folk-puzzle 150)))
    (is (= 119 (ch-01/folk-puzzle 140)))
    (is (= 119 (ch-01/folk-puzzle 130)))
    (is (= 119 (ch-01/folk-puzzle 135)))
    (is (= 119 (ch-01/folk-puzzle 121)))))

(deftest highest-common-factor-test
  (doseq [method [:theorem-1-1 :theorem-1-2]]
    (testing (str "HCF By "
                  (if (= method :theorem-1-1)
                    "Euclid's Division Lemma"
                    "Prime Fctorisation")
                  " Method")
      (let [method :theorem-1-1]
        (is (= 7 (ch-01/highest-common-factor [455 42] method)))
        (is (= 4 (ch-01/highest-common-factor [4052 12576] method)))
        (is (= 6 (ch-01/highest-common-factor [6 72 120] method)))))))

(deftest theorem-1-2-test
  (is (= [2 2 2 3 3 5 7 13] (ch-01/theorem-1-2 32760)))
  (is (= [2 2 101] (ch-01/theorem-1-2 404)))
  (is (= [2 2 2 2 2 3] (ch-01/theorem-1-2 96)))
  (is (= [2 2 2 3 5] (ch-01/theorem-1-2 120))))

(deftest least-common-multiple-test
  (testing "LCM By Prime Factorisation Method"
    (let [method :theorem-1-2]
      (is (= 45 (ch-00/least-common-multiple [15 9] method)))
      (is (= 15 (ch-00/least-common-multiple [3 15] method)))
      (is (= 45 (ch-00/least-common-multiple [45 5] method)))
      (is (= 20 (ch-00/least-common-multiple [4 5] method))))))

(deftest theorem-1-5-test
  (is (= [375 1000] (ch-01/theorem-1-5 [3 8])))
  (is (= [104 1000] (ch-01/theorem-1-5 [13 125])))
  (is (= [875 10000] (ch-01/theorem-1-5 [7 80])))
  (is (= [233408 10000] (ch-01/theorem-1-5 [14588 625])))
  (is (thrown? AssertionError (ch-01/theorem-1-5 [2 8])))
  (is (thrown? AssertionError (ch-01/theorem-1-5 [15 125]))))

(deftest theorem-1-6-test
  (is (= [250 1000] (ch-01/theorem-1-6 [2 8])))
  (is (= [120 1000] (ch-01/theorem-1-6 [15 125]))))

(deftest theorem-1-7-test
  (is (false? (ch-01/theorem-1-7 [50 22])))
  (is (true? (ch-01/theorem-1-7 [50 20]))))