(ns org.clojars.punit-naik.class-11.chapter-01-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [org.clojars.punit-naik.class-11.chapter-01 :as ch-01]))

(deftest properties-of-union
  (let [set-a [1 2]
        set-b [2 3]
        set-c [3 4]
        universal-set [1 2 3 4 5 6 7 8 9 10]]
    (testing "A ∪ B = B ∪ A"
      (is (= [1 2 3]
             (sort (ch-01/union set-a set-b))
             (sort (ch-01/union set-b set-a)))))
    (testing "( A ∪ B ) ∪ C = A ∪ ( B ∪ C)"
      (is (= [1 2 3 4]
             (sort (ch-01/union (ch-01/union set-a set-b) set-c))
             (sort (ch-01/union set-a (ch-01/union set-b set-c))))))
    (testing "A ∪ φ = A"
      (is (= set-a (ch-01/union set-a []))))
    (testing "A ∪ A = A"
      (is (= set-a
             (sort (ch-01/union set-a set-a)))))
    (testing "U ∪ A = U, with universal set as [1 2 3 4 5 6 7 8 9 10]"
      (is (= universal-set
             (sort (ch-01/union universal-set set-a)))))))

(deftest properties-of-intersection
  (testing "A ∩ B = B ∩ A"
    (is (= [2]
           (sort (ch-01/intersection [1 2] [2 3]))
           (sort (ch-01/intersection [2 3] [1 2])))))
  (testing "( A ∩ B ) ∩ C = A ∩ ( B ∩ C )"
    (is (= [4]
           (sort (ch-01/intersection (ch-01/intersection [1 2 3 4] [3 4 5 6]) [4 5 6 7]))
           (sort (ch-01/intersection [1 2 3 4] (ch-01/intersection [3 4 5 6] [4 5 6 7]))))))
  (testing "φ ∩ A = φ"
    (is (= nil (ch-01/intersection [] [1]))))
  (testing "U ∩ A = A"
    (is (= [1 2]
           (sort (ch-01/intersection [1 2])))))
  (testing "A ∩ A = A"
    (is (= [1 2]
           (sort (ch-01/intersection [1 2] [1 2])))))
  (testing "A ∩ ( B ∪ C ) = ( A ∩ B ) ∪ ( A ∩ C )"
    (is (= [3]
           (sort (ch-01/intersection [1 2 3] (ch-01/union [3 4 5] [5 6 7])))
           (sort (ch-01/union
                  (ch-01/intersection [1 2 3] [3 4 5])
                  (ch-01/intersection [1 2 3] [5 6 7])))))))

(deftest difference-test
  (is (= [1]
         (ch-01/difference [1 2] [2 3]))))

(deftest complement-test
  (is (= [1 4]
         (ch-01/complement [1 2 3 4] [2 3]))))

(deftest properties-of-complement
  (testing "With universal set as [1 2 3 4 5 6 7 8 9 10]"
    (let [universal-set [1 2 3 4 5 6 7 8 9 10]
          set-a [1 2]
          set-b [2 3]]
      (testing "A ∪ A′ = U"
        (is (= universal-set
               (sort
                (ch-01/union
                 set-a
                 (ch-01/complement universal-set set-a))))))
      (testing "A ∩ A′ = φ"
        (is (= []
               (sort
                (ch-01/intersection
                 set-a
                 (ch-01/complement universal-set set-a))))))
      (testing "(A ∪ B)´ = A′ ∩ B′"
        (is
         (= [4 5 6 7 8 9 10]
            (sort
             (ch-01/complement
              universal-set
              (ch-01/union set-a set-b)))
            (sort
             (ch-01/intersection
              (ch-01/complement universal-set set-a)
              (ch-01/complement universal-set set-b))))))
      (testing "(A ∩ B)′ = A′ ∪ B′"
        (is
         (= [1 3 4 5 6 7 8 9 10]
            (sort
             (ch-01/complement
              universal-set
              (ch-01/intersection set-a set-b)))
            (sort
             (ch-01/union
              (ch-01/complement universal-set set-a)
              (ch-01/complement universal-set set-b))))))
      (testing "(A′)′ = A"
        (is (= [1 2]
               (sort
                (ch-01/complement
                 universal-set
                 (ch-01/complement
                  universal-set
                  set-a))))))
      (testing "φ′ = U"
        (is (= universal-set
               (ch-01/complement universal-set []))))
      (testing "U′ = φ"
        (is (= []
               (ch-01/complement universal-set universal-set)))))))