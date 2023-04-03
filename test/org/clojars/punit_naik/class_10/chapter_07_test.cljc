(ns org.clojars.punit-naik.class-10.chapter-07-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.clojars.punit-naik.class-10.chapter-07 :as ch-07]))

(deftest distance-formula-test
  (is (= 1.0 (ch-07/distance-formula [1 1] [2 1])))
  (testing "Do the points (3, 2), (–2, –3) and (2, 3) form a triangle? If so, name the
            type of triangle formed"
    (let [p [3 2]
          q [-2 -3]
          r [2 3]
          pq (ch-07/distance-formula p q)
          qr (ch-07/distance-formula q r)
          pr (ch-07/distance-formula p r)]
      (is (and (> (+ pq qr) pr)
               (> (+ pq pr) qr)
               (> (+ qr pr) pq)))))
  (testing "Show that the points (1, 7), (4, 2), (–1, –1) and (– 4, 4) are the vertices
            of a square."
    (let [a [1 7]
          b [4 2]
          c [-1 -1]
          d [-4 4]
          ab (ch-07/distance-formula a b)
          bc (ch-07/distance-formula b c)
          cd (ch-07/distance-formula c d)
          da (ch-07/distance-formula d a)
          ac (ch-07/distance-formula a c)
          bd (ch-07/distance-formula b d)]
      (is (and (= ab bc cd da)
               (= ac bd))))))

(deftest section-formula-test
  (is (= [7 3] (ch-07/section-formula [4 -3] [8 5] [3 1]))))

(deftest area-of-triangle-test
  (is (= 24 (ch-07/area-of-triangle [1 -1] [-4 6] [-3 -5])))
  (testing "Area is always positive"
    (is (= 2 (ch-07/area-of-triangle [5 2] [4 7] [7 -4]))))
  (testing "Area is zero means the points are collinear"
    (is (zero? (ch-07/area-of-triangle [-1.5 3] [6 -2] [-3 4])))))