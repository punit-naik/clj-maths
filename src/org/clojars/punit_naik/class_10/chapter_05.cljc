(ns org.clojars.punit-naik.class-10.chapter-05
  "Arithmetic progressions"
  (:require [org.clojars.punit-naik.class-10.chapter-03 :as ch-03]))

(defn arithmetic-progression
  "Checks if a progression is an arithmetic progression or not and returns `d`"
  [progression]
  (loop [[a b :as p] progression
         differences []]
    (if (= (count differences) (dec (count progression)))
      (when (apply = differences)
        (first differences))
      (recur (rest p) (conj differences (- b a))))))

(defn nth-arithmetic-progression
  "Finds the nth term for the arithmetic prgression, when the first term and difference is given"
  [a d n]
  (+ a (* (dec n) d)))

(defn find-n
  "Finds out the `n` for the number `an` in the arithmetic progression `ap`
   an = a + (n-1) x d"
  [[a :as ap] an]
  (let [d (arithmetic-progression ap)]
    (float (/ (+ an (* -1 a) d) d))))

(defn exists-in-arithmetic-progression?
  "Checks if `an` term exists in the arithmetic progression `ap`
   Hint: Solve for `n` in the function above"
  [ap an]
  (let [n (find-n ap an)]
    (= (int n) n)))

(defn generate-arithmetic-progression
  "Generates arithmetic progression given n, m, nth and mth terms of the ap, returns first `x` items
   Hint: Uses the formula from the `find-n` function above and solves a pair of linear equations"
  [[n nth] [m mth] & [x]]
  (let [[a d] (ch-03/solve-eq-pair [1 (dec n) (* -1 nth)] [1 (dec m) (* -1 mth)])]
    (loop [a a
           ap [a]]
      (if (= (count ap) (or x 10))
        ap
        (let [next-term (+ a d)]
          (recur next-term (conj ap next-term)))))))

(defn sum-n
  "Gives the sum of first `n` terms of an ap"
  [[a :as ap] n]
  (float (/ (* n (+ (* 2 a) (* (dec n) (arithmetic-progression ap)))) 2)))