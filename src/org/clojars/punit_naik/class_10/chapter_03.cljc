(ns org.clojars.punit-naik.class-10.chapter-03
  "Pair of linear equations in two variables"
  (:require [org.clojars.punit-naik.class-10.chapter-02 :as ch-02]))

(defn solution
  "And equation ax+by+c=0 is represented as [a b c]"
  [[a b c] & [x-sub]]
  (let [minus-c (* -1 c)
        ;; substituting x = `x-sub`
        y (float (/ (+ minus-c (* -1 a (or x-sub 0))) b))]
    [x-sub y]))

(defn human-readable-eq
  [[a b c]]
  (str (when-not (zero? a)
         (str a "x"))
       (when-not (zero? b)
         (str (when-not (neg? b) "+") b "x"))
       (when-not (zero? c)
         (str (when-not (neg? c) "+") c))
       "=0"))

(defn infinity
  []
  #?(:clj Double/POSITIVE_INFINITY :cljs ##Inf))

(defn solutions-count
  [[a1 b1 c1] [a2 b2 c2]]
  (let [first-ratio (/ a1 a2)
        second-ratio (/ b1 b2)
        third-ratio (/ c1 c2)]
    (cond
      (not= first-ratio second-ratio) 1
      (= first-ratio second-ratio third-ratio) (infinity)
      (and (= first-ratio second-ratio)
           (not= first-ratio third-ratio)) 0)))

(defn solve-eq-pair
  "Solves pair of equations using eimination method"
  [eq-1 eq-2]
  (let [sols-count (solutions-count eq-1 eq-2)
        parallel? (zero? sols-count)
        coincident? (= (infinity) sols-count)
        str-to-be-printed (str "Equations " (human-readable-eq eq-1) " and " (human-readable-eq eq-2))]
    (cond
      coincident? (str str-to-be-printed " are coincident")
      parallel? (str str-to-be-printed " are parallel")
      :else (let [multiplier (ch-02/eq-multiplier eq-1 eq-2)
                  eq-2-multiplied-by-multiplier (map (partial * multiplier) eq-2)
                  sign (if (ch-02/diff-signs? (first eq-1) (first eq-2-multiplied-by-multiplier)) 1 -1)
                  [_ b c] (->> eq-2-multiplied-by-multiplier
                               (map (fn [a b]
                                      (+ a (* sign b))) eq-1))
                  y-intersection (float (/ (* -1 c) b))
                  x-intersection (float (+ (* -1 (last eq-1)) (* -1 (second eq-1) y-intersection)))]
              [x-intersection y-intersection]))))
