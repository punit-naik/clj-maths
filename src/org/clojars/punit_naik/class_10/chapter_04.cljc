(ns org.clojars.punit-naik.class-10.chapter-04
  "Quadratic Equations"
  (:require [org.clojars.punit-naik.class-10.chapter-00 :as ch-00]
            #?(:clj [clojure.math :as math]
               :cljs [cljs.math :as math])))

(def root-combo?
  (memoize
   (fn [[a-combo c-combo]]
     (and (= a-combo c-combo)
          (double? a-combo)
          (double? c-combo)))))

(defn middle-term-splittable?
  [[a b c] combo]
  (and (= b (apply + combo))
       (let [factors-count-ok? (when-not (root-combo? combo)
                                 (let [factors-combo-roduct (ch-00/factors (ch-00/absolute (apply * combo)))]
                                   (and (> (count factors-combo-roduct) (count (ch-00/factors (ch-00/absolute a))))
                                        (> (count factors-combo-roduct) (count (ch-00/factors (ch-00/absolute c)))))))]
         (if (nil? factors-count-ok?) true factors-count-ok?))))

(defn adjust-combo-sign
  [b [a-combo c-combo :as combo]]
  (let [b-sign (int (/ b (ch-00/absolute b)))
        max-factor (apply max combo)]
    (if (= max-factor a-combo)
      (let [a-combo-adjusted (* b-sign a-combo)]
        [[a-combo-adjusted c-combo]
         [a-combo-adjusted (- c-combo)]])
      (let [c-combo-adjusted (* b-sign c-combo)]
        [[a-combo c-combo-adjusted]
         [(- a-combo) c-combo-adjusted]]))))

(defn split-middle-term
  "Gets the first (`a`) and the last (`c`) term of a quadratic equation
   And generates combinations using their factors which could be used to split
   The middle (`b`) term of the equation
   Returns the split combination"
  [[a b c :as eq]]
  (let [a-c-factors (cond-> (ch-00/factors (ch-00/absolute (* a c)))
                      (double? b) (concat [(math/sqrt (ch-00/absolute (* a c)))]))]
    (->> (ch-00/cartesian-product
          a-c-factors
          a-c-factors)
         (some
          (fn [factors]
            (let [combo (->> (adjust-combo-sign b factors)
                             (some #(when (middle-term-splittable? eq %) %)))]
              (when combo combo)))))))

(defmulti solve-quadratic-eqation
  (fn [_ method] method))

(defn same-roots?
  [[root-1 root-2]]
  (and (double? root-1)
       (double? root-2)
       (not= root-1 root-2)
       (<= (ch-00/absolute (- (ch-00/absolute root-1) (ch-00/absolute root-2))) 0.00000001)))

(defn adjust-roots
  [[root-1 :as roots]]
  (if (same-roots? roots) [root-1 root-1] roots))

(defmethod solve-quadratic-eqation :factorisation
  [[a _ c :as eq] _]
  (let [[a-split c-split :as combo] (split-middle-term eq)]
    (->> [a a-split c-split c]
         (partition-all 2)
         (map-indexed
          (fn [i [first-part second-part :as part]]
            (if (root-combo? combo)
              (if (zero? i)
                [(math/sqrt first-part) (map #(/ % (math/sqrt first-part)) part)]
                [(math/sqrt second-part) (map #(/ % (math/sqrt second-part)) part)])
              [first-part (map #(/ % first-part) part)])))
         (apply map (fn [common-a common-b]
                      (if (coll? common-a)
                        (let [common-a-sign-reversed (map (partial * -1) common-a)]
                          (/ (second common-a-sign-reversed) (ch-00/absolute (first common-a-sign-reversed))))
                        (/ (ch-00/absolute common-b) common-a))))
         adjust-roots)))

(defmethod solve-quadratic-eqation :completing-squares
  ;; TODO
  [eq _])

(defn discriminant
  "Discriminant of a quadratic equation `ax^2+bx+c=0` is: `b^2-4ac`"
  [[a b c]]
  (- (math/pow b 2.0) (* 4 a c)))

(defn nature-of-roots
  [quadratic-eq]
  (let [d (discriminant quadratic-eq)]
    (cond
      (> d 0) :distinct-real-roots
      (zero? d) :equal-real-roots
      :else :no-real-roots)))