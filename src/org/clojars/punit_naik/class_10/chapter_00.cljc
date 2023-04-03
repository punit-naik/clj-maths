(ns org.clojars.punit-naik.class-10.chapter-00
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pprint]
            [clojure.string :as str]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defn absolute
  [n]
  (Math/abs
   (if (#?(:clj ratio?
           :cljs identity) n)
     (double n) n)))

(defn cartesian-product
  "Generates all combinations of elements from `a-coll` paried with elements from `b-coll`"
  [a-coll b-coll]
  (->> (map
        #(map (partial conj [%]) b-coll)
        a-coll)
       (apply concat)))

(defn recurring-decimal
  [[num denom]]
  (loop [remainders {}
         result nil
         remainder (mod num denom)]
    (if (or (zero? remainder)
            (contains? remainders remainder))
      (when-not (zero? remainder)
        (#?(:clj Long/parseLong :cljs js/ParseInt) (subs result (get remainders remainder))))
      (recur (assoc remainders remainder (count result))
             (str result (int (/ (* 10 remainder) denom)))
             (mod (* 10 remainder) denom)))))

(defn in
  [coll v]
  (->> coll
       (filter #(= v %))
       count
       zero?
       not))

(defn not-in
  [coll v]
  (not (in coll v)))

(defn display-powers
  "Displays a number's powers if repeated in a collection"
  [coll]
  (->> coll
       frequencies
       (sort-by first)
       (map #(str (first %) "^" (second %)))
       (str/join " * ")))

(defn factors
  "Finds all factors of `num`"
  [num & [visualise?]]
  {:pre [(pos-int? num)
         (or (nil? visualise?)
             (boolean? visualise?))]
   :post [(coll? %)
          (every? pos-int? %)]}
  (if (= 1 num)
    [num]
    (loop [factor 2
           factors []
           steps []]
      (if (> factor num)
        (do (when visualise?
              (pprint/print-table steps))
            factors)
        (let [factor? (zero? (mod num factor))]
          (recur (inc factor)
                 (cond-> factors
                   factor? (conj factor))
                 (if visualise?
                   (conj steps
                         {"Tried Number" factor
                          "Does It Divide?" factor?})
                   steps)))))))

(defn prime-number?
  "Checks if `num` is a prime number"
  [num]
  {:pre [(pos-int? num)]
   :post [(boolean? %)]}
  (or (zero? num)
      (= 1 num)
      (let [[first-factor :as factors] (factors num)]
        (and (= 1 (count factors))
             (= first-factor num)))))

(defn lowest-prime-factor
  [num]
  {:pre [(pos-int? num)]
   :post [(or (nil? %)
              (pos-int? %))]}
  (when (> num 1)
    (loop [try 2
           result nil]
      (or result
          (recur (inc try)
                 (when (and (zero? (mod num try))
                            (prime-number? try))
                   try))))))

(defmulti least-common-multiple
  "Least common multiple is the smallest number
   Which is a common multiple of all the numbers in a list"
  (fn [_ method & [_]] method))

(defn lcm-divide
  "Divides a list of numbers `num-list` by `num`
   If it divides, get the quotient, otherwise return the same number from the list"
  [num-list num]
  (map #(if (zero? (mod % num))
          (quot % num)
          %)
       num-list))

(defmethod least-common-multiple :division
  [num-list _ & [visualise?]]
  (let [sorted-num-list (sort num-list)]
    (loop [carry sorted-num-list
           lpf (some lowest-prime-factor carry)
           lc-set [lpf]
           steps [{"Divisor" lpf
                   "Carry" (str/join ", " carry)}]]
      (if (every? (partial = 1) carry)
        (do (when visualise?
              (pprint/print-table
               (conj steps
                     {"Carry" (str/join ", " carry)})))
            (reduce * lc-set))
        (let [new-carry (lcm-divide carry lpf)
              new-lpf (some lowest-prime-factor new-carry)]
          (recur new-carry
                 new-lpf
                 (cond-> lc-set
                   (not (nil? new-lpf)) (conj new-lpf))
                 (if visualise?
                   (cond-> steps
                     new-lpf (conj {"Divisor" new-lpf
                                    "Carry" (str/join ", " new-carry)}))
                   steps)))))))