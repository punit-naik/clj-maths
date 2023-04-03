(ns org.clojars.punit-naik.class-10.chapter-01
  "Real Numbers"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [org.clojars.punit-naik.class-10.chapter-00 :refer [least-common-multiple] :as ch-00]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defn add-till
  "Continuosly adds the number `num` to itself till becomes less than or equal to `limit`"
  [num limit]
  (loop [n num]
    (if (> n limit)
      (- n num)
      (recur (+ n num)))))

(defn theorem-1-1
  "*Euclid’s Division Lemma*
   Given positive integers `a` and `b`, there exist unique integers `q` and `r`
   satisfying a = bq + r, 0 ≤ r < b.
   NOTE: Basically `q` is `(quot a b)` and `r` is `(mod a b)`"
  [[a b]]
  {:pre [(pos-int? a)
         (pos-int? b)]
   :post [(>= (second %) 0)
          (< (second %) b)]}
  (cond
    (> b a) [0 a]
    (>= a b) (let [added-till (add-till b a)]
               [(/ added-till b) (Math/abs (- added-till a))])))

(defn folk-puzzle
  "Find the number of eggs i.e. `a`, given the egg basket can't hold more than 150 eggs
   And a set of conditions like:
   1. `a = 7p + 0` OR `(= (mod a 7) 0)`
   2. `a = 6q + 5` OR `(= (mod a 6) 5)`
   3. `a = 5r + 4` OR `(= (mod a 5) 4)`
   4. `a = 4s + 3` OR `(= (mod a 4) 3)`
   5. `a = 3t + 2` OR `(= (mod a 3) 2)`
   6. `a = 2u + 1` OR `(= (mod a 2) 1)`"
  [a-tentative]
  ;; Since `a` is completely divisible by `7`, it's actual value is definitely a multiple of `7`
  ;; `a` could be anything from 7 <= a <= 150
  ;; Lets find the first number, counting down from 150, which satisfies `1`,
  ;; which will give us a potential value of `a`
  (let [a-potential (add-till 7 a-tentative)]
    ;; Finding the actual value of `a`
    (loop [a a-potential]
      ;; Check is `a` satisfies all the other conditions, from `2` to `6`
      (if (->> [[6 5] [5 4] [4 3] [3 2] [2 1]]
               (map (fn [[b remaining]]
                      (->> (theorem-1-1 [a b])
                           second
                           (= remaining))))
               (every? true?))
        a
        (recur (- a 7))))))

(defmulti highest-common-factor
  (fn [_ method & [_]] method))

(defmethod highest-common-factor :theorem-1-1
  [[a b] _ & [visualise?]]
  (loop [[p q :as nums] [a b]
         step 1]
    (if (zero? q)
      p
      (let [[r s] (theorem-1-1 nums)]
        (when visualise?
          (println (str "Step " step ": " p " = " q " * " r " + " s)))
        (recur [q s] (inc step))))))

(defn next-prime
  "Gives the next prime number which comes after `num`"
  [num]
  (loop [n num]
    (if (and (not= n num)
             (ch-00/prime-number? n))
      n
      (recur (inc n)))))

(defn theorem-1-2
  "Prime Factorization
   Every composite number can be expressed (factorised) as a product of primes,
   and this factorisation is unique, apart from the order in which the prime factors occur.
   In general, given a composite number x, we factorise it as x = p1 p2 ... pn,
   where p1, p2,..., pn are primes and written in ascending order, i.e., p1 ≤ p2 ≤ . . . ≤ pn."
  [num & [visualise?]]
  (if (<= num 2)
    [num]
    (loop [start-prime 2
           quotient num
           result []]
      (if (= quotient 1)
        (do (when visualise?
              (->> result
                   (partition-by identity)
                   (map (fn [[num :as partition]]
                          (let [partition-count (count partition)]
                            (cond-> num
                              (> partition-count 1) (str "^" partition-count)))))
                   (str/join " * ")
                   (str "Prime factorization of `" num "` is: ")
                   println))
            result)
        (let [[p q] (theorem-1-1 [quotient start-prime])
              remainder-zero? (zero? q)]
          (recur (if remainder-zero? start-prime (next-prime start-prime))
                 (if remainder-zero? p quotient)
                 (if remainder-zero? (conj result start-prime) result)))))))

(defmethod least-common-multiple :theorem-1-2
 [num-list _ & [visualise?]]
 (let [prime-factors (map theorem-1-2 num-list)
       distinct-prime-factors (->> prime-factors
                                   (apply concat)
                                   distinct
                                   sort)
       lcm-raw (reduce (fn [result prime-factor]
                         (->> prime-factors
                              (map (fn [pf]
                                     (->> pf
                                          (filter (partial = prime-factor))
                                          count)))
                              (apply max)
                              (conj [prime-factor])
                              (conj result)))
                       []
                       distinct-prime-factors)]
   (when visualise?
     (println
      (str (->> lcm-raw
                (map (partial str/join "^"))
                (str/join ", "))
           " are the greatest powers of "
           (str/join ", " distinct-prime-factors)
           " respectively, involved in the numbers "
           (->> num-list
                sort
                (str/join ", ")))))
   (->> (map (fn [[prime-factor power]]
               (Math/pow prime-factor power))
             lcm-raw)
        (reduce *)
        long)))

(defmethod highest-common-factor :theorem-1-2
  [num-list _ & [visualise?]]
  {:pre [(coll? num-list)]
   :post [(pos-int? %)]}
  (let [prime-factors (map theorem-1-2 num-list)
        smallest-power-common-factors
        (->> (map set prime-factors)
             (apply set/intersection)
             (reduce (fn [result common-prime-factor]
                       (->> prime-factors
                            (keep (fn [pf]
                                    (let [cpf (->> pf
                                                   (filter (partial = common-prime-factor))
                                                   seq)]
                                      (and cpf (count cpf)))))
                            sort
                            first
                            (conj [common-prime-factor])
                            (conj result)))
                     []))]
    (when visualise?
      (println
       (str (->> smallest-power-common-factors
                 (sort-by first)
                 (map (fn [[common-prime-factor power]]
                        (str common-prime-factor "^" power)))
                 (str/join ", "))
            " are the greatest powers of the common prime factors "
            (->> smallest-power-common-factors
                 (sort-by first)
                 (map first)
                 (str/join ", "))
            " respectively, involved in the numbers "
            (str/join ", " num-list))))
    (->> (map (fn [[common-prime-factor power]]
                (Math/pow common-prime-factor power))
              smallest-power-common-factors)
         (reduce *)
         long)))

(defn theorem-1-3
  "Let p be a prime number.
   If p divides a^2, then p divides a, where
   a is a positive integer."
  [])

(defn theorem-1-4
  "Square-root of prime numbers is irrational"
  [])

(defn theorem-1-5
  "Let x be a rational number whose decimal expansion terminates.
   Then x can be expressed in the form , p/q
   where p and q are coprime, and the prime factorisation of q
   is of the form 2^n*5^m, where n, m are non-negative integers
   
   We can convert a rational number of the form p/q,
   where q is of the form 2^n5^m, to an equivalent rational number of the form,
   a/ b, where b is a power of 10.
   Therefore, the decimal expansion of such a rational number terminates"
  [[num denom] & [visualise? skip-coprime-check?]]
  {:pre [(pos-int? num)
         (pos-int? denom)
         ;; `num` and `denom` are coprimes
         (or skip-coprime-check?
             (not (seq (set/intersection (into #{} (ch-00/factors num)) (into #{} (ch-00/factors denom))))))
         (->> (theorem-1-2 denom)
              (every? (partial ch-00/in [2 5])))]
   :post [(zero? (mod (second %) 10))]}
  (let [prime-factors-num (theorem-1-2 num)
        prime-factors-denom (theorem-1-2 denom)
        prime-factors-denom-counts (frequencies prime-factors-denom)
        max-count (val (apply max-key val prime-factors-denom-counts))
        two-count (get prime-factors-denom-counts 2 0)
        max-minus-two (- max-count two-count)
        five-count (get prime-factors-denom-counts 5 0)
        max-minus-five (- max-count five-count)
        repeat-and-append (fn [times repeat-num to-coll]
                            (into (repeat times repeat-num) to-coll))
        prime-factors-denom-adjusted-to-ten (->> prime-factors-denom
                                                 (repeat-and-append max-minus-two 2)
                                                 (repeat-and-append max-minus-five 5))
        ;; Multiply prime factors of the numberator by the same adjustment done above 
        prime-factors-num-adjusted (->> prime-factors-num
                                        (repeat-and-append max-minus-two 2)
                                        (repeat-and-append max-minus-five 5))
        result [(reduce * prime-factors-num-adjusted) (reduce * prime-factors-denom-adjusted-to-ten)]]
    (when visualise?
      (println (str "Steps: " num " / " denom
                    " = " (ch-00/display-powers prime-factors-num) " / " (ch-00/display-powers prime-factors-denom)
                    (when-not (= (count prime-factors-denom) (count prime-factors-denom-adjusted-to-ten))
                      (str " = " (ch-00/display-powers prime-factors-num-adjusted) " / "
                           (ch-00/display-powers prime-factors-denom-adjusted-to-ten)))
                    " = " (first result) " / 10^" max-count
                    " = " (float (/ (first result) (second result))))))
    result))

(defn theorem-1-6
  "Let x = p/q be a rational number, such that the prime factorisation of q
   is of the form 2^n*5^m, where n, m are non-negative integers.
   Then x has a decimal expansion which terminates."
  [args & [visualise?]]
  ;; Same as Theorem 1.5, without the coprime check
  (theorem-1-5 args visualise? true))

(defn theorem-1-7
  "Let x = p/q , where p and q are coprimes, be a rational number,
   such that the prime factorisation of q is not of the form 2^n*5^m,
   where n, m are non-negative integers.
   Then, x has a decimal expansion which is non-terminating repeating (recurring).
   This function will return true or false, based no whether the decimal expansion is terminating or not."
  [[num denom :as arg]]
  {:pre [(pos-int? num)
         (pos-int? denom)
         #_(->> (theorem-1-2 denom)
              (some (partial ch-00/not-in [2 5])))]
   :post [(boolean? %)]}
  (zero? (or (ch-00/recurring-decimal arg) 0)))