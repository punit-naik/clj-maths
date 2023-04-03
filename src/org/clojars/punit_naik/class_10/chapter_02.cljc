(ns org.clojars.punit-naik.class-10.chapter-02
  "Polynomials"
  (:require [clojure.string :as str]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defmulti solve-eq
  (fn
    [eq]
    (comment "An equation, `a*x^m+b*x^(m-1)+...+z = 0`, is represented as a list of coefficients, appended by it's constant
              [a, b, .... , z]
              When we are trying to solve an equation, we are actually trying to find at which point(s) the plot of the equation
              is crossing the `x-axis`")
    (condp = (count eq)
      2 :linear
      3 :quadratic)))

(defmethod solve-eq :linear
  [[a b]]
  (/ (* -1 b) a))

(defmethod solve-eq :quadratic
  [[a b c]]
  (let [negative-b (* -1 b)
        square-root-of-b-suared-minus-4-ac (Math/sqrt (- (Math/pow b 2) (* 4 a c)))
        twice-a (* 2 a)]
    [(/ (+ negative-b square-root-of-b-suared-minus-4-ac) twice-a)
     (/ (- negative-b square-root-of-b-suared-minus-4-ac) twice-a)]))

(defmulti relationship-between-zeros-and-coefficients
  (fn
    [eq _]
    (comment "2.3 Relationship between Zeroes and Coefficients of a Polynomial")
    (let [eq-count (count eq)]
      (cond
        (= eq-count 3) :quadratic
        (= eq-count 4) :cubic))))

(defn almost-equal
  [a b]
  (let [diff (- (Math/abs a) (Math/abs b))]
   (or (zero? diff)
       (<= diff 0.000001))))

(defmethod relationship-between-zeros-and-coefficients :quadratic
  [[a b c] [alpha beta]]
  (and (almost-equal
        (+ alpha beta)
        (float (/ (* -1 b) a)))
       (almost-equal
        (* alpha beta)
        (float (/ c a)))))

(defmethod relationship-between-zeros-and-coefficients :cubic
  [[a b c d] [alpha beta gamma]]
  (and (almost-equal
        (+ alpha beta gamma)
        (float (/ (* -1 b) a)))
       (almost-equal
        (+ (* alpha beta)
           (* beta gamma)
           (* alpha gamma))
        (float (/ c a)))
       (almost-equal
        (* alpha beta gamma)
        (float (/ (* -1 d) a)))))

(defn human-readable-polynomial
  [eq]
  (let [count-eq (count eq)]
    (->> (map-indexed vector eq)
         (reduce (fn [acc [idx i]]
                   (if-not (zero? i)
                     (let [power (- count-eq (inc idx))
                           i-abs (Math/abs i)]
                       (conj acc
                             (cond-> []
                               (not (zero? idx)) (conj (if (pos? i) "+" "-"))
                               (or (not= i-abs 1)
                                   (zero? power)) (conj i-abs)
                               (> power 0) (conj "x")
                               (> power 1) (conj (str "^" power))
                               true str/join)))
                     acc)) [])
         str/join)))

(defn diff-signs?
  [a b]
  (or (and (pos? a) (neg? b))
      (and (pos? b) (neg? a))))

(defn eq-multiplier
  "Returns the multiplier for the second equation in the arguments
   For the purpose of solving the equations"
  [[a] [b]]
  (float (/ a b)))

(defn divide-polynomial
  "2.4 Divion algorithm for polynomials"
  [eq-numerator eq-denominator & [visualise?]]
  (loop [coefficients eq-numerator
         result []]
    (if (< (count coefficients)
           (count eq-denominator))
      (cond-> {:quotient result}
        (seq coefficients) (assoc :remainder coefficients))
      (let [multiplier (eq-multiplier coefficients eq-denominator)
            to-be-subtracted-from-eq-numerator (-> (map (partial * multiplier) eq-denominator)
                                                   (concat (repeat (- (count coefficients) (count eq-denominator)) 0)))
            sign (if (diff-signs? (first coefficients) (first to-be-subtracted-from-eq-numerator)) 1 -1)
            new-coefficients (->> to-be-subtracted-from-eq-numerator
                                  (map (fn [num denom]
                                         (+ num (* sign denom))) coefficients)
                                  (partition-by identity)
                                  rest
                                  flatten)
            new-result (conj result multiplier)]
        (when visualise?
          (println (str (#?(:clj format :cljs gstring/format)
                         "Dividing %s by %s"
                         (human-readable-polynomial coefficients)
                         (human-readable-polynomial eq-denominator))
                        (str ", quotient: " (-> new-result
                                                (concat (repeat (inc (- (count new-coefficients) (count eq-denominator))) 0))
                                                human-readable-polynomial))
                        (str ", remainder: " (human-readable-polynomial new-coefficients)))))
        (recur new-coefficients new-result)))))