(ns org.clojars.punit-naik.class-10.chapter-14
  "Surface areas and volumes"
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pprint]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defmulti frequencies-provided?
  (fn [_ interval?] interval?))

(defmethod frequencies-provided? false
  [data _]
  (coll? (first data)))

(defmethod frequencies-provided? true
  [data _]
  (and (coll? (first (first data)))
       (pos-int? (second (first data)))))

(defmulti mean
  (fn [_ method] method))

(defmethod mean :normal
  [data _]
  (comment "Simply calculates average
            The data can be either `[1 2 3 4]` which is a plain list of observations or
            `[[1 2] [2 1] [3 5] [4 6]]` which is a pair of observations and it's frequencies")
  (let [freq-provided? (frequencies-provided? data false)
        sum-of-frequencies (if-not freq-provided?
                             (count data)
                             (->> data (map second) (reduce +)))
        sum-of-observations-times-frequencies (if-not freq-provided?
                                                (reduce + data)
                                                (->> data (map (partial reduce *)) (reduce +)))]
    (/ sum-of-observations-times-frequencies sum-of-frequencies)))

(defmethod mean :intervals-direct-method
  [data _]
  (comment "In this case the observations are an interval")
  (let [freq-provided? (frequencies-provided? data true)
        sum-of-frequencies (if-not freq-provided?
                             (count data)
                             (->> data (map second) (reduce +)))
        sum-of-observations-times-frequencies (if-not freq-provided?
                                                (->> data (map #(mean % :normal)) (reduce +))
                                                (->> data
                                                     (map
                                                      (fn [[interval frequency]]
                                                        (* (mean interval :normal)
                                                           frequency)))
                                                     (reduce +)))]
    (/ sum-of-observations-times-frequencies sum-of-frequencies)))

(defmethod mean :intervals-assumed-mean-method
  [data _]
  (comment "In this case the observations are an interval")
  (let [freq-provided? (frequencies-provided? data true)
        frequencies (if-not freq-provided?
                      (repeat (count data) 1)
                      (map second data))
        sum-of-frequencies (reduce + frequencies)
        xi (map #(if-not freq-provided?
                   (mean % :normal)
                   (mean (first %) :normal)) data)
        assumed-mean (nth xi (int (/ (count xi) 2)))
        di (map #(- % assumed-mean) xi)
        sum-of-di-times-frequencies (if-not freq-provided?
                                      (reduce + di)
                                      (->> frequencies
                                           (map #(* %1 %2) di)
                                           (reduce +)))]
    (/ sum-of-di-times-frequencies sum-of-frequencies)))

(defmethod mean :intervals-step-deviation-method
  [data _]
  (comment "In this case the observations are an interval")
  (let [freq-provided? (frequencies-provided? data true)
        frequencies (if-not freq-provided?
                      (repeat (count data) 1)
                      (map second data))
        sum-of-frequencies (reduce + frequencies)
        interval-size (cond->> (first data)
                        freq-provided? first
                        true (reduce -)
                        true (Math/abs))
        xi (map #(if-not freq-provided?
                   (mean % :normal)
                   (mean (first %) :normal)) data)
        assumed-mean (nth xi (int (/ (count xi) 2)))
        ui (map #(/ (- % assumed-mean) interval-size) xi)
        sum-of-ui-times-frequencies (if-not freq-provided?
                                      (reduce + ui)
                                      (->> frequencies
                                           (map #(* %1 %2) ui)
                                           (reduce +)))]
    (/ sum-of-ui-times-frequencies sum-of-frequencies)))

(defn mode
  "In this case the observations are an interval and frequencies are always given"
  [data]
  (let [modal-data (->> (map-indexed vector data)
                        (sort-by #(second (second %)) >)
                        first) ;; [i [[1 2] 5]]
        modal-class (first (second modal-data))
        l (first modal-class)
        h (Math/abs (reduce - modal-class))
        f1 (second (second modal-data))
        f0 (->> (first modal-data)
                dec
                (nth data)
                second)
        f2 (->> (first modal-data)
                inc
                (nth data)
                second)]
    (+ l (* (/ (- f1 f0) (- (* 2 f1) f0 f2)) h))))

(defmulti median
  (fn [_ interval?] interval?))

(defmethod median false
  [data _]
  (let [sorted-data (->> data
                         (sort-by first))
        n (->> data (map second) (reduce +))
        cumulative-frequencies (->> sorted-data
                                    (map second)
                                    (reductions +))
        relevant-observations (cond->> (->> (map vector sorted-data cumulative-frequencies)
                                            (keep (fn [[d cf]]
                                                    (when (>= cf (if (even? n)
                                                                   (/ n 2)
                                                                   (/ (inc n) 2)))
                                                      d))))
                                (even? n) (take 2)
                                (not (even? n)) (take 1))]
    (mean (->> relevant-observations (map first)) :normal)))

(defmethod median true
  [data _]
  (let []))