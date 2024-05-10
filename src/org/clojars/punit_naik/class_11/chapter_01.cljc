(ns org.clojars.punit-naik.class-11.chapter-01
  "Sets")

;; NOTE: Properties of operatrions on sets will be covered in respective tests

(defn union
  "Rudimentary implementation of `clojure.set/union`"
  [& sets]
  (->> (apply concat sets)
       (reduce
        (fn [m v]
          (assoc m v v))
        {})
       vals))

(defn intersection
  [& sets]
  (->> (apply concat sets)
       (reduce
        (fn [m v]
          (cond-> m
            (contains? m v) (update v inc)
            (not (contains? m v)) (assoc v 1)))
        {})
       (filter
        (fn [[_ v]]
          (if (= 1 (count sets))
            true
            (> v 1))))
       keys))

(defn set-contains?
  "Rudimentary implementation of `clojure.core/contains?`"
  [s v]
  (boolean (seq (filter #(= v %) s))))

(defn difference
  "Rudimentary implementation of `clojure.set/difference`"
  [& sets]
  (loop [result (first sets)
         to-be-subtracted-sets (rest sets)]
    (when (seq sets)
      (if-not (seq to-be-subtracted-sets)
        result
        (recur
         (reduce
          (fn [r v]
            (cond->> r
              (set-contains? r v)
              (remove #(= v %))))
          result
          (first to-be-subtracted-sets))
         (rest to-be-subtracted-sets))))))

(defn complement
  "Complement of set `s1` compared to set `s`"
  [s s1]
  (difference s s1))