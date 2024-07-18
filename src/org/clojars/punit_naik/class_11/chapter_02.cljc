(ns org.clojars.punit-naik.class-11.chapter-02
  "Relations and Functions")

(defn cartesian-product
  "Basically, a X b"
  [a b]
  (let [a (cond->> a (not (coll? a)) (conj []))
        b (cond->> b (not (coll? b)) (conj []))]
    (->> a
         (mapcat
          (fn [i]
            (->> b
                 (map
                  (fn [j]
                    [i j]))))))))