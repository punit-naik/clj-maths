(ns org.clojars.punit-naik.class-10.chapter-07.utils
  "Co-ordinate geometry uitls"
  (:require [org.clojars.punit-naik.class-10.chapter-07 :as ch-07]
            #?(:clj [clojure.math :as math]
               :cljs [cljs.math :as math])))

(defn dot-product
  [a b]
  (->> b
       (map
        (fn [i j]
          (* i j))
        a)
       (reduce +)))

(defn generate-rotation-matrix
  "Generates a rotation matrix for a point (x, y) by `theta` degreres clockwise or anti-clockwise
   Corresponding elements of `rotation-matrix-x` and (x, y) are to be multiplied together and added a.k.a dot product
   To get the newly rotated x co-ordinate
   Same thing to be done for the y cordinate"
  [theta & [clockwise?]]
  (let [theta-degrees (math/to-radians theta)
        rotation-matrix-x [(math/cos theta-degrees)
                           ((if clockwise? + -)
                            (math/sin theta-degrees))]
        rotation-matrix-y [((if clockwise? - +)
                            (math/sin theta-degrees))
                           (math/cos theta-degrees)]]
    ;; Have to do some rounding because of irrational value of pi
    [(map math/round rotation-matrix-x)
     (map math/round rotation-matrix-y)]))

(defn rotate-vector
  "Rotate a vector defined in it's two point form
   Where the points are `a` and `b`
   By `theta` degrees in the anti-clockwise direction by default, unless specified"
  [clockwise? theta a b]
  (let [[rotation-matrix-x rotation-matrix-y]
        (generate-rotation-matrix theta clockwise?)
        rotated-a [(dot-product a rotation-matrix-x)
                   (dot-product a rotation-matrix-y)]
        rotated-b [(dot-product b rotation-matrix-x)
                   (dot-product b rotation-matrix-y)]]
    [rotated-a rotated-b]))

(defn unit-vector
  "Finds the unit vector in two point form of any vector"
  [a b]
  (let [magnitude (ch-07/distance-formula a b)]
    [(->> a
          (map #(/ % magnitude)))
     (->> b
          (map #(/ % magnitude)))]))

;; `grid-size` should always be odd
(def vertical-grid-size 11)
(def multiplication-factor 2.5)
;; multiplying by 2 because two x direction units are equal to one y direction unit, so that they look symmetrical
(def horizontal-grid-size (dec (* 2 vertical-grid-size multiplication-factor)))
(def horizontal-center (int (quot horizontal-grid-size 2)))
(def vertical-center (int (quot vertical-grid-size 2)))

(defn max-co-ordinate
  [data & [vertically?]]
  (->> data
       (map (if vertically? second first))
       (sort >)
       first))

(defn determine-dimension
  "Determines the dimension of the plot based on data"
  [data]
  (let[max-horizontal (max-co-ordinate data)
       max-vertical (max-co-ordinate data true)
       height (dec (* 4 max-vertical))
       width (dec (* 2 height multiplication-factor))
       width (if (<= width (* max-horizontal 2))
               (* 2 width)
               width)]
   [width height]))

(defn init-grid
  [[width height]]
  (->> (repeat width " ")
       (into [])
       (repeat height)
       (into [])))

(defn draw-axes
  [grid]
  (let [grid-with-x-axis-drawn (->> (range (dec horizontal-grid-size))
                                    (reduce
                                     (fn [g i]
                                       (assoc-in
                                        g [vertical-center i]
                                        (if (odd? (inc i))
                                          " "
                                          "-")))
                                     grid))]
    (->> (range vertical-grid-size)
         (reduce
          (fn [g i]
            (assoc-in g [i horizontal-center] "|"))
          grid-with-x-axis-drawn))))

(defn plot-vector [grid [x y]]
  (let [;; below is actually y co-ordinate
        plot-x (+ vertical-center x)
        ;; below is actually x co-ordinate
        plot-y (+ horizontal-center (* 2 y))]
    (assoc-in grid [plot-x plot-y] "x")))

(defn plot-vectors [grid vectors]
  (reduce plot-vector grid vectors))

(defn print-grid [grid]
  (doseq [row (reverse grid)]
    (println (apply str row))))

(defn test []
  (let [vectors [[1 0] [0 1] [3 3] [-2 -2]]
        grid (init-grid (determine-dimension vectors))
        grid-with-axes (draw-axes grid)
        final-grid (plot-vectors grid-with-axes vectors)]
    (print-grid final-grid)))