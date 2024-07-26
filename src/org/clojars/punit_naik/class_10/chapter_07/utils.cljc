(ns org.clojars.punit-naik.class-10.chapter-07.utils
  "Co-ordinate geometry uitls"
  (:require [org.clojars.punit-naik.class-10.chapter-07 :as ch-07]
            #?(:clj [clojure.math :as math]
               :cljs [cljs.math :as math])
            [clojure.string :as str]))

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

(def multiplication-factor 2.5)

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
       height (dec (* max-vertical (+ max-vertical (math/ceil (* max-vertical 0.3))) 2))
       width (dec (* 2 height multiplication-factor))
       width (if (<= width (* max-horizontal 2))
               (* 2 width)
               width)]
   [width height]))

(defn calculate-dimension-origin
  [[width height]]
  [(int (quot width 2))
   (int (quot height 2))])

(defn init-grid
  [[width height]]
  (->> (repeat width " ")
       (into [])
       (repeat height)
       (into [])))

(defn draw-axes
  [[width height] [width-center height-center] grid]
  (let [grid-with-x-axis-drawn (->> (range (dec width))
                                    (reduce
                                     (fn [g i]
                                       (assoc-in
                                        g [height-center i]
                                        (if (odd? (inc i))
                                          " "
                                          "-")))
                                     grid))]
    (->> (range height)
         (reduce
          (fn [g i]
            (assoc-in g [i width-center] "|"))
          grid-with-x-axis-drawn))))

(defn plot-vector
  [grid
   [width-center height-center]
   {:keys [label]
    :or {label "."}
    [x y] :point}]
  (let [plot-y (+ height-center y)
        plot-x (+ width-center (* 2 x))
        co-ordinate-value (get-in grid [plot-y plot-x])
        co-ordinate-value-exists? (and (not= "-" co-ordinate-value)
                                       (not= "|" co-ordinate-value)
                                       (not= " " co-ordinate-value)
                                       (not= "." co-ordinate-value)
                                       (not= nil co-ordinate-value))]
    ;; Below, we plot y component first because of the structure of our grid
    ;; As it's a coll where each row (height/y-component) is a collection of column (width/x-component) values
    (cond-> grid
      (not co-ordinate-value-exists?)
      (assoc-in [plot-y plot-x] label)
      #_co-ordinate-value-exists?
      #_(assoc-in [plot-y plot-x] (str co-ordinate-value "," label)))))

(defn identify-data
  [data]
  (->> data
       (map-indexed
        (fn [i v]
          {:point v
           :label (str (char (+ i 97)))}))))

(defn print-identified-vectors
  [identified-vectors]
  (->> identified-vectors
       (group-by :point)
       (map
        (fn [[point labels]]
          (str
           (->> labels
                (map :label)
                (str/join ","))
           " -> "
           point)))
       (str/join "\n")
       println))

(defn plot-vectors
  [grid origin vectors & [print-identified-vectors?]]
  (let [identified-vectors (identify-data vectors)]
    (when print-identified-vectors?
     (print-identified-vectors identified-vectors))
    (reduce
     (fn [g v]
       (plot-vector g origin v))
     grid
     identified-vectors)))

(defn print-grid [grid]
  (doseq [row (reverse grid)]
    (println (apply str row))))

(defn test []
  (let [vectors [[1 0] [0 1] [3 3] [-2 -2] [3 3] [4 3] [6 3]]
        dimension (determine-dimension vectors)
        origin (calculate-dimension-origin dimension)
        grid (init-grid dimension)
        grid-with-axes (draw-axes dimension origin grid)
        grid-with-vectors-plotted (plot-vectors grid-with-axes origin vectors true)]
    (print-grid grid-with-vectors-plotted)))