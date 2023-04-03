(ns org.clojars.punit-naik.class-10.chapter-07
  "Co-ordinate geometry"
  (:require [org.clojars.punit-naik.class-10.chapter-00 :as ch-00]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defn distance-formula
  [[x1 y1] [x2 y2]]
  (Math/sqrt
   (+ (Math/pow (- x2 x1) 2)
      (Math/pow (- y2 y1) 2))))

(defn section-formula
  [[x1 y1] [x2 y2] [m1 m2]]
  (let [m1+m2 (+ m1 m2)]
    [(/ (+ (* m1 x2) (* m2 x1)) m1+m2)
     (/ (+ (* m1 y2) (* m2 y1)) m1+m2)]))

(defn area-of-triangle
  [[x1 y1] [x2 y2] [x3 y3]]
  (ch-00/absolute
   (/ (+ (* x1 (- y2 y3))
         (* x2 (- y3 y1))
         (* x3 (- y1 y2)))
      2)))