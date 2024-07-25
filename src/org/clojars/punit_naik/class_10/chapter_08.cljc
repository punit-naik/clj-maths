(ns org.clojars.punit-naik.class-10.chapter-08
  "Introduction to Trigonometry"
  (:require
   #?(:clj [clojure.math :as math]
      :cljs [cljs.math :as math])))

(defn table-8-1
  []
  (->> [["sin A" 0 0.5 (/ 1 (math/sqrt 2.0)) (/ (math/sqrt 3.0) 2) 1]
        ["cos A" 1 (/ (math/sqrt 3.0) 2) (/ 1 (math/sqrt 2.0)) 0.5 0]
        ["tan A" 0 (/ 1 (math/sqrt 3.0)) 1 (math/sqrt 3.0) "Not Defined"]
        ["cosec A" "Not Defined" 2 (math/sqrt 2.0) (/ 2 (math/sqrt 3.0)) 1]
        ["sec A" 1 (/ 2 (math/sqrt 3.0)) (math/sqrt 2.0) 2 "Not Defined"]
        ["cot A" "Not Defined" (math/sqrt 3.0) 1 (/ 1 (math/sqrt 3.0)) 0]]
       (map (partial zipmap ["angle A" "0 deg" "30 deg" "45 deg" "60 deg" "90 deg"]))))