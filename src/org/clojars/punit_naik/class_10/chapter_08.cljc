(ns org.clojars.punit-naik.class-10.chapter-08
  "Introduction to Trigonometry"
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pprint]
            #?(:cljs [cljs.math :as Math]))
  (:import #?(:clj [java.lang Math])))

(defn table-8-1
  []
  (->> [["sin A" 0 0.5 (/ 1 (Math/sqrt 2)) (/ (Math/sqrt 3) 2) 1]
        ["cos A" 1 (/ (Math/sqrt 3) 2) (/ 1 (Math/sqrt 2)) 0.5 0]
        ["tan A" 0 (/ 1 (Math/sqrt 3)) 1 (Math/sqrt 3) "Not Defined"]
        ["cosec A" "Not Defined" 2 (Math/sqrt 2) (/ 2 (Math/sqrt 3)) 1]
        ["sec A" 1 (/ 2 (Math/sqrt 3)) (Math/sqrt 2) 2 "Not Defined"]
        ["cot A" "Not Defined" (Math/sqrt 3) 1 (/ 1 (Math/sqrt 3)) 0]]
       (map (partial zipmap ["angle A" "0 deg" "30 deg" "45 deg" "60 deg" "90 deg"]))))