(ns org.clojars.punit-naik.class-11.chapter-02-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [org.clojars.punit-naik.class-11.chapter-02 :as ch-02]))

(deftest cartesian-product-test
  (is (= [["red" "b"] ["red" "c"] ["red" "s"]
          ["blue" "b"] ["blue" "c"] ["blue" "s"]]
         (ch-02/cartesian-product
          ["red" "blue"]
          ["b" "c" "s"]))))