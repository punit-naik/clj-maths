(ns org.clojars.punit-naik.class-10.chapter-08-test
  (:require [clojure.test :refer [deftest is]]
            [org.clojars.punit-naik.class-10.chapter-08 :as ch-08]))

(deftest table-8-1-test
  (is (= 0.5 (get (->> (ch-08/table-8-1)
                       (filter #(= "sin A" (get % "angle A")))
                       first)
                  "30 deg"))))