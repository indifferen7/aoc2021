(ns aoc2021.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-school
  (frequencies (map read-string (str/split (slurp (io/resource "day6")) #","))))

(defn tick [school]
  (reduce
    (fn [acc curr]
      (case curr
        0 (assoc acc
            6 (+ (school 0 0) (acc 6 0))
            8 (get school 0 0))
        (assoc acc (dec curr) (school curr 0))))
    {}
    (reverse (range 9))))

(defn school-size-after [days]
  (loop [days-left days
         school initial-school]
    (if (= 0 days-left)
      (reduce + (vals school))
      (recur (dec days-left) (tick school)))))

(println "Part one:" (school-size-after 80))
(println "Part two:" (school-size-after 256))