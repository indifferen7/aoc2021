(ns aoc2021.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def real-data
  (->> (slurp (io/resource "day1"))
       (str/split-lines)
       (map read-string)))

(defn tuples [d] (partition 2 1 d))

(defn count-increases [d]
  (map
    (fn [[n1 n2]]
      (if (> n2 n1) 1 0))
    (tuples d)))

; part one
(reduce + (count-increases real-data))

; part two
(->>
  (partition 3 1 real-data)
  (map #(reduce + %))
  (count-increases)
  (reduce +))