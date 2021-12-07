(ns aoc2021.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def crabs
  (map read-string (str/split (slurp (io/resource "day7")) #",")))

(defn distance [position crab] (Math/abs (- position crab)))

(defn calculate-fuel-cost [algorithm]
  (reduce
    (fn [acc position]
      (assoc acc position (algorithm position)))
    {}
    (range (apply min crabs) (inc (apply max crabs)))))

; part one
(defn fuel-spent-part-one [position]
  (->> (map #(distance position %) crabs)
       (reduce +)))

(println
  "Part one:"
  (->> (calculate-fuel-cost fuel-spent-part-one)
       (clojure.set/map-invert)
       (keys)
       (apply min)))

; part two
(defn fuel-spent-part-two [position]
  (reduce + (map
              #(->> (distance position %)
                  (inc)
                  (range 1)
                  (reduce +))
              crabs)))

(println
  "Part two:"
  (->> (calculate-fuel-cost fuel-spent-part-two)
       (clojure.set/map-invert)
       (keys)
       (apply min)))