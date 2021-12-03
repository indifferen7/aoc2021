(ns aoc2021.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def binary-numbers (str/split-lines (slurp (io/resource "day3"))))
(defn rotate [xs] (apply map list xs))

; part one
(defn gamma-and-epsilon [bits]
  (let [{zeroes \0 ones \1} (frequencies bits)]
    (if (> ones zeroes) [1 0] [0 1])))

(->> (rotate binary-numbers)
     (map gamma-and-epsilon)
     (apply map list)
     (map str/join)
     (map #(Integer/parseInt % 2))
     (reduce *))

; part two
(defn rating-calculator [rating-fn]
  (loop [idx 0
         candidates binary-numbers]
    (let [bits (nth (rotate candidates) idx)
          idx-to-keep (set (rating-fn bits))
          next-candidates (mapv candidates idx-to-keep)]
      (if (= (count next-candidates) 1)
        (Integer/parseInt (first next-candidates) 2)
        (recur (inc idx) next-candidates)))))

(defn ogr-rating-fn [bits]
  (let [{zeroes \0 ones \1} (frequencies bits)
        val-to-keep (if (>= ones zeroes) \1 \0)]
    (keep-indexed #(if (= val-to-keep %2) %1) bits)))

(defn co2-rating-fn [bits]
  (let [{zeroes \0 ones \1} (frequencies bits)
        val-to-keep (if (>= ones zeroes) \0 \1)]
    (keep-indexed #(if (= val-to-keep %2) %1) bits)))

(* (rating-calculator ogr-rating-fn)
   (rating-calculator co2-rating-fn))