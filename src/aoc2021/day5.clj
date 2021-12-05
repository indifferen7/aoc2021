(ns aoc2021.day5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

; part one
(def rows (str/split-lines (slurp (io/resource "day5"))))

(defn is-straight? [[x1 y1] [x2 y2]] (or (= x1 x2) (= y1 y2)))

(defn create-points [[x1 y1] [x2 y2]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(defn line->points [line]
  (let [[x1y1 x2y2] (->> (str/split line #" -> ")
                         (map #(str/split % #","))
                         (flatten)
                         (map #(read-string %))
                         (partition 2 2))]
    (if (is-straight? x1y1 x2y2)
      (create-points x1y1 x2y2)
      nil)))

(->> (map line->points rows)
     (filter #(not (nil? %)))
     (apply concat)
     (frequencies)
     (vals)
     (filter #(> % 1))
     (count))

; no time today for pt 2 :'(