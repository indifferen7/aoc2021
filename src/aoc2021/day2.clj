(ns aoc2021.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; data and shared stuff
(def sample-data "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2")

(defn line->command
  [in]
  (let [[dir steps] (str/split in #" ")]
    [dir (read-string steps)]))

(def real-data
  (->>
    (slurp (io/resource "day2"))
    (str/split-lines)
    (map line->command)))

(defn run-commands
  [fn data]
  (->
    (reduce fn {:depth 0 :hpos 0 :aim 0} data)
    (select-keys [:depth :hpos])
    (vals)
    (as-> vals (apply * vals))))

; part one
(defn part-one-fn
  [acc curr]
  (let [{:keys [depth hpos]} acc
        [dir steps] curr]
    (case dir
      "forward" (assoc acc :hpos (+ hpos steps))
      "up" (assoc acc :depth (- depth steps))
      "down" (assoc acc :depth (+ depth steps)))))

(run-commands part-one-fn real-data)

; part two
(defn part-two-fn
  [acc curr]
  (let [{:keys [depth hpos aim]} acc
        [dir steps] curr]
    (case dir
      "forward" (assoc acc :hpos (+ hpos steps) :depth (+ depth (* aim steps)))
      "up" (assoc acc :aim (- aim steps))
      "down" (assoc acc :aim (+ aim steps)))))

(run-commands part-two-fn real-data)