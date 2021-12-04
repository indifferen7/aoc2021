(ns aoc2021.day4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

; utils
(defn rotate [xs] (apply map list xs))

; part one
(def lines (str/split-lines (slurp (io/resource "day4"))))
(defn parse-row [row]
  (->> (str/split row #" ")
       (filter #(not (str/blank? %)))
       (map #(read-string %))))
(defn parse-rows [rows] (map parse-row rows))

(def boards
  "All the boards"
  (->> (partition 6 (drop 1 lines))
       (map #(drop 1 %))
       (map parse-rows)))

(defn possible-ways-to-bingo
  "Return a set with all possible number combos to reach bingo for the given board."
  [board]
  (let [rows (map set board)
        cols (map set (rotate board))]
    (concat rows cols)))

(def boards-bingo-ways
  "A seq with all possible ways boards can reach bingo."
  (map possible-ways-to-bingo boards))

(def numbers-to-draw
  "The numbers drawn in the bingo game."
  (map read-string (str/split (first lines) #",")))

(defn bingo-fn [numbers-drawn]
  (fn [board-bingo-ways]
    (every? numbers-drawn board-bingo-ways)))

(loop [numbers numbers-to-draw
       numbers-drawn #{}]
  (let [[just-drawn & numbers-left] numbers
        new-numbers-drawn (conj numbers-drawn just-drawn)
        bingo? (bingo-fn new-numbers-drawn)
        outcome (map #(filter bingo? %) boards-bingo-ways)
        winner (keep-indexed #(if (not (empty? %2)) %1) outcome)]
    (if (empty? winner)
      (recur numbers-left new-numbers-drawn)
      (let [winner-idx (first winner)
            board-numbers (set (flatten (nth boards winner-idx)))
            numbers-not-drawn-in-board (apply disj board-numbers new-numbers-drawn)]
        (println "Result part one:"
                 (* just-drawn
                    (reduce + numbers-not-drawn-in-board)))))))

; part two
(loop [numbers numbers-to-draw
       numbers-drawn #{}]
  (let [[just-drawn & numbers-left] numbers
        new-numbers-drawn (conj numbers-drawn just-drawn)
        bingo? (bingo-fn new-numbers-drawn)
        outcome (map #(filter bingo? %) boards-bingo-ways)
        all-has-won? (every? false? (map empty? outcome))
        ]
    (if (not all-has-won?)
      (recur numbers-left new-numbers-drawn)
      (let [bingo-last-round? (bingo-fn numbers-drawn)
            last-round-outcome (map #(filter bingo-last-round? %) boards-bingo-ways)
            loser-idx (first (keep-indexed #(if (empty? %2) %1) last-round-outcome))
            board-numbers (set (flatten (nth boards loser-idx)))
            numbers-not-drawn-in-board (apply disj board-numbers new-numbers-drawn)]
        (println )
        (println "Result part two:"
                 (* just-drawn
                    (reduce + numbers-not-drawn-in-board)))))))
