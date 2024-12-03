(ns aoc.2024.day2
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn safe? [list]
  (let [pairs (partition 2 1 list)
        deltas (map (fn [[x y]] (- x y)) pairs)]
    (or (every? #{1 2 3} deltas)
        (every? #{-1 -2 -3} deltas))))

(defn part1 []
  (->> "resources/2024/input.txt"
       (u/read-input)
       (map #(s/split % #"\s+"))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (filter safe?)
       (count)))

(defn generate-list-except-one-element
  "input: [7 6 4 2 1]
       output: [[6 4 2 1] [7 4 2 1] [7 6 2 1] [7 6 4 1] [7 6 4 2]]"
  [list]
  (->> list
       count
       range
       (map (fn [idx]
              (keep-indexed #(when-not (= %1 idx) %2) list)))))

(defn soft-safe? [list]
  (or (safe? list)
      (some safe? (generate-list-except-one-element list))))

(defn part2 []
  (->> "resources/2024/input.txt"
       (u/read-input)
       (map #(s/split % #"\s+"))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (filter soft-safe?)
       (count)))

(comment
  (part1)
  (part2))
