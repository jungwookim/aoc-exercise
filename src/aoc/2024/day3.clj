(ns aoc.2024.day3
  (:require [util.core :as u]))

(defn parse [path]
  (->> path
       (u/read-input)))

(defn extract-mul [s]
    (let [re #"mul\((\d+),(\d+)\)"]
      (re-seq re s)))

(defn calc [[_, x, y]]
  (* (Integer/parseInt x) (Integer/parseInt y)))

(defn part1 [path]
  (->> (parse path)
       (map extract-mul)
       (apply concat)
       (map calc)
       (reduce +)))

(defn extract-mul-with-don't-and-do [s]
  (let [re #"mul\((\d+),(\d+)\)|don't\(\)|do\(\)"]
    (re-seq re s)))

(defn process [regexed-list]
  (let [init-state {:enabled true :total 0}]
    (reduce (fn [{:keys [enabled total]} [pattern, x, y]]
              (cond
                (= pattern "don't()") {:enabled false :total total}
                (= pattern "do()") {:enabled true :total total}
                (and enabled x y) {:enabled true :total (+ total (* (Integer/parseInt x) (Integer/parseInt y)))}
                :else {:enabled false :total total})) init-state regexed-list)))

  

(defn part2 [path]
  (->> (parse path)
       (map extract-mul-with-don't-and-do)
       (apply concat)
       (process)
       :total))

(comment
  (def input-path "resources/2024/input.txt")
  (parse input-path)
  (part1 input-path)
  (part2 input-path)

     )