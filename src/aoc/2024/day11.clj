(ns aoc.2024.day11
  (:require [util.core :as u]
            [clojure.string :as s]
            [clojure.math :as math]))

(def input
  (map parse-long (-> "resources/2024/input.txt"
                      (u/read-input)
                      first
                      (s/split #" "))))

(defn blink [num]
  (let [str-num (str num)
        half-size (quot (count str-num) 2)
        remaining (rem (count str-num) 2)]
    (cond
      (zero? num) [1]
      (even? remaining) [(int (quot num (math/pow 10 half-size))) (int (rem num (math/pow 10 half-size)))]
      :else [(* 2024 num)])))

(defn part1
  []
  (count (nth (iterate #(mapcat blink %) input) 25)))

(defn blink2
  [freq]
  (apply merge-with + (for [[num cnt] freq
                            new-nums (blink num)]
                        {new-nums cnt})))

(defn part2
  []
  (let [result (nth (iterate blink2 (frequencies input)) 75)]
    (reduce + (vals result))))

(comment

  (part1)
  (part2))