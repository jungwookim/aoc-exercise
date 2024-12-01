(ns aoc.2024.day1
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn parse [input]
  (mapv #(Integer/parseInt %) (s/split input #"\s+")))

(defn part1 [path]
  (let [input (u/read-input path)
        first_array (->> input
                         (map parse)
                         (map first)
                         sort)
        second_array (->> input
                          (map parse)
                          (map second)
                          sort)]
    (apply + (map (fn [x y]
                    (abs (- x y)))
                  first_array
                  second_array))))

(defn part2 [path]
  (let [input (u/read-input path)
        first_array (->> input
                         (map parse)
                         (map first)
                         sort)
        second_array (->> input
                          (map parse)
                          (map second)
                          sort)]
    (apply + (map (fn [x]
                    (* x (count (filter #(= x %) second_array))))
                  first_array))))

(comment
  (def sample-input-path "resources/2024/day1_part1.txt")

  (part1 sample-input-path)
  (part2 sample-input-path))
