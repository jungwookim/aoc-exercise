(ns aoc.2024.day1
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn parse [input]
  (mapv #(Integer/parseInt %) (s/split input #"\s+")))

(defn prepare [path]
  (let [input (->> path
                  (u/read-input)
                  (map parse))]
    [(->> input
          (map first)
          sort)
     (->> input
          (map second)
          sort)]))

(defn part1 [path]
  (let [[first_list second_list] (prepare path)]
    (apply + (map #(abs (- %1 %2))
                  first_list
                  second_list))))

(defn part2 [path]
  (let [[first_list second_list] (prepare path)
        freq (frequencies second_list)]
    (apply + (map #(* %1 (get freq %1 0))
                  first_list))))

(comment
  ;; Today I learned
  (def input (->> (str "[" (slurp "resources/2024/day1_part1.txt") "]")
                  read-string
                  (partition 2)
                  (apply map vector)))

  (defn part1b [[l r]]
    (apply + (mapv (comp abs -) (sort l) (sort r))))

  (part1b input)

  (def sample-input-path "resources/2024/day1_part1.txt")

  (part1 sample-input-path)
  (part2 sample-input-path)
  )
