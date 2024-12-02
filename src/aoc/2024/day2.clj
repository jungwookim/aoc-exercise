(ns aoc.2024.day2
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn original-safe [list]
  (cond
    (some #(= :invalid %) list) list
    (every? #(= :dec %) list) 1
    (every? #(= :inc %) list) 1
    :else list))

(defn process
  "
  example input: [7 6 4 2 1]
  output: [1 1 1 [1 2]]
  "
  [list]
  (->> list
       (partition 2 1)
       (map (fn [[a b]]
              (let [delta (abs (- a b))]
                (cond
                  (and (> a b) (>= 3 delta)) :dec
                  (and (< a b) (>= 3 delta)) :inc
                  :else :invalid))))
       (original-safe)))

(defn generate-list-except-one-element
  "input: [7 6 4 2 1]
       output: [[6 4 2 1] [7 4 2 1] [7 6 2 1] [7 6 4 1] [7 6 4 2]]"
  [list]
  (map (fn [idx]
         (keep-indexed #(when-not (= %1 idx) %2) list))
       (range (count list))))

(defn part1 []
  (->> "resources/2024/input.txt"
       (u/read-input)
       (map #(s/split % #"\s+"))
       (map #(map (fn [x] (Integer/parseInt x)) %))
       (map process)
       (filter number?)
       (reduce +)))

(defn part2 []
  (let [parsed-input (->> "resources/2024/input.txt"
                          (u/read-input)
                          (map #(s/split % #"\s+"))
                          (map #(map (fn [x] (Integer/parseInt x)) %)))
        process-data (->> parsed-input
                          (map process))
        unsafe-indexes (->> process-data
                            (map-indexed (fn [idx val]
                                           (when-not (number? val) idx)))
                            (remove nil?))
        unsafe-input (->> unsafe-indexes
                          (map (fn [idx]
                                 (nth parsed-input idx))))]
    (+ (->> process-data
            (filter #(= 1 %))
            (reduce +))
       (->> unsafe-input
            (map #(let [safe? (->> %1
                                   (generate-list-except-one-element)
                                   (map process)
                                   (some number?))]
                     (if safe? 1 0)))
            (reduce +)))))

(comment
  (part1)
  (part2))
