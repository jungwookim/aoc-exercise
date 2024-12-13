(ns aoc.2024.day10
  (:require [util.core :as u]))

(def grid
  (->> (u/read-input "resources/2024/input.txt")
       (mapv (partial mapv (comp parse-long str)))))

(defn trailheads [grid]
  (for [i (range (count grid))
        j (range (count (grid i)))
        :when (zero? (get-in grid [i j]))]
    [i j]))

(defn four-directions
  [grid point]
  (let [height (get-in grid point)]
    (for [delta [[0 1] [1 0] [0 -1] [-1 0]]
          :let [next (mapv + point delta)
                next-height (get-in grid next)]
          :when (= next-height (inc height))]
      next)))

(defn move
  [grid path]
  (for [next (four-directions grid (first path))]
    (cons next path)))

(defn paths [trailhead]
  (nth (iterate #(mapcat (fn [path]
                           (move grid path)) %) [[trailhead]]) 9))

(defn part1
  []
  (->> grid
       (trailheads)
       (map paths)
       (map (fn [paths-coll]
              (map (fn [paths]
                     (first paths)) paths-coll)))
       (map (fn [paths]
              (reduce #(clojure.set/union %1 (set (vector %2))) #{} paths)))
       (map count)
       (reduce +)))

(defn part2
  []
  (->> grid
       (trailheads)
       (map paths)
       (map (fn [paths-coll]
              (map (fn [paths]
                     (first paths)) paths-coll)))
       (map count)
       (reduce +)))

(comment
  (part1)
  (part2))
