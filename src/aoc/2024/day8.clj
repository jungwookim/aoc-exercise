(ns aoc.2024.day8
  (:require [util.core :as u]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo]))

(defn parse [path]
  (->> path
       (u/read-input)
       (mapv vec)))

(defn prepare [parsed-input]
  (->> parsed-input
       (map-indexed (fn [i row]
                      (map-indexed (fn [j cell]
                                     (when  (not= \. cell)
                                       [cell [i j]])) row)))
       (apply concat)
       (remove nil?)
       (reduce (fn [acc [anthena point]]
                 (if (acc anthena)
                   (update acc anthena conj point)
                   (assoc acc anthena [point]))) {})))

(defn possible-antinode
  [[a1 b1] [a2 b2]]
  (let [dx (- a2 a1)
        dy (- b2 b1)]
    [[(- a1 dx) (- b1 dy)] [(+ a2 dx) (+ b2 dy)]]))


(defn install [{:keys [city anthena point1 point2 total] :as state}]
  (let [antinodes (possible-antinode point1 point2)]
    (reduce (fn [state antinode] 
              (let [node (get-in (state :city) antinode)]
                (cond
                  (nil? node) state
                  (not= anthena node) (-> state
                                          (update-in (cons :city antinode) (constantly \#))
                                          (update :total inc))
                  :else state))) state antinodes)))

(defn merge-cities [cities]
  (apply map
         (fn [& cells]
           (map
            (fn [cell]
              (if (some #{\#} cell) \# (first cell)))
            (apply map vector cells))) cities))

(defn count-antinodes [city]
  (->> city
       flatten
       (filter #{\#})
       count))

(defn process [points anthena city]
  (let [combis (combo/combinations points 2)]
    (reduce (fn [{:keys [total city]} [point1 point2]]
              (install {:city city :total total :anthena anthena :point1 point1 :point2 point2}))
            {:total 0 :city city}
            combis)))

(defn part1 [path]
  (let [city (parse path)
        prepared (prepare city)]
    (->> prepared
         (map (fn [[anthena points]]
                (process points anthena city)))
         (map :city)
         (merge-cities) 
         (count-antinodes))))

(comment
  (def path "resources/2024/input.txt")
  (def parsed-input (parse path))
  (prepare parsed-input)
  parsed-input
  (part1 path)
  )