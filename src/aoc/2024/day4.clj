(ns aoc.2024.day4
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn parse [path]
  (->> path
       (u/read-input)))

;; part 1
(def parsed-data (parse "resources/2024/input.txt"))

(def get-point (fn [x y] (str (get-in parsed-data [x y]))))

(def sum-point (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])) 

(defn one-way-process
  [dx dy]
  (map #(vector (* %1 dx) (* %1 dy)) (range 1 4)))

(defn process
  "Counter for XMAS in 8 directions"
  [x y]
  (if (or (not= "X" (get-point x y))
          (nil? (get-point x y)))
    0
    (let [directions [[-1 -1] [-1 0] [-1 1]
                      [0 -1] [0 1]
                      [1 -1] [1 0] [1 1]]]
      (reduce (fn [acc [dx dy]]
                (let [[d1 d2 d3] (one-way-process dx dy)]
                  (if (and (= "M" (apply get-point (sum-point [x y] d1)))
                           (= "A" (apply get-point (sum-point [x y] d2)))
                           (= "S" (apply get-point (sum-point [x y] d3))))
                    (inc acc)
                    acc))) 0 directions))))

(defn part1 []
  (let [init-state {:x 0 :y 0 :total 0}]
    (reduce (fn [{:keys [x y total] :as state}, row]
              (let [sub-result (reduce (fn [{:keys [x y total]}, col]
                                         {:x (inc x) :y y :total (+ total (process x y))}) state row)]
                {:x x :y (inc y) :total (sub-result :total)})) init-state parsed-data)))

;; part 2
(defn process2
  [x y]
  (cond
    (or (not= "A" (get-point x y))
        (nil? (get-point x y))) 0
    (or (and (= "M" (apply get-point (sum-point [x y] [1 1])))
             (= "M" (apply get-point (sum-point [x y] [-1 1])))
             (= "S" (apply get-point (sum-point [x y] [1 -1])))
             (= "S" (apply get-point (sum-point [x y] [-1 -1]))))
        (and (= "M" (apply get-point (sum-point [x y] [1 1])))
             (= "M" (apply get-point (sum-point [x y] [1 -1])))
             (= "S" (apply get-point (sum-point [x y] [-1 1])))
             (= "S" (apply get-point (sum-point [x y] [-1 -1]))))
        (and (= "M" (apply get-point (sum-point [x y] [-1 1])))
             (= "M" (apply get-point (sum-point [x y] [-1 -1])))
             (= "S" (apply get-point (sum-point [x y] [1 1])))
             (= "S" (apply get-point (sum-point [x y] [1 -1]))))
        (and (= "M" (apply get-point (sum-point [x y] [-1 -1])))
             (= "M" (apply get-point (sum-point [x y] [1 -1])))
             (= "S" (apply get-point (sum-point [x y] [1 1])))
             (= "S" (apply get-point (sum-point [x y] [-1 1]))))) 1
    :else 0))

(defn part2 []
  (let [init-state {:x 0 :y 0 :total 0}]
    (reduce (fn [{:keys [x y total] :as state}, row]
              (let [sub-result (reduce (fn [{:keys [x y total]}, col]
                                         {:x (inc x) :y y :total (+ total (process2 x y))}) state row)]
                {:x x :y (inc y) :total (sub-result :total)})) init-state parsed-data)))

(comment
  (process2 1 1)
  (part1)
  (part2)
  (parse "resources/2024/input.txt")
  (or true 1))