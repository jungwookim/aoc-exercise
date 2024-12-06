(ns aoc.2024.day6
  (:require [util.core :as u]
            [clojure.set]))
(defn parse []
  (->> "resources/2024/input.txt"
       (u/read-input)
       (mapv vec)))

(defn move [{:keys [x y dir map]}]
  (cond
    (= dir :up) (let [point (get-in map [(dec x) y])]
                  (cond
                    (nil? point) nil
                    (= point \#) {:x x :y y :dir :right :map (assoc-in map [x y] \X)}
                    :else {:x (dec x) :y y :dir :up :map (assoc-in map [x y] \X)}))
    (= dir :right) (let [point (get-in map [x (inc y)])]
                     (cond
                       (nil? point) nil
                       (= point \#) {:x x :y y :dir :down :map (assoc-in map [x y] \X)}
                       :else {:x x :y (inc y) :dir :right :map (assoc-in map [x y] \X)}))
    (= dir :down) (let [point (get-in map [(inc x) y])]
                    (cond
                      (nil? point) nil
                      (= point \#) {:x x :y y :dir :left :map (assoc-in map [x y] \X)}
                      :else {:x (inc x) :y y :dir :down :map (assoc-in map [x y] \X)}))
    (= dir :left) (let [point (get-in map [x (dec y)])]
                    (cond
                      (nil? point) nil
                      (= point \#) {:x x :y y :dir :up :map (assoc-in map [x y] \X)}
                      :else {:x x :y (dec y) :dir :left :map (assoc-in map [x y] \X)}))))

(defn move2 [{:keys [x y dir map visited]}] 
  (let [to-visit {:x x :y y :dir dir}]
    (if (get visited to-visit)
      :found-loop
      (-> (move {:x x :y y :dir dir :map map})
          (assoc :visited (conj visited to-visit))))))

(defn count-steps [map]
  ((frequencies (flatten map)) \X))

(defn start-info
  [parsed-data]
  (->> parsed-data
       (map-indexed (fn [i x]
                      (map-indexed (fn [j y]
                                     (when (= y \^)
                                       {:x i :y j :dir :up})) x)))
       (flatten)
       (remove nil?)))

(defn part1 []
  (let [parsed-data (parse)
        {:keys [x y dir]} (start-info parsed-data)
        init-state {:x x :y y :dir dir :map parsed-data}]
    (->> init-state
         (iterate move)
         (take-while some?)
         last
         :map
         count-steps)))

(defn part2 []
  (let [parsed-data (parse)
        multiple-maps (->> parsed-data
                           (map-indexed (fn [i x]
                                          (map-indexed (fn [j y]
                                                         (when (= y \.)
                                                           (assoc-in parsed-data [i j] \#))) x)))
                           (apply concat)
                           (remove nil?))
        {:keys [x y dir]} (start-info parsed-data)]
    (->> multiple-maps
         (map (fn [m]
                (->> {:x x :y y :dir dir :map m :visited #{}}
                     (iterate move2)
                     (drop-while (fn [state]
                                   (and (:x state) (not= state :found-loop))))
                     first
                     (= :found-loop))))
         (filter identity)
         count)))

(comment
  (part2)
  (part1) ;; + 1
  )
