(ns _2021.p2
  (:require [clojure.string :as s]))

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn prepare-data [path]
  (->> path
       read-input
       (map #(s/split % #" "))
       (mapv (fn [[k v]] [k (Integer/parseInt v)]))))

(defn logic-part1 [v-2d]
  (reduce (fn [acc [action value]]
            (case action
              "forward" (update acc :x #(+ % value))
              "up" (update acc :y #(- % value))
              "down" (update acc :y #(+ % value))))
          {:x 0
           :y 0}
          v-2d))
(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1
       vals
       (apply *)))

(defn logic-part2 [v-2d]
  (reduce (fn [acc [action value]]
            (case action
              "forward" (-> acc
                            (update :x #(+ % value))
                            (update :y #(+ % (* value (get acc :aim)))))
              "up" (update acc :aim #(- % value))
              "down" (update acc :aim #(+ % value))))
          {:x   0
           :y   0
           :aim 0}
          v-2d))

(defn solve-part2 [path]
  (->> path
       prepare-data
       logic-part2
       ((juxt :x :y))
       (apply *)))

(def sample-input-path "resources/_2021/sample_input_p2.txt")
(def input-path "resources/_2021/input_p2.txt")

(comment
  (prepare-data sample-input-path)
  (solve-part1 sample-input-path)
  (solve-part2 sample-input-path)
  (solve-part1 input-path)
  (solve-part2 input-path))
