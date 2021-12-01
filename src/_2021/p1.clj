(ns _2021.p1
  (:require [clojure.string :as s]))

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn prepare-data [path]
  (->> path
       read-input
       (mapv #(Integer/parseInt %))))

(defn logic [v-int]
  (reduce (fn [{:keys [cur-val _] :as acc} val]
            (if (> val cur-val)
              (-> acc
                  (update :cur-val (constantly val))
                  (update :ans inc))
              (update acc :cur-val (constantly val))))
          {:cur-val (first v-int)
           :ans 0}
          (rest v-int)))

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic))

(defn solve-part2 [path]
  (->> path
       prepare-data
       (partition 3 1)
       (map #(apply + %))
       logic))

(def sample-input-path "resources/_2021/sample_input_p1.txt")
(def input-path "resources/_2021/input_p1.txt")

(comment
  (prepare-data sample-input-path),
  (solve-part1 sample-input-path)
  (solve-part2 sample-input-path)
  (solve-part1 input-path)
  (solve-part2 input-path))