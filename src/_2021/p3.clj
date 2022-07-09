(ns _2021.p3
  (:require [clojure.string :as s]))

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn transpose [xs]
  (apply map list xs))

(defn prepare-data [path]
  (fn [f]
    (->> path
         read-input
         transpose
         (map frequencies)
         (map #(apply f val %))
         keys)))

(defn binary->decimal [binary]
  (Integer/parseInt binary 2))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn get-epsilon-rate [n gamma-rate]
  (- (dec (exp 2 (count n))) gamma-rate))


(defn solve-part1 [path]
  (let [prep         (->> ((prepare-data path) max-key)
                          (apply str))
        gamma-rate   (binary->decimal prep)
        epsilon-rate (get-epsilon-rate prep gamma-rate)]
    (* epsilon-rate gamma-rate)))


(defn calc-count [coll n]
  (let [count1 (->> (map (fn [s]
                           (if (= \1 (nth s n))
                             1
                             0)) coll)
                    (apply +))
        count2 (->> (map (fn [s]
                           (if (= \0 (nth s n))
                             1
                             0)) coll)
                    (apply +))]
    {:count1 count1
     :count2 count2}))


(defn oxygen-generator-rating
  [data filters]
  (->> (range 0 5)
       ((partial iterate data))
       (take 5)))

(defn solve-part2 [path]
  (let [oxygen-filter (->> ((prepare-data path) max-key)
                           (apply str))
        data (-> path
                 read-input)
        oxygen-generator-rating (oxygen-generator-rating data oxygen-filter)]))

(def sample-input-path "resources/_2021/sample_input_p3.txt")
(def input-path "resources/_2021/input_p3.txt")

(comment
  (oxygen-generator-rating (-> sample-input-path
                               read-input) {})
  ((prepare-data sample-input-path) max-key)
  ((prepare-data sample-input-path) min-key)
  (solve-part1 sample-input-path)
  (solve-part2 sample-input-path)
  (solve-part1 input-path)
  (solve-part2 input-path))
