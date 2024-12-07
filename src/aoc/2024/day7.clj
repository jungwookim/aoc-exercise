(ns aoc.2024.day7
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn parse [path]
  (->> path
       (u/read-input)))

(defn prepare [path]
  (->> path
       (parse)
       (map (fn [x] (s/split x #": ")))
       (map (fn [[k v]] [(parse-long k) (map #(parse-long %) (s/split v #" "))]))))

(defn process [repr ops]
  (reduce (fn [{:keys [idx candi] :as state} val]
            (if (empty? candi)
              {:idx (inc idx) :candi (conj candi val)}
              {:idx (inc idx) :candi (->> candi
                                          (map (fn [x] (map #(% x val) ops)))
                                          flatten)})) {:idx 0 :candi []} repr))

(defn calc [line process-fn ops]
  (let [[goal repr] line
        expected-results ((process-fn repr ops) :candi)]
    {:result (some #(= %1 goal) expected-results)
     :goal goal
     :expected-results expected-results}))

(defn part1 [path]
  (->> path
       prepare
       (map #(calc % process [+ *]))
       (filter #(true? (:result %)))
       (map :goal)
       (reduce +)))

(defn part2 [path]
  (->> path
       prepare
       (map #(calc % process [+ * (comp parse-long str)]))
       (filter #(true? (:result %)))
       (map :goal)
       (reduce +)))

(comment
  (def path "resources/2024/input.txt")
  (part1 path)
  (part2 path)
  )
