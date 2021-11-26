(ns p2020-day1)

; clojure.math/permutations 같은게 있음

(defn read-input [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn prepare-data [path]
  (->> path
       read-input
       (map (fn [x] (Integer/parseInt x)))))

(defn comb-seq-2 [int-seq]
  (for [x1 int-seq
        x2 int-seq
        :when (= (+ x1 x2) 2020)]
    (* x1 x2)))

(defn comb-seq-3 [int-seq]
  (for [x1 int-seq
        x2 int-seq
        x3 int-seq
        :when (= (apply + [x1 x2 x3]) 2020)]
    (apply * [x1 x2 x3])))

(defn logic-part1 [int-seq]
  (->> int-seq
       comb-seq-2
       set
       first))

(defn logic-part2 [int-seq]
  (->> int-seq
       comb-seq-3
       set
       first))

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1))

(defn solve-part2 [path]
  (->> path
       prepare-data
       logic-part2))

(def sample_input "resources/sample_input_p2020_day1.txt")
(def full_input "resources/input_p2020_day1.txt")
(comment
  (solve-part1 sample_input),
  (solve-part2 sample_input),
  (solve-part1 full_input)
  (solve-part2 full_input))
