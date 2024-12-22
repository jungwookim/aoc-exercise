(ns aoc.2024.day13
  (:require [clojure.string :as str]))


(defn parse-buttons [input]
  (let [pattern #"Button (\w): X\+([\d]+), Y\+([\d]+)|Prize: X=([\d]+), Y=([\d]+)"
        matches (re-seq pattern input)]
    (into {} (map (fn [[_ label x y px py]]
                    (cond
                      label [(keyword label) [(parse-long x) (parse-long y)]]
                      px    [:P [(parse-long px) (parse-long py)]]))
                  matches))))

(def data
  (->> (-> "resources/2024/input.txt"
           slurp
           (str/split #"\n\n"))
       (map #(str/replace % "\n" " "))
       (map parse-buttons)))

(defn calc-possible-tokens
  "input: {:A [94 34], :B [22 67], :P [8400 5400]}"
  [{:keys [A B P]}]
  (let [[xa ya] A
        [xb yb] B
        [xp yp] P]
    (for [i (range 101)
          j (range 101)
          :when (and (= (+ (* i xa) (* j xb)) xp)
                     (= (+ (* i ya) (* j yb)) yp))]
      (+ (* 3 i) j))))

(defn find-min-token
  "input: {:A [94 34], :B [22 67], :P [8400 5400]}"
  [input]
  (let [possible-tokens (calc-possible-tokens input)]
    (if (empty? possible-tokens)
      0
      (apply min (calc-possible-tokens input)))))

(defn part1
  []
  (transduce (map find-min-token) + data))

(comment
  (part1)
  data
  (find-min-token {:A [94 34], :B [22 67], :P [8400 5400]}))