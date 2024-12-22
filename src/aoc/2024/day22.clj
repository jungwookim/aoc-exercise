(ns aoc.2024.day22)

(def prune-num 16777216)
(defn pruning
  [num]
  (rem num prune-num))

(defn step1
  [num]
  (-> num
      (* 64)
      (bit-xor num)
      (pruning)))

(defn step2
  [num]
  (-> num
      (/ 32)
      (Math/floor)
      int
      (bit-xor num)
      (pruning)))

(defn step3
  [num]
  (-> num
      (* 2048)
      (bit-xor num)
      (pruning)))

(defn next-pnum
  [num]
  (-> num
      step1
      step2
      step3))

(def data
  (->> "resources/2024/input.txt"
       slurp
       (clojure.string/split-lines)
       (map parse-long)))

(defn part1
  []
  (transduce (map #(nth (iterate next-pnum %) 2000)) + data))

