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

(defn rem-10
  [num]
  (rem num 10))

(defn seq-to-price-map
  [data]
  (->> (iterate next-pnum data)
       (take 2001)
       (map rem-10)
       (partition 2 1)
       (map (fn [[a b]] {:delta (- b a)
                         :price b}))
       (partition 4 1)
       (reduce (fn [acc [info1 info2 info3 info4]]
                 (update acc [(info1 :delta) (info2 :delta) (info3 :delta) (info4 :delta)] #(or %1 %2) (info4 :price)))
               {})))

(defn part2
  []
  (->> data
       (map seq-to-price-map)
       (apply merge-with +)
       vals
       (apply max)))

(comment
  data
  (part1)
  (part2))
