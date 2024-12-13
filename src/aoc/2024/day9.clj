(ns aoc.2024.day9
  (:require [clojure.string :as str]))

(defn parse [path]
  (->> (slurp path)))

(defn ->number [char]
  (Integer/parseInt (str char)))

(defn ->blocks [input]
  (map-indexed (fn [i x]
                 (if (even? i)
                   {:id (quot i 2) :size (->number x)}
                   {:size (->number x)})) input))


(defn process [parsed-blocks]
  (loop [remaining parsed-blocks
         blocks []]
    (let [fblock (first remaining)
          lblock (last remaining)]
      (cond
        (nil? fblock) blocks

        (zero? (:size fblock)) (do
                                 (recur (rest remaining) blocks))

        (:id fblock) (do
                       (recur (rest remaining) (conj blocks fblock)))

        (or (zero? (:size lblock))
            (not (:id lblock))) (do
                                  (recur (butlast remaining) blocks))

        :else (let [move (min (:size fblock) (:size lblock))]
                (recur (concat [(update fblock :size #(- % move))]
                               ((comp butlast rest) remaining)
                               [(update lblock :size #(- % move))])
                       (conj blocks (assoc lblock :size move))))))))

(defn check-sum [blocks]
  (reduce (fn [{:keys [sum start]} {:keys [id size]}]
            (let [end (+ start size)
                  r (range start end)]
              {:sum (+ sum (apply + (map #(* id %) r))) :start end})) {:sum 0 :start 0} blocks))

(defn part1 [path]
  (let [blocks (->> path
                    parse
                    ->blocks)]
    (->> blocks
         process
         check-sum
         :sum)))

(comment
  (apply + (map (fn [x] (* 2 x)) (range 0 4)))
  (parse "resources/2024/input.txt")
  (->> "resources/2024/input.txt"
       parse
       ->blocks)

  (process (->> "resources/2024/input.txt"
                parse
                ->blocks))

  (part1 "resources/2024/input.txt"))
