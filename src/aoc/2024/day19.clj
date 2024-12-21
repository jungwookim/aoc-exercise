(ns aoc.2024.day19
  (:require [clojure.string :as str]))

(def parse
  (-> "resources/2024/input.txt"
      slurp
      (str/split #"\n\n")))

(def patterns
  (-> parse
      first
      (str/split #", ")))

(def designs
  (-> parse
      second
      (str/split #"\n")))

(def possible-count
  (memoize (fn [design]
             (if (empty? design)
               1
               (->> patterns
                    (filter #(str/starts-with? design %))
                    (map #(possible-count (subs design (count %))))
                    (reduce +))))))

(defn part1
  []
  (->> designs
       (map possible-count)
       (filter pos?)
       count))

(defn part2
  []
  (->> designs
       (map possible-count)
       (reduce +)))

(comment
  ;; 처음에 이렇게 했는데, 너무 nested seq가 많아져서 메모리 사용량이 높아져서 문제가 생김.
  ;; memo도 통하지 않게 됨.
;;   (if (empty? design)
;;     true
;;     (->> patterns
;;          (filter #(str/starts-with? design %))
;;          (map #(possible-count (subs design (count %))))))
  designs
  patterns
  (part1)
  (part2)
  (possible-count "brwrr")
  )