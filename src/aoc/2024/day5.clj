(ns aoc.2024.day5
  (:require [util.core :as u]
            [clojure.string :as s]))

(defn parse []
  (->> "resources/2024/input.txt"
       (u/read-input)
       (reduce (fn [[rules pages] line]
                 (cond
                   (= "" line) [rules pages]
                   (s/includes? line "|") [(cons (s/split line #"\|") rules) pages]
                   :else [rules (cons (s/split line #"\,") pages)])) [[] []])))

(defn page-info [page]
  (->> page
       (map-indexed (fn [i x]
                      {x (set (take i page))}))
       (reduce (fn [{:keys [head rest]} x]
                 (let [key (first (keys x))
                       value (first (vals x))]
                   (if (empty? value)
                     {:head key :rest rest}
                     {:head head :rest (apply conj rest (map #(vector key %1) value))})))
               {:head nil :rest []})))

(defn broken? [rules rest]
  (some (fn [info]
          (some (fn [rule] (= rule info)) rules)) rest))

(defn ordered? [rules page]
  (let [{:keys [head rest]} (page-info page)]
    (not (broken? rules rest))))


(defn middle-value
  [list]
  (nth list (quot (count list) 2)))

(defn part1 []
  (let [[rules pages] (parse)]
    (->> pages
         (filter #(ordered? rules %1))
         (map middle-value)
         (map #(Integer/parseInt %))
         (apply +))))

(defn compare-pages [rules x y]
  (cond
    ;; x가 y 앞에 와야 한다면 -1 반환
    (some #(= [x y] %) rules) -1
    ;; y가 x 앞에 와야 한다면 1 반환
    (some #(= [y x] %) rules) 1
    ;; 우선순위가 없다면 0 반환
    :else 0))

(defn order [rules page]
  (sort #(compare-pages rules %1 %2) page))

(defn part2 []
  (let [[rules pages] (parse)]
    (->> pages
         (filter #((complement ordered?) rules %1))
         (map #(order rules %1))
         (map middle-value)
         (map #(Integer/parseInt %))
         (apply +))))

(comment


  (part1)
  (part2)
  )