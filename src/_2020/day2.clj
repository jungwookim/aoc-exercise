(ns _2020.day2
  (:require [clojure.string :as s]))

(def sample-input-path "resources/_2020/sample_input_day2.txt")
(def input-path "resources/_2020/input_day2.txt")

(defn read-input
  [path]
  (-> path
      slurp
      (s/split #"\n")))

(defn prep-data
  [item]
  (let [[num1 num2 target-str pwd] (re-seq #"\w+" item)
        num1 (Integer/parseInt num1)
        num2 (Integer/parseInt num2)
        target-char (first (seq target-str))]
    {:num1 num1
     :num2 num2
     :target-char target-char
     :pwd pwd}))

(defn logic-part-1
  "process for single item"
  [item]
  (let [{:keys [num1 num2 target-char pwd]} (prep-data item)]
    (<= num1 (get (frequencies pwd) target-char 0) num2)))

(defn solve-part-1
  [path]
  (->> path
       read-input
       (map logic-part-1)
       (filter true?)
       count))

(defn logic-part-2
  [item]
  (let [{:keys [num1 num2 target-char pwd]} (prep-data item)
        on-position1? (= (nth pwd (dec num1)) target-char)
        on-position2? (= (nth pwd (dec num2)) target-char)]
    (when (or
           (and (true? on-position1?)
                (false? on-position2?))
           (and (false? on-position1?)
                (true? on-position2?)))
      1)))

(defn solve-part-2
  [path]
  (->> path
       read-input
       (keep logic-part-2)
       (apply +)))

(comment
  (seq "a")
  (frequencies "abcde")
  (logic-part-1 "1-3 b: cdefg")
  (re-seq #"\w+" "1-3 a: abcde") ; we need to get sequences
  (read-input sample-input-path)
  (solve-part-1 input-path)
  
  
  ;; part2
  (solve-part-2 sample-input-path)
  (solve-part-2 input-path))
