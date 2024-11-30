(ns util.core
  (:require [clojure.string :as s]))

(defn read-input [path]
  (-> path
      (slurp)
      (s/split-lines)))

(comment
  (read-input "resources/2024/day1_part1.txt"))
