(ns p2018_day5_reviewed
  (:require [clojure.string :as s]))

; read input

(defn read-input [path]
  "output type: string"
  (-> path
      slurp
      (s/split #"\n")
      first))

(defn upper-case? [char]
  (= (s/upper-case char) (str char)))


(defn lower-case? [char]
  (= (s/lower-case char) (str char)))

(defn remove? [char1 char2]
  (if (or (nil? char1)
          (nil? char2))
    false
    (and (or (= (upper-case? char1) (lower-case? char2))
             (= (lower-case? char1) (upper-case? char2)))
         (= (s/upper-case char1) (s/upper-case char2)))))

(defn squeeze-string [string]
  (reduce (fn [new-seq
               val]
            (if (remove? val (peek new-seq))
              (pop new-seq)
              (conj new-seq val)))
          []
          string))

; peek pop
; require 사용하기

(defn solve-part1 [path]
  (-> path
      read-input
      squeeze-string
      count))

(def sample-input-path "resources/sample_input_p5.txt")
(def input-path "resources/input_p5.txt")

(def alphabets (map char (range 97 123)))

(defn delete-units-in-string [char string]
  (-> string
      (s/replace (s/lower-case char) "")
      (s/replace (s/upper-case char) "")))

(defn removed-seq-by-unit-seq [string]
  (->> alphabets
       (map (fn [x] (delete-units-in-string x string)))))


(defn logic-part2 [string]
  (->> string
       removed-seq-by-unit-seq
       (map squeeze-string)
       (map count)
       (apply min)))

(defn solve-part2 [path]
  (->> path
       read-input
       logic-part2))

(comment
  (solve-part1 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 sample-input-path),
  (solve-part2 input-path),)

