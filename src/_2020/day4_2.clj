(ns _2020.day4-2
  (:require [clojure.string :as s]
            [clojure.spec.alpha :as sp]))


(defn read-input [path]
  (-> path
      slurp
      (s/split #"\n\n")))

(defn prepare-data [path]
  (->> path
       read-input
       (map (fn [x]
              (s/replace x "\n" " ")))))

(defn parse-string-info-to-hash-map [string-info]
  (->> (s/split string-info #" ")
       (map (fn [x]
              (let [[key value] (s/split x #":")]
                {(keyword key) value})))
       (into {})))

(defn valid-between-numbers [string-n n1 n2]
  (and (boolean string-n)
       (>= n2 (Integer/parseInt string-n) n1)))

(sp/def ::byr (fn [x] (valid-between-numbers x 1920 2002)))
(sp/def ::iyr (fn [x] (valid-between-numbers x 2010 2020)))
(sp/def ::eyr (fn [x] (valid-between-numbers x 2020 2030)))

(sp/def ::hgt (fn [x] (and (boolean x)
                           (let [hgt (first (re-seq #"[0-9]+" x))]
                             (cond
                               (s/includes? x "cm") (valid-between-numbers hgt 150 193)
                               (s/includes? x "in") (valid-between-numbers hgt 59 76)
                               :else false)))))
(sp/def ::hcl (fn [x] (and (boolean x)
                           (= (first x) \#)
                           (= (count x) 7)
                           (boolean (re-seq #"[a-f0-9]{6}" x)))))
(sp/def ::ecl (fn [x] (and (boolean x)
                           (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} x))))
(sp/def ::pid (fn [x] (and (boolean x)
                           (= (count x) 9)
                           (int? (Integer/parseInt x)))))


(sp/def ::valid (sp/keys :req-un [::byr ::iyr ::eyr ::hgt
                                  ::hcl ::ecl ::pid]
                         :opt-un [::cid]))


(def required-key-set #{:ecl :pid :eyr :hcl :byr :iyr :hgt})

(defn valid1? [m]
  (clojure.set/subset? required-key-set (set (keys m))))

(defn valid? [m]
  (sp/valid? ::valid m))


(defn logic-part1 [string-info-seq]
  (->> string-info-seq
       (map parse-string-info-to-hash-map)
       (filter valid1?)
       count))

(defn logic-part2 [string-info-seq]
  (->> string-info-seq
       (map parse-string-info-to-hash-map)
       (filter valid?)
       count))

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1))

(defn solve-part2 [path]
  (->> path
       prepare-data
       logic-part2))

(def sample-input-path "resources/sample_input_p2020_day4.txt")
(def input-path "resources/input_p2020_day4.txt")

(comment
  (solve-part1 sample-input-path),
  (solve-part2 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 input-path),)
