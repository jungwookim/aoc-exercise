(ns p2020_day4
  (:require [clojure.string :as s]))

;clojure spec
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
                {:key key :value value})))))

(def required-key-set #{"ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"})

(defn part1-passport-valid? [m-seq]
  (clojure.set/subset? required-key-set (set (map #(:key %) m-seq))))

(defn part2-passport-valid? [m-seq]
  (let [{_ :key byr-val :value} (first (filter #(= (:key %) "byr") m-seq))
        {_ :key iyr-val :value} (first (filter #(= (:key %) "iyr") m-seq))
        {_ :key eyr-val :value} (first (filter #(= (:key %) "eyr") m-seq))
        {_ :key hgt-val :value} (first (filter #(= (:key %) "hgt") m-seq))
        {_ :key hcl-val :value} (first (filter #(= (:key %) "hcl") m-seq))
        {_ :key ecl-val :value} (first (filter #(= (:key %) "ecl") m-seq))
        {_ :key pid-val :value} (first (filter #(= (:key %) "pid") m-seq))]
    (let [valid-byr (and (boolean byr-val)
                         (<= 1920 (Integer/parseInt byr-val))
                         (>= 2002 (Integer/parseInt byr-val)))
          valid-iyr (and (boolean iyr-val)
                         (<= 2010 (Integer/parseInt iyr-val))
                         (>= 2020 (Integer/parseInt iyr-val)))
          valid-eyr (and (boolean eyr-val)
                         (<= 2020 (Integer/parseInt eyr-val))
                         (>= 2030 (Integer/parseInt eyr-val)))
          valid-hgt (and (boolean hgt-val)
                         (cond
                           (s/includes? hgt-val "cm") (and (<= 150 (Integer/parseInt (first (re-seq #"[0-9]+" hgt-val))))
                                                           (>= 193 (Integer/parseInt (first (re-seq #"[0-9]+" hgt-val)))))
                           (s/includes? hgt-val "in") (and (<= 59 (Integer/parseInt (first (re-seq #"[0-9]+" hgt-val))))
                                                           (>= 76 (Integer/parseInt (first (re-seq #"[0-9]+" hgt-val)))))
                           :else false))
          valid-hcl (and (boolean hcl-val)
                         (= (first hcl-val) \#)
                         (= (count hcl-val) 7)
                         (boolean (re-seq #"[a-f0-9]{6}" hcl-val)))
          valid-ecl (and (boolean ecl-val)
                         (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl-val))
          valid-pid (and (boolean pid-val)
                         (= (count pid-val) 9)
                         (int? (Integer/parseInt pid-val)))]
      (and valid-byr
           valid-iyr
           valid-eyr
           valid-hgt
           valid-hcl
           valid-ecl
           valid-pid))))

(defn logic-part1 [string-info-seq]
  (->> string-info-seq
       (map parse-string-info-to-hash-map)
       (map part1-passport-valid?)
       (filter true?)
       count))

(defn logic-part2 [string-info-seq]
  (->> string-info-seq
       (map parse-string-info-to-hash-map)
       (map part2-passport-valid?)
       (filter true?)
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
