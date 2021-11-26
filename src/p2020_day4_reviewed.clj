(ns p2020_day4_reviewed
  (:require [clojure.string :as s]))

;clojure spec

(def required-fields-map {:ecl "ecl"
                          :pid "pid"
                          :eyr "eyr"
                          :hcl "hcl"
                          :byr "byr"
                          :iyr "iyr"
                          :hgt "hgt"})

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

(defn part1-passport-valid? [m-seq]
  (clojure.set/subset? (vals required-fields-map) (set (map #(:key %) m-seq))))

(defn filter-matching-fields [m-seq field]
  (->> m-seq
       (filter #(= (:key %) field))
       first))

(defn valid-between-numbers [string-n n1 n2]
  (and (boolean string-n)
       (<= n1 (Integer/parseInt string-n))
       (>= n2 (Integer/parseInt string-n))))

(defn part2-passport-valid? [m-seq]
  (let [{_ :key byr-val :value} (filter-matching-fields m-seq "byr")
        {_ :key iyr-val :value} (filter-matching-fields m-seq "iyr")
        {_ :key eyr-val :value} (filter-matching-fields m-seq "eyr")
        {_ :key hgt-val :value} (filter-matching-fields m-seq "hgt")
        {_ :key hcl-val :value} (filter-matching-fields m-seq "hcl")
        {_ :key ecl-val :value} (filter-matching-fields m-seq "ecl")
        {_ :key pid-val :value} (filter-matching-fields m-seq "pid")]
    (let [valid-byr (valid-between-numbers byr-val 1920 2002)
          valid-iyr (valid-between-numbers iyr-val 2010 2020)
          valid-eyr (valid-between-numbers eyr-val 2020 2030)
          valid-hgt (and (boolean hgt-val)
                         (let [hgt (first (re-seq #"[0-9]+" hgt-val))]
                           (cond
                             (s/includes? hgt-val "cm") (valid-between-numbers hgt 150 193)
                             (s/includes? hgt-val "in") (valid-between-numbers hgt 59 76)
                             :else false)))
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
