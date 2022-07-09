(ns _2018.p2018-day4-reviewed
  (:require [clojure.string :as s]))

; read input and parsing
(defn read-input [path]
  (-> path
      slurp
      (s/split #"\n")))

(defn extract-timestamp [string-info]
  (last (last (re-seq #"\[(.*?)\]" string-info))))

(defn extract-minute [string-info]
  (-> (nth (into [] (re-seq #"[0-9]+" string-info)) 4)
      (Integer/parseInt)))

(defn extract-guard-id [string-info]
  (last (last (re-seq #"#([0-9]+)" string-info))))

(defn ->element [action timestamp minute who]
  {:action    action
   :timestamp timestamp
   :minute    minute
   :who       who})

(defn reverse-includes? [str1 str2]
  (s/includes? str2 str1))

(defn prepare-data [path]
  (->> path
       read-input
       (map (fn [x]
              (let [timestamp (extract-timestamp x)
                    minute (extract-minute x)]
                (condp reverse-includes? x
                  "wakes" (->element :wake-up timestamp minute nil)
                  "falls" (->element :fall-asleep timestamp minute nil)
                  "shift" (->element :shift timestamp minute (extract-guard-id x))))))
       (sort-by :timestamp)))


(defn get-all-guard-ids [m-seq]
  (->> m-seq
       (keep (fn [x] (get x :who)))
       set))

(defn init-minutes-map []
  (zipmap (map #(keyword (str %)) (range 60)) (repeat 0)))

(defn init-sleep-data [m-seq]
  (let [minutes-map (init-minutes-map)]
    (->> m-seq
         get-all-guard-ids
         (reduce (fn [acc guard-id]
                   (assoc acc (keyword guard-id) minutes-map))
                 {}))))

(defn init-guard-working-time [m-seq]
  (->> m-seq
       get-all-guard-ids
       (reduce (fn [acc guard-id]
                 (assoc acc (keyword guard-id) []))
               {})))

; to improve
(defn guard-minutes-each-minutes
  "input: {:99 [40 41]}, output: ({:99 40} {:99 41})"
  [m-seq]
  (mapcat (fn [m]
            (let [guard-id (first (keys m))
                  minutes (first (vals m))]
              (map (fn [min]
                     {(keyword guard-id) min})
                   minutes)))
          m-seq))


(defn guard-working-all-minutes [data]
  (mapcat (fn [[guard-id times]]
            (let [x (into [] (partition 2 times))]
              (map (fn [[start end]] {(keyword guard-id) (range start end)}) x))) data))

(defn interpolate-data [m-seq]
  (->> (reduce (fn [{current-guard :current-guard
                     :as           m}
                    {minute :minute
                     who    :who}]
                 (if who
                   (update m :current-guard (constantly who))
                   (update-in m [:acc-data (keyword current-guard)] #(conj % minute))))
               {:acc-data      (init-guard-working-time m-seq)
                :current-guard nil}
               m-seq)
       :acc-data
       guard-working-all-minutes
       guard-minutes-each-minutes))

(defn update-data [data val]
  (let [guard-id (keyword (first (keys val)))
        minute (keyword (str (first (vals val))))]
    (update-in data
               [guard-id minute]
               inc)))

(defn common-logic [path]
  (let [prepared-data (->> path
                           prepare-data
                           interpolate-data)
        init-data (->> path
                       prepare-data
                       init-sleep-data)]
    (reduce (fn [acc-data
                 val]
              (update-data acc-data val))
            init-data
            prepared-data)))

(defn total-sleep-time [minutes]
  (apply + minutes))

(defn get-most-sleep-minute [m]
  (apply max-key val m))

(defn solve-part1 [path]
  (->> path
       common-logic
       (map (fn [x] {:guard-id              (name (first x))
                     :total-sleep-time      (total-sleep-time (vals (second x)))
                     :get-most-sleep-minute (get-most-sleep-minute (second x))}))
       (apply max-key :total-sleep-time)
       (apply (fn [x _ z] (* (Integer/parseInt (second x))
                             (Integer/parseInt (name (first (second z)))))))))


(defn solve-part2 [path]
  (->> path
       common-logic
       (map (fn [x] {:guard-id              (name (first x))
                     :total-sleep-time      (total-sleep-time (vals (second x)))
                     :get-most-sleep-minute (get-most-sleep-minute (second x))}))
       (apply max-key #(second (:get-most-sleep-minute %)))
       (apply (fn [x _ z] (* (Integer/parseInt (second x))
                             (Integer/parseInt (name (first (second z)))))))))

(def sample-input-path "resources/sample_input_p4.txt")
(def input-path "resources/input_p4.txt")

(comment
  (prepare-data sample-input-path),
  (interpolate-data (prepare-data sample-input-path))
  (solve-part1 sample-input-path),
  (solve-part2 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 input-path),)
