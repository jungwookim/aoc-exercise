(ns _2018.p2018-day4
  (:require [clojure.string :as str]))

; 0분 1분 ... 59분을 key로 하는 map을 관리한다. vals의 합이 결국 제일 오래 잔 사람
; {:kim {:1 0, :2 0, ...}, ...} part1: sum of vals by each person * max of vals of that person
; part2: 0분끼리 1분끼리 비교해서 가장 많이 잠들었던 가드 1명을 찾아서 가드 넘버 * 가장 많이 잠들었던 분

; read input and parsing
(defn read-input [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn extract-timestamp [string-info]
  (last (last (re-seq #"\[(.*?)\]" string-info))))

(defn extract-minute [string-info]
  (-> (nth (into [] (re-seq #"[0-9]+" string-info)) 4)
      (Integer/parseInt)))

(defn extract-guard-id [string-info]
  (last (last (re-seq #"#([0-9]+)" string-info))))

(defn prepare-data [path]
  (->> path
       read-input
       (map (fn [x]
              (cond
                (str/includes? x "wakes") {:action    "wake-up"
                                           :timestamp (extract-timestamp x)
                                           :min       (extract-minute x)}
                (str/includes? x "falls") {:action    "fall-asleep"
                                           :timestamp (extract-timestamp x)
                                           :min       (extract-minute x)}
                (str/includes? x "shift") {:action    "shift"
                                           :timestamp (extract-timestamp x)
                                           :min       (extract-minute x)
                                           :who       (extract-guard-id x)})))
       (sort-by :timestamp)))


(defn get-all-guard-ids [m-seq]
  (->> m-seq
       (map (fn [x] (get x :who)))
       (remove nil?)
       set))

(defn init-sleep-data [m-seq]
  (->> m-seq
       get-all-guard-ids
       (reduce (fn [acc guard-id]
                 (assoc acc (keyword guard-id) (reduce (fn [acc x]
                                                         (assoc acc (keyword (str x)) 0))
                                                       {}
                                                       (range 60))))
               {})))

(defn init-guard-working-time [m-seq]
  (->> m-seq
       get-all-guard-ids
       (reduce (fn [acc guard-id]
                 (assoc acc (keyword guard-id) []))
               {})))

(defn guard-mins-flatten
  "input: {:99 [40 41 42 43 44 45 46 47 48 49]}"
  [m]
  (let [guard-id (first (keys m))
        minutes (first (vals m))]
    (map (fn [min]
           {(keyword guard-id) min})
         minutes)))

(defn interpolate-data [m-seq]
  (->> (reduce (fn [{acc-data      :acc-data
                     current-guard :current-guard}
                    {_ :action
                     min    :min
                     who    :who}]
                 (if (nil? who)
                   {:acc-data      (assoc-in acc-data [(keyword current-guard)] (conj (get acc-data (keyword current-guard)) min))
                    :current-guard current-guard}
                   {:acc-data      acc-data
                    :current-guard who}))
               {:acc-data      (init-guard-working-time m-seq)
                :current-guard nil}
               m-seq)
       :acc-data
       (map (fn [[guard-id times]]
              (let [x (into [] (partition 2 times))]
                (map (fn [[start end]] {(keyword guard-id) (range start end)}) x))))
       flatten
       (map (fn [x] (guard-mins-flatten x)))
       flatten))

(defn update-data [data val]
      (let [guard-id (keyword (first (keys val)))
            min (keyword (str (first (vals val))))]
        (assoc-in data
                  [guard-id min]
                  (inc (get-in
                         data
                         [guard-id min])))))


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

(defn get-most-sleep-min [m]
  (apply max-key val m))

(defn solve-part1 [path]
 (->> path
      common-logic
      (map (fn [x] {:guard-id (name (first x))
                    :total-sleep-time (total-sleep-time (vals (second x)))
                    :get-most-sleep-min (get-most-sleep-min (second x))}))
      (apply max-key :total-sleep-time)
      (apply (fn [x _ z] (* (Integer/parseInt (second x))
                            (Integer/parseInt (name (first (second z)))))))))


(defn solve-part2 [path]
  (->> path
       common-logic
       (map (fn [x] {:guard-id (name (first x))
                     :total-sleep-time (total-sleep-time (vals (second x)))
                     :get-most-sleep-min (get-most-sleep-min (second x))}))
       (apply max-key #(second (:get-most-sleep-min %)))
       (apply (fn [x _ z] (* (Integer/parseInt (second x))
                             (Integer/parseInt (name (first (second z)))))))))

(def sample-input-path "resources/sample_input_p4.txt")
(def input-path "resources/input_p4.txt")

(comment
  (solve-part1 sample-input-path),
  (solve-part2 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 input-path),)
