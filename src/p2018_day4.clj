(ns p2018_day4
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
       (map (fn [guard-id]
              (map (fn [x]
                     {(keyword guard-id) {(keyword (str x)) 0}})
                   (range 60))))
       flatten))

(defn init-guard-working-time [m-seq]
  (->> m-seq
       get-all-guard-ids
       (reduce (fn [acc guard-id]
                 (assoc acc (keyword guard-id) []))
               {})))


(defn interpolate-data [m-seq]
  (->> (reduce (fn [{acc-data      :acc-data
                     current-guard :current-guard}
                    {action :action
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
       :acc-data))


(defn update-data [data who min]
  {:acc-data      (assoc-in data
                            [(keyword who) (keyword min)]
                            (inc (get-in
                                   data
                                   [(keyword who) (keyword min)])))
   :current-guard who})

(defn solve-part1 [path]
  (let [prepared-data (->> path
                           prepare-data)]
    (let [init-data (->> prepared-data
                         init-sleep-data)]
      (reduce (fn [{acc-data      :acc-data
                    current-guard :current-guard}
                   {action :action
                    min    :min
                    who    :who}]
                (if (nil? who)
                  (update-data acc-data current-guard min)
                  (update-data acc-data who min)))
              {:acc-data init-data :current-guard nil}
              prepared-data))))


(def sample-input-path "resources/sample_input_p4.txt")
(def input-path "resources/input_p4.txt")

(comment
  (prepare-data sample-input-path),
  (interpolate-data (prepare-data sample-input-path)),
  (solve-part1 sample-input-path),)
; part 1
