(ns _2020.day8
  (:require [clojure.string :as s]))

; vector로 index 접근을 계속 하도록 하자
; visited vector 를 관리해서 이미 방문한 인덱스가 발견되면 loop를 종료하고 acc 값을 출력하자

(def sample-input-path "resources/sample_input_p2020_day8.txt")
(def input-path "resources/input_p2020_day8.txt")


(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn parsing [path]
  (->> (read-input path)
       (map #(s/split % #" "))
       (map (fn [[k v]] [k (Integer/parseInt v)]))
       (into [])))

(def init-state
  {:acc-val 0
   :visited []
   :idx     0
   :break1? false
   :break2? false})

; processing의 결과는 현재 인덱스와 현재 visited set, acc-val을 관리하도록 하자
; processing을 한번 거치면 update val, update idx, update visited vector를 하자

(defn update-in-case-of-nop [state idx]
  (-> state
      (update :visited #(conj % idx))
      (update :idx inc)))

(defn update-in-case-of-acc [state idx value]
  (-> state
      (update :acc-val #(+ % value))
      (update :visited #(conj % idx))
      (update :idx inc)))

(defn update-in-case-of-jmp [state idx value]
  (-> state
      (update :visited #(conj % idx))
      (update :idx #(+ % value))))

; 반대로 하자
(defn index-out-of-bounds [v-size idx]
  (or (< (dec v-size) idx)
      (neg? idx)))

; 반대로 하자
(defn break-point-part1? [state]
  (->> (frequencies (:visited state))
       (some (fn [[_ v]] (> v 1)))))

(defn update-state-break1 [state]
  (update
    state
    :break1?
    (constantly (break-point-part1? state))))

(defn update-state-break2 [state data-size]
  (update
    state
    :break2?
    (constantly (index-out-of-bounds data-size (:idx state)))))

(defn processing
  "state: [[\"nop\" \"+0\"] [\"acc\" \"+1\"] ... ]\n"
  [data {:keys [idx] :as state}]
  (let [[action value] (nth data idx)]
    (-> (case action
          "nop" (update-in-case-of-nop state idx)
          "acc" (update-in-case-of-acc state idx value)
          "jmp" (update-in-case-of-jmp state idx value))
        (update-state-break1)
        (update-state-break2 (count data)))))

(defn agg-ans [data state]
  (- (:acc-val state) (->> (:visited state)
                           peek
                           (nth data)
                           second)))

(defn solve-part1 [path]
  (let [data (parsing path)]
    (->> init-state
         (iterate #(processing data %))
         (drop-while #(not (:break1? %)))
         first
         (agg-ans data))))

; defnmulti로 변경해보기

(defn generate-candidate-data [data]
  (->> data
       (map-indexed (fn [idx [action value]]
                      (case action
                        "nop" (update data idx (constantly ["jmp" value])) ; assoc data idx "jmp" value
                        "jmp" (update data idx (constantly ["nop" value]))
                        "acc" nil)))
       (remove nil?)))

(defn logic-part2 [data]
    (->> init-state
         (iterate #(processing data %))
         (drop-while #(and (not (:break2? %))
                          (not (:break1? %))))
         first))

; take-while을 쓰면 좋을 수도 있음

(defn solve-part2 [path]
  (->> (parsing path)
       generate-candidate-data
       (map logic-part2)
       (filter :break2?)
       first
       :acc-val))

(comment
  (parsing sample-input-path),
  init-state,
  (let [data (parsing sample-input-path)]
    (->> init-state
         (iterate #(processing data %))
         (drop-while break-point-part1?)
         first
         (agg-ans data))),
  (logic-part2 (parsing sample-input-path)),
  (solve-part1 sample-input-path),
  (solve-part2 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 input-path),)