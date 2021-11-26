(ns p2018_day7_reviewed
  (:require [clojure.string :as s]))

(def sample-input-path "resources/sample_input_p7.txt")
(def input-path "resources/input_p7.txt")

(def state {:time        -1
            :worker      {0 {:work :not-working
                             :time 0}
                          1 {:work :not-working
                             :time 0}}
            :remain-work {\A #{\C}, \B #{\A}, \C #{}, \D #{\A}, \E #{\B \D \F}, \F #{\C}}
            :done        []})

(def sample-worker-n 2)
(def full-worker-n 5)

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

; 이거는 [from to]를 hash-map의 key로 사용하기 위해서 가독성을 위해 {:from from :to to} 로 바꾸지 않기로 함
(defn extract-from-to [string-info]
  [(nth string-info 5) (nth string-info 36)])

(defn init-work-data [v-seq]
  (reduce (fn [acc x]
            (assoc acc x #{}))
          {}
          (set (flatten v-seq))))
;(init-work-data '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),

(defn init-work-data-2 [v-seq]
  (->> (for [key (set (flatten v-seq))]
         {key #{}})
       (into {})))

(defn group-by-val [v-seq]
  (reduce (fn [acc-m [from to]]
            (update acc-m to #(conj % from)))
          (init-work-data v-seq)
          v-seq))

; 근데 이렇게 하면 aggregation 하지 못한 값들에 대해서 누락된 key가 있을 수 있음. 이럴 경우에는 어떻게 하는게 나은가?
; 먼저 해야할 일을 group-by
(defn group-by-val2 [v-seq]
  (->> (group-by second v-seq)
       (map (fn [[k v]]
              [k (set (map (fn [[k2 _]] k2) v))]))
       (into {})))

(defn parsing [path]
  (->> (read-input path)
       (map extract-from-to)
       group-by-val))

(defn init-worker-state
  "n: # of worker"
  [n]
  (reduce (fn [acc-m x]
            (assoc acc-m x {:work :not-working
                            :time -1}))
          {}
          (range n)))

(defn find-can-work-workers [worker-state]
  (->> (filter (fn [[_ {:keys [_ time]}]]
                 (>= 0 time))
               worker-state)
       (map (fn [[worker _]] worker))))

(defn alphabet-to-second [a]
  (- (int a) 4))

(defn sample-alphabet-to-second [a]
  (- (int a) 64))

(defn will-remove-work [worker-state]
  (keep (fn [[_ {:keys [work time]}]]
          (when (= 0 time)
            work))
        worker-state))

(defn update-multiple-pre-work-done [will-remove-works m]
  (->> (map (fn [[work pre-work]]
              [work (clojure.set/difference pre-work (set will-remove-works))])
            m)
       (into {})))

(defn update-new-work [state matched-work]
  (reduce (fn [acc-state work]
            (update acc-state :worker #(conj % work)))
          state
          matched-work))


(defn update-worker-time [worker-state]
  (->> (keys worker-state)
       (reduce (fn [acc-worker-state key]
                 (update-in
                   acc-worker-state
                   [key :time]
                   #(dec %)))
               worker-state)))

(defn update-worker-work [worker-state]
  (->> (keys worker-state)
       (reduce (fn
                 [acc-worker-state x]
                 (update-in
                   acc-worker-state
                   [x :work]
                   #(if (zero? (get-in acc-worker-state [x :time]))
                      :not-working
                      %)))
               worker-state)))

(defn update-worker-state [state worker-state]
  (update state :worker (constantly worker-state)))

(defn update-worker [state]
  (->> (:worker state)
       update-worker-work
       update-worker-time
       (update-worker-state state)))

(defn update-remain-work [state matched-work]
  (update
    state
    :remain-work
    (constantly (->> (reduce (fn [acc-m val]
                               (dissoc acc-m (:work (first (vals val)))))
                             (:remain-work state)
                             matched-work)
                     (update-multiple-pre-work-done (will-remove-work (:worker state)))))))

(defn update-done [state]
  (->> (keys (:worker state))
       (reduce (fn [acc-state key]
                 (if (zero? (get-in acc-state [:worker key :time]))
                   (update acc-state :done #(conj % (get-in acc-state [:worker key :work])))
                   acc-state))
               state)))

(defn agg-next-work [can-work-workers next-works]
  (map (fn [worker next-work] {worker {:work next-work
                                       :time (sample-alphabet-to-second next-work)}})
       can-work-workers
       next-works))

(defn match [state matched-work]
  (->> matched-work
       (update-new-work state)))

(defn process [state]
  (let [available-workers (find-can-work-workers (:worker state))
        next-works (->> (filter (fn [[_ pre-work]] (empty? pre-work)) (:remain-work state))
                        (sort-by key)
                        (map first))
        matched-work (agg-next-work available-workers next-works)]
    (-> state
        (match matched-work)
        (update :time inc)
        update-worker
        (update-remain-work matched-work))))

(defn init-state [path n]
  {:time        -1
   :worker      (init-worker-state n)
   :remain-work (parsing path)
   :done        []})

(defn everyone-not-working [worker-state]
  (every? (fn [[_ {:keys [work _]}]] (= :not-working work)) worker-state))

(defn break-point [state]
  (and (empty? (:remain-work state))
       (everyone-not-working (:worker state))))

(defn solve-part2 [path n]
  (->> (init-state path n)
       (iterate process)
       (drop-while #(not (break-point %)))
       first
       :time))

(comment
  (init-work-data '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (init-work-data-2 '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (group-by-val '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (group-by-val2 '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (process state),
  (solve-part2 sample-input-path sample-worker-n),
  (solve-part2 input-path full-worker-n),)
