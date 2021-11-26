(ns p2018_day7
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn extract-from-to [string-info]
  [(nth string-info 5) (nth string-info 36)])

(defn init-work-data [v-seq]
  (reduce (fn [acc x]
            (assoc acc x #{}))
          {}
          (set (flatten v-seq))))

(defn group-by-val [v-seq]
  (reduce (fn [acc-m [from to]]
            (update acc-m to #(conj % from)))
          (init-work-data v-seq)
          v-seq))

(defn group-by-val2 [v-seq]
  (->> (group-by second v-seq)
       (map (fn [[k v]]
              [k (set (map (fn [[k2 _]] k2) v))]))
       (into {})))
; group-by 를 사용해보고 [from to]보다는 {:from from :to to}를 사용해보자

(defn parsing [path]
  (->> (read-input path)
       (map extract-from-to)
       group-by-val))

(defn find-next-work
  "input: {\\A #{\\C}, \\B #{\\A}, \\D #{}, \\C #{}, \\E #{\\B \\D \\F}, \\F #{\\C}}"
  [m]
  (->> (filter (fn [[_ pre-work]] (empty? pre-work)) m)
       (sort-by key)
       ffirst))

(defn update-pre-work-done [next-work m]
  (->> (map (fn [[work pre-work]]
              [work (remove (fn [x] (= x next-work)) pre-work)])
            m)
       (into {})))

(defn processing-part1
  "input: {\\A #{\\C}, \\B #{\\A}, \\D #{}, \\C #{}, \\E #{\\B \\D \\F}, \\F #{\\C}}"
  [m]
  (:work-plan (reduce (fn [{:keys [acc-m work-plan]} _]     ; loop recur가 낫지 않았을까? 하는 의견
                        (let [next-work (find-next-work acc-m)]
                          {:work-plan (conj work-plan next-work)
                           :acc-m     (->> (dissoc acc-m next-work)
                                           (update-pre-work-done next-work))}))
                      {:acc-m     m
                       :work-plan []}
                      m)))

;history... time...
;(defn process [state] ;1 sec state update
;  (-> state
;      (update :time inc)
;      (update :history update-history)
;      (update :available-worker update-worker)))
;

;(->> init-data
;     (iterate process)
;     (drop-while part2-condition)
;     first
;     :history)

(defn aggregating-part1 [char-seq]
  (s/join "" char-seq))

(defn solve-part1 [path]
  (->> (parsing path)
       processing-part1
       aggregating-part1))

(defn init-worker-state
  "n: # of worker"
  [n]
  (reduce (fn [acc-m x]
            (assoc acc-m x {:work :not-working
                            :time :not-working}))
          {}
          (range n)))

(defn find-can-work-workers [worker-state]
  (->> (filter (fn [[_ {:keys [work _]}]]
                 (= :not-working work))
               worker-state)
       (map (fn [[worker _]] worker))))

(defn alphabet-to-second [a]
  (- (int a) 4))

(defn sample-alphabet-to-second [a]
  (- (int a) 64))

(defn matching-next-work-for-workers [m worker-state]
  (let [can-work-workers (find-can-work-workers worker-state)
        next-works (->> (filter (fn [[_ pre-work]] (empty? pre-work)) m)
                        (sort-by key)
                        (map first))]
    (map (fn [worker next-work] {worker {:work next-work
                                         :time (sample-alphabet-to-second next-work)}})
         can-work-workers
         next-works)))

(defn will-remove-work [worker-state]
  (keep (fn [[_ {:keys [work time]}]]
          (when
            (and (not= :not-working time)
                 (zero? time))
            work))
        worker-state))


(defn update-multiple-pre-work-done [will-remove-works m]
  (->> (map (fn [[work pre-work]]
              [work (clojure.set/difference pre-work (set will-remove-works))])
            m)
       (into {})))

(defn update-work-plan [m matched-work will-remove-works]
  (->> (reduce (fn [acc-m val]
                 (dissoc acc-m (:work (first (vals val)))))
               m
               matched-work)
       (update-multiple-pre-work-done will-remove-works)))

(defn update-new-work [worker-state matched-work]
  (reduce (fn [acc-work work]
            (conj acc-work work))
          worker-state
          matched-work))

(defn update-old-work [worker-state]
  (->> (map (fn [[worker {:keys [work time]} :as x]]
              (cond
                (= :not-working time) x
                (zero? time) [worker {:work :not-working
                                      :time :not-working}]
                :else [worker {:work work
                               :time (dec time)}]))
            worker-state)
       (into {})))

(defn update-worker-state [worker-state matched-work]
  (prn worker-state matched-work)
  (->> (update-new-work worker-state matched-work)))

(defn everyone-not-working [worker-state]
  (every? (fn [[k {:keys [work _]}]] (= :not-working work)) worker-state))

(defn processing-part2
  "n: # of workers, m: {\\A #{\\C}, \\B #{\\A}, \\D #{}, \\C #{}, \\E #{\\B \\D \\F}, \\F #{\\C}}"
  [n m]
  (reduce (fn [{:keys [remain-work time worker-state] :as x} _]
            (let [matched-work (matching-next-work-for-workers remain-work worker-state)
                  updated-old-work (update-old-work worker-state)
                  will-remove-works (will-remove-work (into [] worker-state))]
              (if (and (empty? remain-work)
                       (everyone-not-working worker-state))
                (reduced x)
                {:remain-work  (update-work-plan remain-work matched-work will-remove-works)
                 :worker-state (update-worker-state updated-old-work matched-work)
                 :time         (inc time)})))
          {:remain-work  m
           :worker-state (init-worker-state n)
           :time         -1}
          (range (* 26 90))))

(def sample-worker-n 2)
(def full-worker-n 5)

(defn solve-part2 [path n]
  (->> (parsing path)
       (processing-part2 n)))

(def sample-input-path "resources/sample_input_p7.txt")
(def input-path "resources/input_p7.txt")

(comment
  (init-work-data '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (group-by-val '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (group-by-val2 '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (find-next-work {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}}),
  (update-pre-work-done \C {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}}),
  (parsing sample-input-path),
  (processing-part1 {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}}),
  (aggregating-part1 [\C \A \B \D \F \E]),
  (init-worker-state 5),
  (find-can-work-workers {0 {:work \A, :time 5},
                          1 {:work :not-working, :time :not-working},
                          2 {:work :not-working, :time :not-working},
                          3 {:work :not-working, :time :not-working},
                          4 {:work :not-working, :time :not-working}}),
  (matching-next-work-for-workers {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}} {0 {:work \A, :time 5},
                                                                                                  1 {:work :not-working, :time :not-working},
                                                                                                  2 {:work :not-working, :time :not-working},
                                                                                                  3 {:work :not-working, :time :not-working},
                                                                                                  4 {:work :not-working, :time :not-working}}),
  (update-worker-state {0 {:work \A, :time 5},
                        1 {:work \B, :time 1},
                        2 {:work :not-working, :time :not-working},
                        3 {:work :not-working, :time :not-working},
                        4 {:work :not-working, :time :not-working}} '({2 {:work \D, :time 64}})),
  (update-work-plan {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}} '({2 {:work \D, :time 64}} {0 {:work :not-working, :time :not-working}}) '(\B)),
  (will-remove-work (into [] {0 {:work \A, :time 5},
                              1 {:work \B, :time 1},
                              2 {:work :not-working, :time :not-working},
                              3 {:work :not-working, :time :not-working},
                              4 {:work :not-working, :time :not-working}}),)
  (processing-part2 sample-worker-n {\A #{\C}, \B #{\A}, \C #{}, \D #{\A}, \E #{\B \D \F}, \F #{\C}}),
  (update-old-work {0 {:work \A, :time 5},
                    1 {:work \B, :time 1},
                    2 {:work :not-working, :time :not-working},
                    3 {:work :not-working, :time :not-working},
                    4 {:work :not-working, :time :not-working}}),
  (solve-part1 sample-input-path),
  (solve-part2 sample-input-path sample-worker-n),
  (solve-part1 input-path),
  (solve-part2 input-path full-worker-n),)



; key를 일(work)로 하고 value를 set of 먼저 선행되어야하는 일(pre-work)이라고 정의
; (parsing) 예시 인풋의 경우 parsing해서 -> 표현한다 {A #{C} B #{A} C #{} D #{A} E #{B D F} F #{C}}

;part1
; (processing) val가 비어있는 경우에 대해서 work를 실행한다. work 가 실행되면 모든 key-val 쌍을 순회하면서 val에 있는 pre-work를 갱신한다(삭제한다)
; 이 과정을 계속 반복한다.

;part2
; max 시간이 있음. 혼자서 일을 다 한다고 가정했을 때 A~Z까지의 걸리는 합
; 매초 상태를 생각하자. 0초에 하는 일, 1초에 하는 일...
; n명의 worker가 어떤 일을 하는지 관리하자.
; 그런 다음에 아래 라인처럼 일을 할당하고 관리히자.
; (processing) next-work을 구한다. part1과 다르게 1개 이상일 수 있다. worker capacity를 고려한다. 즉, min(next-work-count, worker-capa)