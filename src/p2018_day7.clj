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

(defn update-pre-work-done [m next-work]
  (->> (map (fn [[work pre-work]]
              [work (remove (fn [x] (= x next-work)) pre-work)])
            m)
       (into {})))

(defn processing
  "input: {\\A #{\\C}, \\B #{\\A}, \\D #{}, \\C #{}, \\E #{\\B \\D \\F}, \\F #{\\C}}"
  [m]
  (:work-plan (reduce (fn [{:keys [acc-m work-plan]} _]
                        (let [next-work (find-next-work acc-m)]
                          {:work-plan (conj work-plan next-work)
                           :acc-m (-> (dissoc acc-m next-work)
                                      (update-pre-work-done next-work))}))
                      {:acc-m m
                       :work-plan []}
                      m)))

(defn aggregating [char-seq]
  (s/join "" char-seq))

(defn solve-part1 [path]
  (->> (parsing path)
       processing
       aggregating))


(def sample-input-path "resources/sample_input_p7.txt")
(def input-path "resources/input_p7.txt")

(comment
  (init-work-data '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (group-by-val '([\C \A] [\C \F] [\A \B] [\A \D] [\B \E] [\D \E] [\F \E])),
  (find-next-work {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}}),
  (update-pre-work-done {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}} \C),
  (parsing sample-input-path),
  (processing {\A #{\C}, \B #{\A}, \D #{}, \C #{}, \E #{\B \D \F}, \F #{\C}}),
  (aggregating [\C \A \B \D \F \E])
  (solve-part1 sample-input-path),
  (solve-part1 input-path),)



; key를 일(work)로 하고 value를 set of 먼저 선행되어야하는 일(pre-work)이라고 정의
; (parsing) 예시 인풋의 경우 parsing해서 -> 표현한다 {A #{C} B #{A} C #{} D #{A} E #{B D F} F #{C}}
; (processing) val가 비어있는 경우에 대해서 work를 실행한다. work 가 실행되면 모든 key-val 쌍을 순회하면서 val에 있는 pre-work를 갱신한다(삭제한다)
; 이 과정을 계속 반복한다.
