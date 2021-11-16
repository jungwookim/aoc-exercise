(ns p1)

;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (as-> [] values
;    (doseq [v (line-seq rdr)]
;      (conj values (Integer/parseInt v)))
;    values
;    )
;  )
;
;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (loop [res 0]
;    (when (line-seq rdr)
;      (recur (+ res (Integer/parseInt (line-seq rdr)))))
;      )
;  )

;; scratch pad on part 1
(def p1-vals (clojure.string/split (slurp "src/input_p1.txt") #"\n"))

(def p1-parsed-vals
  (map #(Integer/parseInt %) p1-vals))

(defn sum-all [numbers]
  (reduce + numbers))

(comment
  (sum-all p1-parsed-vals))


;; final on part 1
(defn read-input-files [target]
  (-> (slurp target)
      (clojure.string/split #"\n")))

(defn string-list-to-int-list [li]
  (map #(Integer/parseInt %) li))

(defn solve-part1 [li]
  (reduce + li))

(comment
  (-> "src/input_p1.txt"
      read-input-files
      string-list-to-int-list
      solve-part1))



;; scratchpad on part2

; 더한 결과 값을 set에 넣고 포함하고 있으면 바로 반환한다
(defn solve-part2 [li]
  (let [inf-li (flatten (repeat li))]
    (loop [temp-set #{}
           cur-li inf-li
           acc 0]
      (if (temp-set (+ acc (first cur-li)))
        (+ acc (first cur-li))
        (recur (conj temp-set (+ acc (first cur-li)))
               (rest cur-li)
               (+ acc (first cur-li)))))))

(comment
  (-> "src/input_p1.txt"
      read-input-files
      string-list-to-int-list
      solve-part2))
