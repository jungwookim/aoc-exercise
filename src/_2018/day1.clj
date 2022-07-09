(ns _2018.day1)

;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (as-> [] values
;    (doseq [v (line-seq rdr)]
;      (conj values (Integer/parseInt v)))
;    values
;    )
;  )

;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (loop [res 0]
;    (when (line-seq rdr)
;      (recur (+ res (Integer/parseInt (line-seq rdr)))))
;      )
;  )

;; scratch pad on part 1
(def p1-vals (clojure.string/split (slurp "resources/input_p1.txt") #"\n"))

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

(defn string-seq-to-int-seq [li]
  (map #(Integer/parseInt %) li))

(defn parse-input
  "output: [1 2 3]"
  [path]
  (-> path
      read-input-files
      string-seq-to-int-seq))

(defn solve-part1
  "sum of sequences"
  [li]
  (reduce + li))

(defn solve-part11
  "another way"
  [seq]
  (apply + seq))

;; scratchpad and final on part2

; 더한 결과 값을 set에 넣고 포함하고 있으면 바로 반환한다
(defn solve-part2 [li]
  (let [inf-seq (flatten (repeat li))]
    (loop [temp-set #{}
           cur-seq inf-seq
           acc 0]
      (if (temp-set (+ acc (first cur-seq)))
        (+ acc (first cur-seq))
        (recur (conj temp-set (+ acc (first cur-seq)))
               (rest cur-seq)
               (+ acc (first cur-seq)))))))

(defn solve-part2-advanced-1 [li]
  (let [inf-seq li]
    (loop [temp-set #{}
           cur-seq inf-seq
           acc 0]
      (let [sum (+ acc (first cur-seq))]
        (if (temp-set sum)
          sum
          (recur (conj temp-set sum)
                 (rest cur-seq)
                 sum))))))

(defn solve-part2-advanced-2 [li]
  (let [inf-partial-sum li]
    (loop [temp-set #{}
           cur-seq inf-partial-sum]
      (let [sum (first cur-seq)]
        (if (temp-set sum)
          sum
          (recur (conj temp-set sum)
                 (rest cur-seq)))))))


(comment
  (-> "resources/input_p1.txt"
      parse-input
      solve-part1),
  (-> "resources/input_p1.txt"
      parse-input
      solve-part11),
  (-> "resources/input_p1.txt"
      parse-input
      solve-part2),
  (-> "resources/input_p1.txt"
      parse-input
      cycle
      solve-part2-advanced-1),
  (->> "resources/input_p1.txt"
      parse-input
      cycle
      (reductions +)
      solve-part2-advanced-2))


;; aoc exercise 밑에 resources 생성
; read file + parsing을 한번에 하고 input output을 이해하기 쉽도록 하자
; 주석까지 달아준다면 금상첨화
; comment 블럭이 맨 아래 하나만 있으면 좋음.
; flatten + repeat -> cycle, 반복된 사용 let으자 빼자
; flatten 을 지양하자
; list가 아니라 사실 seq임
; reductions  (partial sum 같은 거) 추가 과제


; list vector map set
; sequence 추상화 : first rest cons 구현하면 seq라고 해줌 / collection 추상화
; ->> (seq 추상화를 다룸)
; -> (conj concat ... )
; to be continued