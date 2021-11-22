(ns p2018_day3)

; part 1 main logics

(defn vec2d
  "2d vectors"
  [sx sy f]
  (mapv (fn [x] (mapv (fn [y] (f x y)) (range sx))) (range sy)))

(defn matrix
  "init matrix"
  []
  (vec2d 2000 2000 (constantly -1)))

(defn update-cell
  "update cell to id at position (x, y) on matrix"
  [matrix id x y]
  (if (neg? (get-in matrix [x y]))
    (update-in matrix [x y] (constantly id))
    (update-in matrix [x y] (constantly 0))))

(defn gen-modified-vals
  "output: sequence of (target-x target-y id)"
  [id ix iy sx sy]
  (map (fn [x] (map (fn [y] [(+ iy x) (+ ix y) id])
                    (range sx)))
       (range sy)))
;
;(for [x (range sx)
;      y (range sy)]
;  ((list (+ iy x))))
; for문으로 바꿀 수 있음 ..!
; list가 아니라 vector로 반환하는게 낫다
; list는 데이터를 순차적으로 처리할 때 용이하고 수정 삭제를 할 때 유리한데...
; 보통은 vector가 유리함.. 관례임 사실
; 좌표의 특성상.. hash-map이 그래도 좋음

; 함수의 이름과 하는 일이 일치하지 않다. 적절한 이름이 찾아지지 않는다면 함수일 필요가 없을 수도
(defn group-by-data
  "flatten and partition 3"
  [values]
  (->> values
       flatten
       (partition 3)))
;
;(flatten-vals (map (fn [[a b c d e]] (gen-modified-vals a b c d e)) [[100 1 3 4 4] [101 1 3 4 4]]))
;
;(reduce (fn [acc [x y id]]
;          (update-cell acc id x y))
;        (matrix)                                            ; 초기값
;        (flatten-vals (gen-modified-vals 100 1 3 4 4)))

(defn logic-part1
  "update each one"
  [data]
  (reduce (fn [acc [x y id]]
            (update-cell acc id x y))
          (matrix)                                          ; 초기값
          (group-by-data (map (fn [[id [ix iy] [sx sy]]]
                                (gen-modified-vals id ix iy sx sy))
                              data))))

(defn update-data-info [data]
  (map (fn [[id [ix iy] [sx sy]]]
         (gen-modified-vals id ix iy sx sy))
       data))

(defn agg-data-into-matrix [data]
  (reduce (fn [acc [x y id]]
            (update-cell acc id x y))
          (matrix)
          data))

(defn logic-part1-advanced
  "update each one"
  [data]
  (->> data
       update-data-info
       group-by-data
       agg-data-into-matrix))



;[[:blank ... 1 1 ...] ...]
; read-input and parsing

(defn read-input [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn parse-id
  "input: \"#44\", output: 44"
  [string]
  (Integer/parseInt (subs string 1 (count string))))

; 정규식을 쓰면 더 간단한 수도 있음

(defn parse-pos
  "input: \"1,3:\", output: (1 3)"
  [string]
  (->> (clojure.string/split (subs string 0 (dec (count string))) #",")
       (mapv (fn [x] (Integer/parseInt x)))))

(defn parse-size
  "input: \"4x4\", output: (4 4)"
  [string]
  (->> (clojure.string/split string #"x")
       (mapv (fn [x] (Integer/parseInt x)))))

(defn prepare-data [path]
  (->> path
       read-input
       (map (fn [x] (clojure.string/split x #" ")))
       (mapv (fn [[id _ pos size]]
               (list (parse-id id) (parse-pos pos) (parse-size size))))))
; parsing한 결과 현재 list -> map을 쓰는게 편한다 (위치를 기억하기 힘들 수 있음)
; hash-map이 좀 좋음
; mapv를 쓰는게 맞는가?

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1
       flatten
       (filter zero?)
       count))

; == vs =
; part2

(defn get-total-count-by-id [path]
  (->> path
       prepare-data
       (map (fn [[id _ [sx sy]]] (list id (* sx sy))))))

(defn logic-part2
  [path data]
  (let [current-id-count (->> data
                              logic-part1
                              flatten
                              (filter pos?)
                              frequencies)

        total-count-by-id (get-total-count-by-id path)]
    (->> (filter (fn [[x y]] (= y (get current-id-count x -1))) total-count-by-id)
        ffirst)))

; threading macro first and last
; idiomatic rule: -> : hash-map이나 string에 좀 씀 ->> seq에다가 많이 씀
(defn solve-part2 [path]
  (->> path
       prepare-data
       (logic-part2 path)))

(defn final-matrix-shape [path]
  (->> path
       prepare-data
       logic-part1))

(comment
  (solve-part1 "resources/sample_input_p3.txt"),
  (solve-part1 "resources/input_p3.txt"),
  (solve-part2 "resources/sample_input_p3.txt"),
  (solve-part2 "resources/input_p3.txt"),
  (final-matrix-shape "resources/sample_input_p3.txt"))
