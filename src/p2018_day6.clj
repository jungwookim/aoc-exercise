(ns p2018_day6
  (:require [clojure.string :as s]))

; 점을 다 포함하는 넉넉한 matrix를 만들고
; 좌표에 대해서 sources까지의 거리를 가지고 있는다
; 예를 들어 {[0 0] {A A까지의 거리, B B까지의 거리...}, ... } 이런식으로
; 그럼 좌표별로 가장 가까운 거리의 source를 알고 있음. 2개 이상의 source가 가장 가까우면 충돌 . 으로 정의
; 그리고 min/max x y 값을 가지면 그 좌표는 무한하게 증식 가능한 영역임

; 정리하면
; min/max x y를 가지는 matrix안에서 전체 순회를 하며 가장 가까운 source를 찾고
; 무한한지 유한한지 판단을 잘 하면 될 것 같음
; 무한한 소스를 관리해서
; 전체 인풋에서 무한한 소스를 뺀 유한한 소스에서
; 유한한 녀석이 소유하고 있는 좌표를 골라내면 됨

; 그러면 part2는 쉽게 풀림. 좌표 별로 sum of vals of val이 N보다 작은 것 구하면 됨

; 아니면... 내부에 좌표에서 가장 가까운 source를 찾고



; read input

(defn read-input [path]
  (-> (slurp path)
      (s/split #"\n")))

(defn prepare-data [path]
  (->> (read-input path)
       (map (fn [x] (s/split x #", ")))
       (map (fn [x] (map #(Integer/parseInt %) x)))))

(defn min-max-x-y [sources]
  (let [xs (map (fn [[x _]] x) sources)
        ys (map (fn [[_ y]] y) sources)]
    {:max-x (apply max xs)
     :min-x (apply min xs)
     :max-y (apply max ys)
     :min-y (apply min ys)}))

(defn manhattan-distance [point1 point2]
  (apply + (map (fn [[a b]]
                  (Math/abs (- a b)))
                (zipmap point1 point2))))

(defn find-closest-source
  "input: point sources [1 2] [[5 5] [1 4] ...], output: one source [1 4]"
  [point sources]
  (->> sources
       (map (fn [x]
              {:distance (manhattan-distance x point)
               :source x}))
       (apply min-key :distance)
       :source))

(defn on-infinite-point? [x y min-x min-y max-x max-y]
  (or (= x min-x)
      (= y min-y)
      (= x max-x)
      (= y max-y)))

(defn logic-part1 [sources]
  (let [{max-x :max-x
         min-x :min-x
         max-y :max-y
         min-y :min-y} (min-max-x-y sources)
        points (for [x (range min-x (inc max-x))
                     y (range min-y (inc max-y))]
                 [x y])]
    (reduce (fn [{inf-points :inf-points
                  f-points :f-points
                  :as m} [x y]]
              (do something) ; infinite points에 있으면 inf-points에 추가, 그게 아니라면 f-points에 키를 좌표로하고 nearest points 추가
              {:inf-points
               :f-points}
              points))))

(defn solve-part1 [path]
  (->> (prepare-data path)
       logic-part1))

(def sample-input-path "resources/sample_input_p6.txt")
(def input-path "resources/input_p6.txt")


(comment
  (prepare-data sample-input-path),
  (min-max-x-y (prepare-data sample-input-path)),
  (manhattan-distance [1 -2] [0 0]),
  (find-closest-source [0 0] [[1 2] [4 5]]),
  (logic-part1 [[1 2] [4 5]]),
  (solve-part1 sample-input-path),
  (solve-part1 input-path),)
  ;(solve-part2 sample-input-path),
  ;(solve-part2 input-path),)

