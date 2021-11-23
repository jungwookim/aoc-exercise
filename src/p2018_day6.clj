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

(defn abs [n] (max n (- n)))

(defn manhattan-distance [point1 point2]
  (apply + (map (fn [a b]
                  (abs (- a b)))
                point1 point2)))

; for part 1
(defn find-closest-source
  "input: point sources [1 2] [[5 5] [1 4] ...], output: one source [1 4]"
  [point sources]
  (let [m-seq (->> sources
                   (map (fn [x]
                          {:distance (manhattan-distance x point)
                           :source   x})))]
    (let [{min-distance :distance
           source       :source} (apply min-key :distance m-seq)]
      (if (> (count (filter #(= (:distance %) min-distance) m-seq)) 1)
        :conflict
        source))))

; for part 2
(defn find-sum-of-distances-from-point-to-sources [point sources]
  (->> sources
       (reduce (fn [acc source]
                 (update acc point #(+ % (manhattan-distance source point))))
               {point 0})
       vals
       first))


; 한가지 타입의 return type이 좋을 것 같다
; 빈 seq


(defn on-infinite-point? [[x y]
                          {:keys [min-x min-y max-x max-y]}]
  (or (<= x min-x)
      (<= y min-y)
      (>= x max-x)
      (>= y max-y)))

(defn make-matrix [min-x min-y max-x max-y]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    [x y]))

(defn update-infinite-areas [m closest-source]
  (update m :inf-points #(conj % closest-source)))

(defn update-finite-areas [m point closest-source]
  (update-in m [:f-points point] (constantly closest-source)))

(defn logic-part1 [sources
                   {max-x :max-x
                    max-y :max-y
                    :as   m1}]
  (let [points (make-matrix 0 0 max-x max-y)]
    (reduce (fn [acc-m
                 point]
              (let [closest-source (find-closest-source point sources)]
                (if (on-infinite-point? point m1)           ; infinite points에 있으면
                  (update-infinite-areas acc-m closest-source) ; inf-points에 point 추가,
                  (update-finite-areas acc-m point closest-source)))) ; 그게 아니라면 f-points에 키를 좌표로하고 nearest points 추가))
            {:inf-points #{}
             :f-points   {}}
            points)))

(defn logic-part2 [sources
                   {max-x :max-x
                    max-y :max-y}]
  (let [points (make-matrix 0 0 max-x max-y)]
    (reduce (fn [acc-v
                 point]
              (let [sum-of-distances (find-sum-of-distances-from-point-to-sources point sources)]
                  (conj acc-v sum-of-distances)))
            []
            points)))


(defn extract-finite-area [{inf-points :inf-points
                            f-points   :f-points}]
  (remove (fn [[_ closest-source]]
            (contains? inf-points closest-source))
          f-points))


(defn count-keys [m-seq]
  (reduce (fn [acc-m value]
            (if (get acc-m value)
              (update acc-m value inc)
              (update acc-m value (constantly 1))))
          {}
          (map #(first (keys %)) m-seq)))

;"{(5 5) 17, (3 4) 9}"
;"agg이 목적임"
; group-by
; frequencies

;(map #(first (keys %)))
;       frequencies)

(defn find-largest-finite-area-size [finite-data]
  (->> (map (fn [[k v]] {v k}) finite-data)
       count-keys
       (apply max-key val)
       last))

(defn solve-part1 [path]
  (let [sources (prepare-data path)]
    (->> sources
         min-max-x-y
         (logic-part1 sources)
         extract-finite-area
         find-largest-finite-area-size)))

(def sample-max 32)
(def full-max 10000)

(defn solve-part2 [path n]
  (let [sources (prepare-data path)]
    (->> sources
         min-max-x-y
         (logic-part2 sources)
         (filter #(> n %))
         count)))


(def sample-input-path "resources/sample_input_p6.txt")
(def input-path "resources/input_p6.txt")


(comment
  (prepare-data sample-input-path),
  (min-max-x-y (prepare-data sample-input-path)),
  (manhattan-distance [8 6] [5 5]),
  (manhattan-distance [5 5] [8 6]),
  (find-closest-source [8 5] [[5 5] [8 3] [8 9]]),
  (find-closest-source [0 0] [[1 2] [2 1]]),
  (update-finite-areas {:f-points {}} [1 2] [5 6]),
  (find-sum-of-distances-from-point-to-sources [0 0] [[1 2] [3 4]]),
  (solve-part1 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 sample-input-path sample-max),
  (solve-part2 input-path full-max),)


