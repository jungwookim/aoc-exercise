(ns p2018_day6_reviewed
  (:require [clojure.string :as s]))


(def sample-max 32)
(def full-max 10000)
(def sample-input-path "resources/sample_input_p6.txt")
(def input-path "resources/input_p6.txt")

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
  (let [m-seq (map (fn [x]
                     {:distance (manhattan-distance x point)
                      :source   x}) sources)]
    (let [{min-distance :distance
           source       :source} (apply min-key :distance m-seq)]
      (if (> (count (filter #(= (:distance %) min-distance) m-seq)) 1)
        []
        source))))

; for part 2
(defn find-sum-of-distances-from-point-to-sources [point sources]
  (->> (map (fn [x] (manhattan-distance x point)) sources)
       (apply +)))

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

(defn processing-part1 [sources
                        {:keys [max-x max-y]
                         :as   m1}]
  (let [points (make-matrix 0 0 max-x max-y)]
    (reduce (fn [acc-m
                 point]
              (let [closest-source (find-closest-source point sources)]
                (if (on-infinite-point? point m1)
                  (update-infinite-areas acc-m closest-source)
                  (update-finite-areas acc-m point closest-source))))
            {:inf-points #{}
             :f-points   {}}
            points)))

(defn processing-part2 [sources
                        {:keys [max-x max-y]}] ; as, or
  (let [points (make-matrix 0 0 max-x max-y)]
    (reduce (fn [acc-v
                 point]
              (let [sum-of-distances (find-sum-of-distances-from-point-to-sources point sources)]
                (conj acc-v sum-of-distances)))
            []
            points)))

(defn extract-finite-area [{:keys [inf-points f-points]}]
  (remove (fn [[_ closest-source]]
            (contains? inf-points closest-source))
          f-points))

(defn find-largest-finite-area-size [finite-data]
  (->> (map (fn [[k v]] {v k}) finite-data) ; map-invert
       (map #(first (keys %)))
       frequencies
       (apply max-key val)
       last))

(defn parsing-part1 [path]
  (prepare-data path))

(defn aggregating-part1 [processed-data]
  (->> processed-data
       extract-finite-area
       find-largest-finite-area-size))

(defn solve-part1 [path]
  (let [parsed-data (parsing-part1 path)
        min-max-x-y-data (min-max-x-y parsed-data)]
    (->> (processing-part1 parsed-data min-max-x-y-data)
         aggregating-part1)))

(defn aggregating-part2 [n processed-data]
  (->> processed-data
       (filter #(> n %))
       count))

(defn solve-part2 [path n]
  (let [parsed-data (parsing-part1 path)
        min-max-x-y-data (min-max-x-y parsed-data)]
    (->> (processing-part2 parsed-data min-max-x-y-data)
         (aggregating-part2 n))))

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


