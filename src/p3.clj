(ns p3)

; part 1 main logics

(defn vec2d [sx sy f]
  (mapv (fn [x] (mapv (fn [y] (f x y)) (range sx))) (range sy)))

(defn matrix []
  (vec2d 10 10 (constantly -1))) ;matrix

(defn update-cell [matrix id x y]
  (if (neg? (get-in matrix [x y]))
    (update-in matrix [x y] (constantly id))
    (update-in matrix [x y] (constantly 0))))

(defn gen-modified-vals [id ix iy sx sy]
  (map (fn [x] (map (fn [y] (list (+ ix x) (+ iy y) id)) (range sx))) (range sy)))

(defn flatten-vals [values]
  (->> values
       flatten
       (partition 3)))

(reduce (fn [acc [x y id]]
          (update-cell acc id x y))
        (matrix)
        (flatten-vals (gen-modified-vals 100 1 3 4 4)))

; read-input and parsing

(defn read-input [path]
  (->> path
       slurp
       (clojure.string/split #"\n")))
