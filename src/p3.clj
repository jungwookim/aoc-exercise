(ns p3)

; part 1 main logics

(defn vec2d [sx sy f]
  (mapv (fn [x] (mapv (fn [y] (f x y)) (range sx))) (range sy)))

(defn matrix []
  (vec2d 2000 2000 (constantly -1)))

(defn update-cell [matrix id x y]
  (if (neg? (get-in matrix [x y]))
    (update-in matrix [x y] (constantly id))
    (update-in matrix [x y] (constantly 0))))

(defn gen-modified-vals [id ix iy sx sy]
  (map (fn [x] (map (fn [y] (list (+ iy x) (+ ix y) id)) (range sx))) (range sy)))

(defn flatten-vals [values]
  (->> values
       flatten
       (partition 3)))
;
(flatten-vals (map (fn [[a b c d e]] (gen-modified-vals a b c d e)) [[100 1 3 4 4] [101 1 3 4 4]]))

(reduce (fn [acc [x y id]]
          (update-cell acc id x y))
        (matrix)                                            ; 초기값
        (flatten-vals (gen-modified-vals 100 1 3 4 4)))

(defn logic-part1 [data]
  (reduce (fn [acc [x y id]]
            (update-cell acc id x y))
          (matrix)                                          ; 초기값
          (flatten-vals (map (fn [[id [ix iy] [sx sy]]]
                               (gen-modified-vals id ix iy sx sy))
                             data))))

; read-input and parsing

(defn read-input [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn parse-id
  "input: \"#44\", output: 44"
  [string]
  (Integer/parseInt (subs string 1 (count string))))

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
       (mapv (fn [x] (clojure.string/split x #" ")))
       (mapv (fn [[id _ pos size]]
               (list (parse-id id) (parse-pos pos) (parse-size size))))))

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1
       flatten
       (filter #(zero? %))
       count))

; part2

(defn get-total-count-by-id [path]
  (->> path
       prepare-data
       (map (fn [[id _ [sx sy]]] (list id (* sx sy))))))

(defn logic-part2 [path data]
  (let [current-id-count (->> data
                              logic-part1
                              flatten
                              (filter #(pos? %))
                              frequencies)

        total-count-by-id (get-total-count-by-id path)]
    (-> (filter (fn [[x y]] (== y (get current-id-count x -1))) total-count-by-id)
        first
        first)))
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
