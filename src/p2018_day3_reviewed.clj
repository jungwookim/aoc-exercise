(ns p2018_day3_reviewed)

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

(defn prepare-data
  "output: vector of {:id 4 :pos (1 3) :size (2 4)}"
  [path]
  (->> path
       read-input
       (map (fn [x] (clojure.string/split x #" ")))
       (mapv (fn [[id _ pos size]]
               {:id (parse-id id) :pos (parse-pos pos) :size (parse-size size)}))))

; part 1 main logics

(defn vec2d
  "2d vectors"
  [sx sy f]
  (mapv (fn [x] (mapv (fn [y] (f x y)) (range sx))) (range sy)))

(defn matrix
  "init matrix"
  []
  (vec2d 2000 2000 (constantly :not-visited)))

(defn update-cell
  "update cell to id at position (x, y) on matrix"
  [matrix id x y]
  (let [target (get-in matrix [x y])]
    (if (and (keyword? target) (= :not-visited target))
      (update-in matrix [x y] (constantly id))
      (update-in matrix [x y] (constantly :visited)))))



(defn matrix-data-to-update-by-id
  "output: sequence of (target-x target-y id)"
  [{id      :id
    [ix iy] :pos
    [sx sy] :size}]
  (for [x (range sx)
        y (range sy)]
    {:x (+ iy x) :y (+ ix y) :id id}))

(defn prepare-to-update-data [data]
  (map (fn [m]
         (matrix-data-to-update-by-id m))
       data))

(defn agg-data-into-matrix [to-update-data]
  (reduce (fn [acc {x :x y :y id :id}]
            (update-cell acc id x y))
          (matrix)
          to-update-data))

(defn logic-part1
  "update each one"
  [data]
  (->> data
       prepare-to-update-data
       flatten
       agg-data-into-matrix))

(defn solve-part1 [path]
  (->> path
       prepare-data
       logic-part1
       flatten
       (filter #(= :visited %))
       count))

; part2

(defn get-total-count-by-id [path]
  (->> path
       prepare-data
       (map (fn [{id      :id
                  _       :pos
                  [sx sy] :size}] {:id id :total-count (* sx sy)}))))

(defn logic-part2
  [path data]
  (let [current-id-count (->> data
                              logic-part1
                              flatten
                              (remove keyword?)
                              (filter pos?)
                              frequencies)

        total-count-by-id (get-total-count-by-id path)]
    (->> (filter (fn [{id :id total-count :total-count}] (= total-count (get current-id-count id))) total-count-by-id)
         first
         :id)))

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
