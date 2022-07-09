(ns _2018.p2018-day5)

; read input

(defn read-input [path]
  "output type: string"
  (-> path
      slurp
      (clojure.string/split #"\n")
      first))

(defn upper-case? [char]
  (= (clojure.string/upper-case char) (str char)))


(defn lower-case? [char]
  (= (clojure.string/lower-case char) (str char)))

(defn to-remove? [char1 char2]
  (if (or (nil? char1)
          (nil? char2))
    false
    (and (or (= (upper-case? char1) (lower-case? char2))
             (= (lower-case? char1) (upper-case? char2)))
         (= (clojure.string/upper-case char1) (clojure.string/upper-case char2)))))


(defn squeeze-string [string]
  (reduce (fn [new-string
               val]
            (if (to-remove? val (last new-string))
              (subs new-string 0 (dec (count new-string)))
              (str new-string val)))
          ""
          string))

(defn solve-part1 [path]
  (-> path
      read-input
      squeeze-string
      count))

(def sample-input-path "resources/sample_input_p5.txt")
(def input-path "resources/input_p5.txt")

; part 2
; alphabet set을 가지고
; 전체 순회하면서
; a A 이런걸 다 없앤 결과를 가지고
; 제일 작은 값 가지면 됨
(def alphabets (map char (range 97 123)))

(defn delete-units-in-string [char string]
  (-> string
      (clojure.string/replace (clojure.string/lower-case char) "")
      (clojure.string/replace (clojure.string/upper-case char) "")))

(defn removed-seq-by-unit-seq [string]
  (->> alphabets
       (map (fn [x] (delete-units-in-string x string)))))


(defn logic-part2 [string]
  (->> string
       removed-seq-by-unit-seq
       (map squeeze-string)
       (map count)
       (apply min)))

(defn solve-part2 [path]
  (->> path
       read-input
       logic-part2))

(comment
  (solve-part1 sample-input-path),
  (solve-part1 input-path),
  (solve-part2 sample-input-path),
  (solve-part2 input-path),)

