(ns p2018_day2)

; part 1
(defn read-input
  "output: [\"abc\" \"abf\" \"bac\"]"
  [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn character-to-keyword [c]
  (keyword (str c)))

(defn get-new-alphabet-map []
  {:a 0
   :b 0
   :c 0
   :d 0
   :e 0
   :f 0
   :g 0
   :h 0
   :i 0
   :j 0
   :k 0
   :l 0
   :m 0
   :n 0
   :o 0
   :p 0
   :q 0
   :r 0
   :s 0
   :t 0
   :u 0
   :v 0
   :w 0
   :x 0
   :y 0
   :z 0})

(defn generator-alphas []
  (into {} (mapv (fn [k] {(character-to-keyword (char k)) 0}) (range (int \a) (inc (int \z))))))

(generator-alphas)

;mapv? not lazy, vector


(defn calc-freq
  "input: [[1 2 3 4] [2 2 0 0] ...]"
  [li]
  (apply * (reduce (fn [[res1 res2] val]
                     (cond
                       (and (some #(== % 3) val) (some #(== % 2) val)) [(inc res1) (inc res2)]
                       (some #(== % 3) val) [res1 (inc res2)]
                       (some #(== % 2) val) [(inc res1) res2]
                       :else [res1 res2])) [0 0] li)))

; filter 를 사용하면 더 좋을 것 같다.
; (count (filter #(= (val %) 2) li))

; reduce는 almighty 함수다
; reduce ban

;Execution error (IllegalArgumentException) at user/eval1375 (form-init8613727876351708500.clj:1).
;contains? not supported on type: clojure.lang.LazySeq

(defn update-freq [acc-map character]
  (let [key (character-to-keyword character)]
    (when (contains? acc-map key)
      (assoc acc-map key (inc (get acc-map key))))))
; contains 는 set에 쓰기 좋을 것 같다, 이유는 set은 명시적이지만 다른 건 좀 헷갈림ㅓ
; (:a acc-map) or (get acc-map key)
; vector (contains? [1 2 3 4] 3)


(defn words-to-freq
  "return: collections of alphabet map. ex. [{:a 0 :b 1 :c 2 ... :z 0} ...]"
  [words]
  (map (fn [word]
         (let [my-map (get-new-alphabet-map)]
           (reduce update-freq my-map word))) words))

; frequencies + char-array

(defn logic-part1 [n str-li]
  (->> str-li
       (map frequencies)
       (map vals)
       (filter #(some (fn [x] (== x n)) %))
       count))

(defn solve-part1 [path]
  (let [res1 (->> path
                  read-input
                  (logic-part1 2))
        res2 (->> path
                  read-input
                  (logic-part1 3))]
    (* res1 res2)))

; part2

(defn get-pairs
  "return example: [[\"abc\" \"abe\"] ...]"
  [str-v]
  (for [str1 str-v
        str2 str-v]
    [str1 str2]))

(defn get-count-word [target seq]
  (->> (list (count seq) seq) ;(0 ())
      (map (fn [x] (prn ">>>" x)))))

(def sample-data '(([\a \a] [\b \b] [\c \c] [\d \d] [\e \e])
                   ([\a \f] [\b \g] [\c \h] [\d \i] [\e \j])))
(filter (fn [x]
          (filter
            (fn [y]
              (= (first y) (last y)))
            x))
        sample-data)

(defn get-count-word [str]
  [{:count (count str) :word str}])

(defn find-different-count [[str1 str2]]
  (->> (map (fn [char1 char2]
              (when (= char1 char2)
                char1))
            str1 str2)
       (apply str)
       prn))
       ;(filter #(not= % ""))))
       ;(remove nil?)
       ;(apply str)
       ;get-count-word
       ;(filter (fn [x] (= (:count x) (dec (count str1)))))
       ;first
       ;:word
       ;(remove nil?)))


(defn logic-part2 [str-v]
  (->> str-v
       get-pairs
       (map (fn [x] (find-different-count x)))))

(defn solve-part2 [path]
  (->> path
       read-input
       logic-part2))

(comment (solve-part1 "resources/input_p2.txt"),
         (find-different-count ["abcde" "abce"]),
         (find-different-count ["bbb" "bba"]),
         (solve-part2 "resources/sample_input_p2.txt"))
