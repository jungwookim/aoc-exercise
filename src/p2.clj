(ns p2)

(defn read-input
  "output: [\"abc\" \"abf\" \"bac\"]"
  [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn character-to-keyword [c]
  (keyword (str c)))

(defn char-convert-to-map-with-value-zero [c]
  {(character-to-keyword c) 0})

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

(defn contains-n-freq-char? [n li]
  (some #(>= % n) li))

(defn update-freq [acc-map character]
  (let [key (character-to-keyword character)]
    (when (contains? acc-map key)
      (assoc acc-map key (inc (get acc-map key))))))

(defn words-to-freq
  "return: collections of alphabet map"
  [words]
  (doseq [word words]
    (let [my-map (get-new-alphabet-map)]
      (reduce update-freq my-map word))))

(defn words-to-freq2
  "return: collections of alphabet map"
  [words]
  (map (fn [word]
         (let [my-map (get-new-alphabet-map)]
           (reduce update-freq my-map word))) words))

(defn calc
  "count >= 2 and >=3 cases"
  [coll]
  (let [cnt-gte-2 0
        cnt-gte-3 0]
    (doseq [word-map coll]
      (let [values (vals word-map)]
        (prn (str values " $$ " cnt-gte-3))
        (cond
          (contains-n-freq-char? 2 values) (do (inc cnt-gte-2) (inc cnt-gte-3))
          (contains-n-freq-char? 3 values) (inc cnt-gte-3))))
    (* cnt-gte-2 cnt-gte-3)))

(comment (-> "resources/input_p2.txt"
             read-input
             words-to-freq2
             calc))

; word 순회하면서 map 업데이트
; 2개 이상 발견된거 있으면 ans1, ans2 둘다 +1
; 3개 이상 발견된거 있으면 ans2 +1
; ans1 * ans2
