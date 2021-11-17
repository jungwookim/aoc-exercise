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

(defn gen-alphabet-map []
  (->> (map char (range (int \a) (inc (int \z))))
       (map char-convert-to-map-with-value-zero)))

(defn contains-n-freq-char? [n li]
  (filter #(% >= n) li))

(defn update-freq [m key]
  (println (str ">>>" m " " key))
  (when (contains? m key)
    (assoc m key (inc (get m key)))))

(defn words-to-freq [words]
  (doseq [word words]
    (let [my-map gen-alphabet-map]
      (doseq [character (seq word)]
        (reduce update-freq my-map character)))))

(comment (-> "resources/input_p2.txt"
             read-input
             words-to-freq))





; word 순회하면서 map 업데이트
; 2개 이상 발견된거 있으면 ans1, ans2 둘다 +1
; 3개 이상 발견된거 있으면 ans2 +1
; ans1 * ans2