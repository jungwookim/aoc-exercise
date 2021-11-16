(ns p2)

(defn read-input [path]
  (-> path
      slurp
      (clojure.string/split #"\n")))

(defn string-to-convert-map-with-value-zero [c]
  {(keyword (str c)) 0})

(defn gen-alphabet-map []
  (->> (map char (range (int \a) (inc (int \z))))
       (map string-to-convert-map-with-value-zero)))

(defn contains-n-freq-char? [n li]
  (filter #(% >= n) li))


; word 순회하면서 map 업데이트
; 2개 이상 발견된거 있으면 ans1, ans2 둘다 +1
; 3개 이상 발견된거 있으면 ans2 +1
; ans1 * ans2