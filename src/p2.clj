(ns p2)

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

(defn calc-freq [li]
  (apply * (reduce (fn [[res1 res2] val]
                     (prn res1 res2 val)
                     (cond
                       (and (some #(== % 3) val) (some #(== % 2) val)) [(inc res1) (inc res2)]
                       (some #(== % 3) val) [res1 (inc res2)]
                       (some #(== % 2) val) [(inc res1) res2]
                       :else [res1 res2])) [0 0] li)))

(defn update-freq [acc-map character]
  (let [key (character-to-keyword character)]
    (when (contains? acc-map key)
      (assoc acc-map key (inc (get acc-map key))))))

(defn words-to-freq
  "return: collections of alphabet map. ex. [{:a 0 :b 1 :c 2 ... :z 0} ...]"
  [words]
  (map (fn [word]
         (let [my-map (get-new-alphabet-map)]
           (reduce update-freq my-map word))) words))

(comment (->> "resources/input_p2.txt"
              read-input
              words-to-freq
              (map (fn [x]
                     (vals x)))
              calc-freq))

; part 2