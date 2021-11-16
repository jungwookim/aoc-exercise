(ns p1)

;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (as-> [] values
;    (doseq [v (line-seq rdr)]
;      (conj values (Integer/parseInt v)))
;    values
;    )
;  )
;
;(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
;  (loop [res 0]
;    (when (line-seq rdr)
;      (recur (+ res (Integer/parseInt (line-seq rdr)))))
;      )
;  )

;; scratch pad
(def p1-vals (clojure.string/split (slurp "src/input_p1.txt") #"\n"))

(println p1-vals)

(def p1-parsed-vals
  (map #(Integer/parseInt %) p1-vals))

(defn sum-all [numbers]
  (reduce + numbers))

(comment
  (sum-all p1-parsed-vals)
  )

;; final
(defn read-input-files [target]
  (-> slurp target
      (clojure.string/split #"\n")))

(defn string-list-to-int-list [li]
  (map #(Integer/parseInt %) li))

(defn sum-list [li]
  (reduce + li))

(comment
  (-> read-input-files "src/input_p1.txt"
       string-list-to-int-list
       sum-list)
  )
