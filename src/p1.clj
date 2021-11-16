(ns p1)

(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
  (as-> [] values
    (doseq [v (line-seq rdr)]
      (conj values (Integer/parseInt v)))
    values
    )
  )

(with-open [rdr (clojure.java.io/reader "src/input_p1.txt")]
  (loop [res 0]
    (when (line-seq rdr)
      (recur (+ res (Integer/parseInt (line-seq rdr)))))
      )
  )

(defn sum-all [numbers]
  (reduce + numbers))

(comment
  (sum-all [1 2 3 -4])
  )
