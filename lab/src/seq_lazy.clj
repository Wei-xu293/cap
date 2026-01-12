(def naturals (lazy-seq (cons 0 (map inc naturals))))
(def factorials (lazy-seq (cons 1N (map * factorials (iterate inc 1N)))))

(def f identity)

(def seq-general (lazy-seq (cons 1 (map f seq-general))))

