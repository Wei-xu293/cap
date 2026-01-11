(ns loop-recur)


(defn fib [x] 
  (loop [i x f1 1 f2 1 res '()]
    (if (= i 0) (reverse res)
        (recur (dec i) f2 (+ f1 f2) (conj res f1)))))

(println (fib 8))