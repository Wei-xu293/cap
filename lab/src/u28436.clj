(ns u28436)
(defn abs-value [x]
  (if (< x 0) (- x) x))

;;(println (abs-value -666))
;;(println (abs-value 1))
;;(println (abs-value 0))
;;(println (abs-value -0.6))

(defn power1 [x p]
  (if (= p 0) 1
      (*' x (power1 x (- p 1)))))
(println (power1 2 3))

(defn power2 [x p]
  (loop [i p
         acc 1]
    (if (= i 0)
      acc
      (let [d (dec i)]
        (recur d (*' acc x))))))

(println (power2 2 0))
(println (power2 2 3))
(println (power2 2 4))
(println (power2 2 31))

(defn div? [n d]
  (= (mod n d) 0))

(defn prime? [n]
  ([n] (prime? n 2))
  ([n d]
   (if (and (not (div? n d)) (> (* d d) n)) 
     true 
     (recur (inc d)))))