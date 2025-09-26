(ns u28436)

(defn abs-value [x] (if (< x 0) (- x) x))

(defn power1 [x p]
  (if (= p 0) 1
      (*' x (power1 x (- p 1)))))

(defn power2 [x p]
  (loop [i p acc 1]
    (if (= i 0) acc
        (let [d (dec i)]
          (recur d (*' acc x))))))

(defn prime?
  ([n] (if (< n 2) false (prime? n 2)))
  ([n d]
   (cond (> (*' d d) n) true
         (zero? (mod n d)) false
         :else (recur n (inc d)))))