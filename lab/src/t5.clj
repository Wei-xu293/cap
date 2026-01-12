(ns t5)

(defn creixent [s]
  (letfn [(auxiliar [sp]
            (if (empty? sp)
              true
              (and (< (first (first sp)) (second (first sp)))
                   (auxiliar (rest sp)))))]
    (let [ss (map vector s (rest s))]
      (auxiliar ss))))

;When you pass multiple collections to map, 
;it iterates over them simultaneously,
;taking the nth element from each collection for the function call.
;   The iteration stops as soon as the shortest collection is exhausted.

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(def fold foldr)

#_{:clj-kondo/ignore [:redefined-var]}
(defn creixent [s] 
  (let [ss (map vector s (rest s))]
    (fold  #(and (< (first %1) (second %1)) %2) true ss)))

