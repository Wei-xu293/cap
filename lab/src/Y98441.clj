(ns Y98441)

(defn xor [a b]
  (or (and (odd? a) (even? b)) (and (even? a) (odd? b))))

(defn vector-especial? [nums]
  (let [len (count nums) x (first nums) v (rest nums)]
    (if (< len 2) true
       (= (count (reduce #(if (xor (peek %1) %2) (conj %1 %2) %1) (vector x) v)) len))))