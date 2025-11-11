(ns t5)

(defn creixent [s]
  (letfn [(auxiliar [sp]
            (if (empty? sp)
              true
              (and (< (first (first sp)) (second (first sp)))
                   (auxiliar (rest sp)))))]
    (let [ss (map vector s (rest s))]
      (auxiliar ss))))

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(def fold foldr)

