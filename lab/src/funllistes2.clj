(ns funllistes2)

(defn build-palindrome [x]
  (loop [list x toConj x]
    (if (empty? toConj) list
        (recur (conj list (first toConj)) (rest toConj)))))
(defn exists [elem x]
  (loop [e elem list x]
    (cond 
      (empty? list) false
      (== e (first list)) true
      :else (recur e (rest list)))))

(defn remove-list [x rem]
  (loop [list x toRemove rem res '()]
    (if (empty? list) (reverse res)
        (if (exists (first list) toRemove)
          (recur (rest list) toRemove res)
          (recur (rest list) toRemove (conj res (first list)))))))

(defn odds-n-evens [x]
  (loop [l x o '() e '()]
    (if (empty? l) (list (reverse o) (reverse e))
        (let [r (rest l) head (first l)]
          (if (even? head)
            (recur r o (conj e head))
            (recur r (conj o head) e))))))

(defn prime?
  ([n] (if (< n 2) false (prime? n 2)))
  ([n d]
   (cond (> (*' d d) n) true
         (zero? (mod n d)) false
         :else (recur n (inc d)))))

(defn prime-divisors [n] 
  (loop [p '() div 2]
    (if (> div n) (reverse p)
        (let [np (if (and (zero? (mod n div)) (prime? div)) (conj p div) p)]
          (recur np (inc div))))))
