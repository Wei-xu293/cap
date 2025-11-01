(ns W94600)

(defn until1 [cond f x0] 
  (if (cond x0) x0
    (let [next (f x0)]
      (recur cond f next))))

(defn until2 [cond f x0] 
  (first (filter cond (iterate f x0))))

(defn mcd [a b]
  (peek (until1 #(== (first %) (second %)) 
                 (fn [[x y]] (if (> x y) [(- x y) y] [x (- y x)]))
                 [a b])))