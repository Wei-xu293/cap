(ns listComp)

(defn my-map [f ls] 
  (for [x ls] (f x)))

(defn my-filter [f ls] 
  (for [x ls :when (f x)] x))

(defn my-zip-withb [f l1 l2]
  (for [x l1]  (last (take x (map (partial f x) l2)))))

(defn my-zip-with [f l1 l2] 
  (for [[x y] (map vector l1 l2)] (f x y)))

(defn thingify [l1 l2]
  (for [x l1 y l2 :when (zero? (mod x y)) ] (vector x y)))

(defn factors [n]
  (for [x (range 1 (inc n)) :when (zero? (mod n x))] x))