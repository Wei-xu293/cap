(ns funhighorder)

(defn eql [l1 l2]
  (= l1 l2))

(defn prod-of-evens [l]
  (reduce * (filter even? l)))

;(scalar-product '(2.0 1.0) '(3.0 2.0))
(defn scalar-product [l1 l2] 
                (reduce + (map * l1 l2)))

(defn count-in)
