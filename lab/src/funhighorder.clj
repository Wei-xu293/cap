(ns funhighorder)

(defn eql [l1 l2]
  (= l1 l2))

(defn prod-of-evens [l]
  (reduce * (filter even? l)))

;(scalar-product '(2.0 1.0) '(3.0 2.0))
(defn scalar-product [l1 l2] 
                (reduce + (map * l1 l2)))

(defn count-elem [x es]
  (count (filter #(= x %) es)))

(defn count-in [es x]
  (reduce + (map #(count-elem x %) es)))

(defn first-word [ls] ())

;(eql '(1 2 3) '(1 2 3))
;(eql '(1 2 3) '(3 2 1))
;(eql '(1 2 3) '(1 2 3 4))
;(prod-of-evens '(2 10 5))
;(scalar-product '(2.0 1.0) '(3.0 2.0))
;(count-in '((3 2 3) (3) () (2 2)) 3)
;(first-word "  Volem pa amb oli  ")