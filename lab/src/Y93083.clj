(ns Y93083)

(defn power [x p]
  (reduce * (repeat p x)))

(println (power 2 3))

(defn myc [val elem]
  (if (nil? elem) val (inc val)))

(defn my-count [ls]
  (reduce myc 0 ls))

(println (my-count '(2 1 3)))

(defn average [ls] 
  (/ (reduce + ls) (my-count ls)))

(println (average '(2 1 3)))
;(build-palindrome '(2 1 3))
;(odds-n-evens '(1 2 3))
;(remove-list '(1 2 3 4) '(1 2))