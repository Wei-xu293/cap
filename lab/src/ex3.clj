(ns ex3)
(defn f [x]
  (if (even? x) "parell" "senar"))
(println (f 5))

(defn f4 [x]
  (if (>= x 5.0) "aprovat" "suspes"))
(println (f4 5.6))
(println (f4 4.9))