(ns ex1)
(println (let [x 2] (+ 1 (/ (+ 1 (* 2 x)) 3) (* x x))))

(let [n 25] (and (odd? n) (or (and (>= n 10) (<= n 20)) (and (>= n 20) (<= n 40)))))
