#_{:clj-kondo/ignore [:underscore-in-namespace]}
(ns hello_world)
(println "Hello World!")
(let [x 2]
  (+ 1 (/ (+ 1 (* 2 x)) 3) (* x x))) 