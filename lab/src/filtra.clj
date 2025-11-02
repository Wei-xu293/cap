(ns filtra)

(defn filtra [f]
  (letfn [(g [ls] 
             (letfn [(ff [x] 
                         (if (= x -1) (do (println ls) :done) 
                             (if (f x) 
                               (g (conj ls x)) 
                               (g ls))))]
               ff))]
    (g [])))

(def h (filtra even?))
((((((h 2) 4) 6) 8) 10) -1)
((((((h 2) 3) 6) 5) 10) -1)