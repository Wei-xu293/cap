(def rev (fn [v] 
  (reduce #(cons %2 %1) [] v)))

(= (rev [1 2 3 4 5]) [5 4 3 2 1])