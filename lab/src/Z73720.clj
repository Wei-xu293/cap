(ns Z73720)


(defn aplicacio-condicional [f cond]
  (letfn [(fr [x] 
             (if (cond x) 
               (letfn [(gr [y] (if (cond y) (f x y) gr))]
                 gr)
               #(fr %)))]
    fr))

;(def suma-si-parell (aplicacio-condicional + even?))
;((suma-si-parell 2) 4)
;(((((suma-si-parell 2) 3) 5) 7) 6)

; (use 'Z73720 :reload-all)