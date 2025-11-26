(ns myMapCPS)

(defn my-map [f s]
  (if (empty? s) '()
      (let [[cap & cua] s]
        (cons (f cap) (my-map f cua)))))

;; (use 'myMapCPS :reload-all)

(defn my-map-cps [f lst cont]
  (if (empty? lst) (cont lst)
       (let [[cap & cua] lst]
         (cons (cont (f cap)) (my-map f cua)))))

(defn my-map-cps-t [f lst cont] 
  (if (empty? lst) #(cont lst)
      (let [[cap & cua] lst]
        #(my-map-cps-t f ))))