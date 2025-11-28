(ns myMapCPS)

(defn my-map [f s]
  (if (empty? s) '()
      (let [[cap & cua] s]
        (cons (f cap) (my-map f cua)))))

;; (use 'myMapCPS :reload-all)

(defn my-map-cps [f lst cont]
  (if (empty? lst) (cont lst)
       (let [[cap & cua] lst]
         (recur f cua (fn [x] (cont (cons (f cap) x)))))))


;; (trampoline my-map-cps-t inc (range 100000) identity)

(defn my-map-cps-t [f lst k]
  (if (empty? lst)
    #(k '())
    (let [[cap & cua] lst]
      #(my-map-cps-t 
        f 
        cua
        (fn [mapped-tail]
          (fn [] (k (cons (f cap) mapped-tail))))))))
