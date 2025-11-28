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


(defn my-filter-cps [p s k]
  (if (empty? s) (k '())
      (let [[x & xs] s]
        (recur p xs 
               (fn [mapped_xs] (k (if (p x) (cons x mapped_xs) mapped_xs)))))))

(def numbers [1 2 3 4 5 6 7 8 9 10])
(def t (range 0 1000000))

(defn my-filter [p s] 
  (letfn [(my-filter-cps-t [p s k]
                           (if (empty? s) #(k '())
                               (let [[x & xs] s] 
                                 #(my-filter-cps-t p xs
                                                 (fn [v]
                                                   (fn [] (k (if (p x) (cons x v) v))))))))]
    (trampoline my-filter-cps-t p s identity)))

;; (count (my-filter even? t)) 👉 500000