(ns parentesis)

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(defn flip [f]
  (fn [a b]
    (f b a)))

(defn bTck [succ obj] 
  (letfn 
   [(bTck' 
     [v] 
     (cond 
       (empty? v) [] 
       (obj (peek v)) (conj (bTck' (pop v)) (peek v)) 
       :else (let [x (peek v)] 
               (recur (foldr (flip conj) (pop v) (succ x))))))] 
    (fn [inicial] 
      (bTck' [inicial]))))