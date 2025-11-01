(ns X11947)

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(def fold foldr)

(defn my-first [ls] 
  (fold  (fn [x y] x) nil ls))