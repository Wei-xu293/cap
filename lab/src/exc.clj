(ns exc)

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(defn concat-elements [xs]
  (reduce #(concat %1 (seq %2)) '() xs))

;; (use 'exc :reload-all)
(comment
  (concat-elements [])                ;👉 ()
  (concat-elements [[:a :b]])         ;👉 (:a :b)
  (concat-elements [[10 20] [30 40]]) ;👉 (10 20 30 40)
  (str-cat ["Sóc" "del" "Barça"])     ;👉 "Sóc del Barça"
  (str-cat ["Ya" "si" "eso"])         ;👉 "Ya si eso"
  (str-cat ["quants" " " "espais"])   ;👉 "quants   espais"
  (str-cat [])                        ;👉 ""
  )

(defn str-cat [xs]
  (reduce 
   (fn [acc s] 
     (if (empty? acc) s
         (str acc " " s)))
   "" 
   xs))