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


(defn interposar [x a-seq]
  (loop [res '() l a-seq]
    (if (empty? l)
      (reverse res)
      (let [[a b & _] l]
        (if (nil? b)
          (recur (conj res a) (rest l))
          (recur (conj res a x) (rest l)))))))


(comment
  (interposar 0 [1 2 3])              ;👉 (1 0 2 0 3)
  (interposar "," ["Yo" "mi" "me"]) ;👉 ("Yo" "," "mi" "," "me")
  (interposar :a [1])                 ;👉 (1)
  (interposar :a [])                  ;👉 ()
  )

(defn paritat [xs]
  (reduce (fn [st elem]
            (if (contains? st elem)
              (disj st elem)
              (conj st elem)))
          (sorted-set)
          xs))

(comment
  (paritat [:a :b :c])    ;👉 #{:a :b :c}
  (paritat [:a :a :b :b]) ;👉 #{}
  (paritat [1 2 3 1])     ;👉 #{2 3}
  )

;; (pred-and p1 p2 ... pN)
;; every? is slightly faster because it stops the iteration entirely.
  (defn pred-and [& preds]
    (fn [x]
      (every? #(% x) preds)))

(comment
  (defn pred-and [& preds]
    (assert (every? fn? preds))
    (fn [x]
      (reduce (fn [b f] (and b (f x))) true preds)))
  
  ;; 1. Standard Case
  (def check-num (pred-and pos? even?))
  (check-num 4)  ;; => true
  (check-num 3)  ;; => false (odd)
  (check-num -4) ;; => false (negative)

  ;; 2. The N=0 Case
  (def check-nothing (pred-and))
  (check-nothing "anything") ;; => true
  (check-nothing nil)        ;; => true
  )
  
