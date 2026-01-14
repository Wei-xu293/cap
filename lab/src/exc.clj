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

(defn aplana-parcial [xs] 
  (if (and (coll? xs) (not-any? coll? xs))
    (list xs)
    (mapcat aplana-parcial xs)))

(comment
  (aplana-parcial [["Fes"] ["Res"]])               ;👉 (["Fes"] ["Res"])
  (aplana-parcial [[[[:a :b]]] [[:c :d]] [:e :f]]) ;👉 ([:a :b] [:c :d] [:e :f])
  (aplana-parcial '((1 2) ((3 4) ((((5 6 7)))))))  ;👉 ((1 2) (3 4) (5 6 7))
  )

(defn merge-maps [f d & ds] 
  (reduce (fn [dict m]
             (reduce (fn [acc [k v]] 
                       (if (contains? dict k) 
                         (assoc acc k (f (get dict k) v)) 
                         (assoc acc k v))) 
                     dict m))
          d ds))
(comment
  (merge-maps * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}) ;👉 {:a 4, :b 6, :c 20}
  (merge-maps - {1 10, 2 20} {1 3, 2 10, 3 15})          ;👉 {1 7, 2 10, 3 15}
  (merge-maps concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
  ;👉 {:a (3 4 5), :b (6 7), :c [8 9]}
  )