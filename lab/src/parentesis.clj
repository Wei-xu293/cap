(ns parentesis)

(defn foldr [f x0 s]
  (if (empty? s) x0
      (let [[cap & cua] s]
        (f cap (foldr f x0 cua)))))

(defn flip [f]
  (fn [a b]
    (f b a)))

(defn balancejat 
  [sequencia] 
  (let [obrir #{\[ \(}] 
    (loop [s sequencia, pila []] ;; fem servir un vector com a pila 
      (if (empty? s)
        (empty? pila)
        (let [c (first s)]
          (cond
            (obrir c) (recur (rest s) (conj pila c))
            (empty? pila) false
            :else (let [t (peek pila)]
                    (if (or (and (= t \[) (= c \]))
                            (and (= t \() (= c \))))
                      (recur (rest s) (pop pila))
                      false))))))))

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

(defn initialNQ [n] [0 0 n ""])

(defn objNQ [[o t n psol]] (and (= o t) (= o n)))

(defn succNQ [[o t n psol]]
  (letfn [(valid [psol [o t]] 
                 (and (>= o t) 
                      (<= o n)))]
    (for [[open closed op] [[(inc o) t true] [o (inc t) false]]
          :when (valid psol [open closed])]
      [open closed n (if op (str psol \() (str psol \)))])))

(defn solucions [n]
  (let [sols ((bTck succNQ objNQ) (initialNQ n))]
    (map last sols)))

(defn nombre-solucions [n]
  (count ((bTck succNQ objNQ) (initialNQ n))))