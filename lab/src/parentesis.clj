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

(defn initialNQ [n] [0 n '()])

(defn objNQ [[c n psol]] (and (= (* 2 n) c) (balancejat psol)))

(defn succNQ [[n psol]]
  (letfn [(valid [])]))

(defn solucions [n]
  (let [[[_ _ s] & cua] ((bTck succNQ objNQ) (initialNQ n))]
    s))

(defn nombre-solucions [n]
  (count ((bTck succNQ objNQ) (initialNQ n))))