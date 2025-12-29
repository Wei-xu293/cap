(ns transducers)
(require '[clojure.string :as str])

;; (use 'transducers :reload-all)

(defn map-reducer
  [f]
  (fn [resultat valor]
    (conj resultat (f valor))))
;; (map inc (range 10)) ≡ (reduce (map-reducer inc) [] (range 10))
;; (map dec (range 10)) ≡ (reduce (map-reducer dec) [] (range 10))
;; (map #(* % %) (range 10)) ≡ (reduce (map-reducer #(* % %)) [] (range 10))

(defn filter-reducer
  [pred]
  (fn [resultat valor]
    (if (pred valor) (conj resultat valor) resultat)))

;; (reduce (filter-reducer even?) [] (range 10))


(defn mapping
  [f]
  (fn [freduce]
    (fn [resultat valor]
      (freduce resultat (f valor)))))

;; (map-reducer f) ≡ ((mapping f) conj)
;; Recibe una función-reduce como argumento y retorna función-reduce => Transducer

(defn match-first-last [word c1 c2]
  (and (= (first word) c1) (= (last word) c2)))

(defn filtering-match [c1 c2] (filter #(match-first-last % c1 c2)))

(defn split-by-space [inp] (str/split inp #" "))

(defn paraules-inici-fi [lst c1 c2]
  (transduce (filtering-match c1 c2) conj [] (split-by-space lst)))


(defn mida-grans [input-str]
  (transduce (comp
              (filter #(> (count %) 3))
              (map count))
             +
             0
             (split-by-space input-str)))

(defn ordre-mida [lst]
  (->> lst
       (into [] (filter #(> (count %) 5)))
       (sort-by count)
       vec))


(defn taking [limit]
  (fn [rf] 
    (let [counter (volatile! limit)] 
      (fn 
        ([] rf)
        ([result] (rf result))
        ([result val] 
         (if (pos? @counter) 
          (do 
            (vswap! counter dec) 
            (rf result val)) 
          (reduced result)))))))

;; (transduce (taking 3) conj [1 2 3 4 5])
;; (into [] (taking 3) [1 2 3 4 5])
;; Result: [1 2 3]