(ns llistesInf)

(def ones (iterate identity 1N))

;; (def naturals (map bigint (range)))

(def naturals (iterate inc 0N))

(def natAtOne (iterate inc 1N))

(def enters (cons 0N
                  (interleave (iterate inc 1N)
                              (iterate dec -1N))))

(def powers-of-2 (iterate #(* 2 %) 1N))

(def triangulars (map #(/ (* % (+ % 1)) 2) naturals))

;(def factorials (cons 1N (lazy-seq (map * natAtOne factorials))))

(def factorials
  (map first
       (iterate (fn [[acc n]] [(* acc n) (inc n)])
                [1N 1N])))

(def fibs
  (map first
       (iterate (fn [[f1 f2]] [(+ f1 f2) f1]) [0N 1N])))

(defn sieve
  [[p & xs]]
  (cons p
        (lazy-seq
         (sieve (for [x xs :when (pos? (mod x p))]
                  x)))))

(def primers (sieve (iterate inc 2N)))

(defn prime-factors [n]
  (loop [n n
         i 2
         factors #{}] ;; Use a set to avoid duplicates
    (cond
      (= n 1) factors
      ;; If i*i > n, the remaining n is a prime itself
      (< n (* i i)) (conj factors n)
      ;; If divisible, add i and divide n
      (zero? (rem n i)) (recur (/ n i) i (conj factors i))
      ;; Else try next number
      :else (recur n (inc i) factors))))

(defn isHamming? [x]
  (cond
    (<= x 0) false
    (= x 1) true
    :else
    (let [pd (prime-factors x)
          ;; We want factors strictly greater than 5
          bad-factors (filter #(> % 5) pd)]
      ;; It is Hamming if there are NO bad factors
      (empty? bad-factors))))

(def hammings (filter isHamming? natAtOne))