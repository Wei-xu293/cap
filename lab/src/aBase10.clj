(ns aBase10)

(defn transforma_a_base_10 [b]
  (assert (and (>= b 2) (<= b 9)))
  (letfn [(f [y]
            (fn [z]
              (assert (>= z -1))
              (if (= z -1) y
                  (f (+ (* y b) z)))))]
    (f 0)))

(def h2 (transforma_a_base_10 2))

;; (use 'aBase10 :reload-all)
;; (println (((((((h2 1) 0) 1) 0) 1) 1) -1))

;; (let [fret (transforma_a_base_10 2)] (((((((fret 1) 0) 1) 0) 1) 1) -1))

;; (let [fret (transforma_a_base_10 3)] ((((((fret 2) 2) 1) 0) 2) -1))


;; (let [fret (transforma_a_base_10 5)] (((((((((fret 4) 3) 2) 0) 1) 4) 3) 3) -1))

;; (let [fret (transforma_a_base_10 9)] (((((((fret 8) 7) 1) 0) 3) 5) -1))
