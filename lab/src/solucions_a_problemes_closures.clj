;; ################################################

(defn aplicacio-condicional
  "f és una funció de dos paràmetres, i condició és un predicat"
  [f condicio]
  (letfn [(g [x] (letfn [(h [y]
                           (if (condicio y) (f x y) h))]
                   (if (condicio x) h g)))]
    g))

;; ################################################

(defn maximitzador [f]
  (letfn [(escriu_max [y]
            (letfn [(tria_max [z]
                      (if (> (f y) (f z))
                        (escriu_max y)
                        (escriu_max z)))]
              (println (f y))
              tria_max))]
    escriu_max))

;; ################################################

(defn ordre [ops]
  (letfn [(aplica [x y]
            (println ((first ops) x y))
            (ordre (conj (vec (rest ops)) (first ops))))]
    aplica))

;; ################################################

;; Comprovem que els paràmetres a gg siguin positius
;; amb (assert (or (= -1 z) (pos? z)))
(defn aplica [f]
  (letfn [(ff [y]
            (letfn [(gg [z]
                      (assert (or (= -1 z) (pos? z)))
                      (if (= -1 z)
                        y
                        (ff (conj y (f z)))))] 
              gg))]
    (ff [])))

;; ################################################

;; Comprovem que els paràmetres a gg siguin positius
;; amb (assert (or (= -1 z) (pos? z)))
(defn filtra [pred]
  (letfn [(ff [y]
            (letfn [(gg [z]
                      (assert (or (= -1 z) (pos? z)))
                      (if (= -1 z)
                        (do
                          (println y)
                          :done)
                        (if (pred z)
                          (ff (conj y z))
                          (ff y))))]
              gg))]
    (ff [])))

;; Només cal fer un petit canvi quan trobem -1:

(defn filtra [pred]
  (letfn [(ff [y]
            (letfn [(gg [z]
                      (assert (or (= -1 z) (pos? z)))
                      (if (= -1 z)
                        y
                        (if (pred z)
                          (ff (conj y z))
                          (ff y))))]
              gg))]
    (ff [])))


;; ################################################

