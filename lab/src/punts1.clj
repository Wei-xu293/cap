(ns punts1)

(require '[clojure.math :as m])

(defn punt [x y]
  (letfn [(f [c & p]
            (cond (= c :crt) (list x y)
                  (= c :plr) (list (m/sqrt (+ (* x x) (* y y))) (m/to-degrees (m/atan2 y x)))
                  (= c :dst) (let [[x2 y2] ((first p) :crt) u (- x x2) v (- y y2)]
                               (m/sqrt (+ (* u u) (* v v))))))]
    f))

(defn mes-propera [p l] 
  ((apply min-key (partial p :dst) l) :crt))

;(use 'punts1 :reload-all)
;(println ((punt 2 0) :crt))
;(println ((punt 2 2) :plr))
;(println ((punt 2 2) :dst (punt 2 0)))
;(println (mes-propera (punt 2 0) (list (punt 1 1) (punt 2 1) (punt 3 2))))