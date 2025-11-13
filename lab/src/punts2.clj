(ns punts2)
(require '[clojure.math :as m])

(defn punt [x y] 
  (let 
   [px (atom x) 
    py (atom y)] 
    (letfn 
     [(f [c & p] 
         (cond 
               (= c :crt) (vector @px @py) 
               (= c :plr) (vector (m/sqrt (+ (* @px @px) (* @py @py))) 
                                (m/to-degrees (m/atan2 @py @px)))
               (= c :dst) (let [[x2 y2] ((first p) :crt) u (- x x2) v (- y y2)]
                            (m/sqrt (+ (* u u) (* v v))))
               (= c :setx) (let [x2 (first p)] (reset! px x2) @px)
               (= c :sety) (let [y2 (first p)] (reset! py y2) @py)
               ))] 
     f)))

(defn mes-propera [p l]
  ((apply min-key (partial p :dst) l) :crt))
 