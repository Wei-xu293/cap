(ns punts2)

(defn punt [x y] (let [px (atom 0) py (atom 0)]
  (letfn [(f [c & p]
    (cond (= c :crt) (list (reset! px x) (reset! py y)) ))]
    f)))
