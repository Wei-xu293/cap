(ns maximitzador)

(defn maximitzador [func]
  (letfn [(g [i] 
             (println (func i))
             (maximitzador #(max (func %) (func i))))] 
    g))

(def h (maximitzador #(* % %)))

((((((h 2) 3) 2) 1) 5) 3)