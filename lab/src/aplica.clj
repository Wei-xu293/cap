(ns aplica)

(require '[clojure.math :as m])

(defn aplica
  [f]
  (letfn [(g [x]
             (when (== x -1) (println (f x)))
             (conj [] (aplica f)))]
    g))

(def h (aplica m/sqrt))
((((((h 2) 4) 6) 8) 10) -1)