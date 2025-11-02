(ns aplica)

(require '[clojure.math :as m])

(defn aplica [f]
  (letfn [(g [acc] 
             (letfn [(apl [x]
                     (assert (or (= -1 x) (pos? x)))
                     (if (= -1 x) (println acc)
                         (g (conj acc (f x)))))]
               apl))] 
    (g [])))

(def h (aplica m/sqrt))
((((((h 2) 4) 6) 8) 10) -1)

(def h (aplica m/cos))
((((h (/ m/PI 4)) (/ m/PI 2)) m/PI) -1)