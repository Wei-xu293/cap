(ns ordre)

(defn ordre
  [fs]
  (letfn [(g [x y] 
             (println ((first fs) x y)) 
             (ordre (conj (vec (rest fs)) #((first fs) %1 %2))))]
    g))

(def funcions [+ * -]) 

(def f (ordre funcions))
(def f (f 1.0 2.0))
(def f (f 1.0 2.0))
(def f (f 1.0 2.0))
(def f (f 1.0 2.0))
(def f (f 1.0 2.0))