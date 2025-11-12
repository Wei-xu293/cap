(ns partialClas)

(defn my-partial [f x] 
  (letfn [(g [& l] 
             (apply f x l))] 
    g))