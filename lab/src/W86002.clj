(ns W86002)

(defn eql [l1 l2] 
  (= l1 l2))

(defn eql-funcions-ordre-superior [llista1 llista2]
  (if (= (count llista1) (count llista2))
    ;; Zip les llistes (map =) i comprova que tots els elements són iguals (every?)
    (every? true? (map = llista1 llista2))
    false))