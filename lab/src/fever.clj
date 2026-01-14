;; 4ever
(ns fever)

(def rev (fn [v] 
  (reduce #(cons %2 %1) [] v)))

(= (rev [1 2 3 4 5]) [5 4 3 2 1])

(defn my-flatten [ls] 
  (loop [xs ls
         res []]
    (if (empty? xs) 
      (seq res)
      (let [[cap & cua] xs]
        (if (coll? cap)
          (recur (concat cap cua) res)
          (recur cua (conj res cap)))))))