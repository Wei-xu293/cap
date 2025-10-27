(ns X13379)

(defn mides [ls] 
  (apply + (filter odd? (map count ls))))

(defn getSnd [name age] )

(defn noms [ls]
  (sort (map (comp symbol key) (filter #(> (val %) 18) ls))))

(defn suma [mp]
  (reduce + (filter #(>= % 100) (map #(* (:quantitat %) (:preu %)) mp))))