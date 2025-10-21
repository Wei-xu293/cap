(ns U86019)

(defn doble [x] (* x 2))

(defn f1 [ls] 
    (apply + (map doble (filter even? ls))))

(defn f2 [v]
  (map #(* % 10) (take 3 (filter #(>= % 0) v))))

(defn f3 [ls]
  ((comp reverse sort) (map #(* % 3) (filter #(>= % 5) ls))))

(defn square [x] (* x x))

(defn f4 [ls]
  (map square (take 5 (filter #(not= (mod % 3) 0) ls))))