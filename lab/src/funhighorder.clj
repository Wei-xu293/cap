(ns funhighorder)

(defn eql [l1 l2]
  (= l1 l2))

(defn prod-of-evens [l]
  (reduce * (filter even? l)))

(defn  my-reverse [l] (apply list (reduce conj '() l)))

;(scalar-product '(2.0 1.0) '(3.0 2.0))
(defn scalar-product [l1 l2]
  (reduce + (map * l1 l2)))

(defn count-elem [x es]
  (count (filter #(= x %) es)))

(defn count-in [es x]
  (map #(count-elem x %) es))

(defn ds [ls]
  (apply str (drop-while #(= \space %) ls)))

(defn first-word [ls]
  (apply str (take-while #(not= \space %) (ds ls))))