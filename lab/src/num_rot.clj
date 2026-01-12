(ns num-rot)

(defn fsmap [x fs]
  (reductions #(%2 %1) x fs))

(defn number-of-rotations [xs] 
  (loop [l xs]
    (let [a (first l) b (second l)]
      (cond
        (nil? b) 0
        (> a b) (inc (.indexOf xs a))
        :else (recur (rest l))))))