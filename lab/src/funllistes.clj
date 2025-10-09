(ns funllistes)

(defn my-count1 [x] (if (empty? x) 0 (+ 1 (my-count1 (rest x)))))

(defn my-count2 [x]
  (loop [list x res 0]
    (if (empty? list) res
        (recur (rest list) (+ res 1)))))

(defn my-max' [m list]
  (if (empty? list) m (my-max' (if (>= m (first list)) m (first list)) (rest list))))

(defn my-maximum1 [x] (if (empty? x) nil (my-max' (first x) (rest x))))

(defn my-maximum2 [x]
  (loop [list x ans (first x)]
    (if (empty? list) ans
        (recur (rest list) (if (>= ans (first list)) ans (first list))))))

(defn sum [x acc]
  (if (empty? x) acc (sum (rest x) (+ acc (first x)))))

(defn average1 [x] (/ (sum x 0) (my-count2 x)))

(defn average2 [x]
  (loop [list x acc 0 cnt 0]
    (if (empty? list) (/ acc cnt)
        (let [len (inc cnt) sum (+ acc (first list))]
          (recur (rest list) sum len)))))