(ns roman)

(defn r2i [a]
    (cond
        (= a \I) 1
        (= a \V) 5
        (= a \X) 10
        (= a \L) 50
        (= a \C) 100
        (= a \D) 500
        (= a \M) 1000
        :else 0))

(defn r2i2 [a b]
    (cond
        (and (= a \I) (= b \V)) 1
        (and (= a \I) (= b \X)) 1
        (and (= a \X) (= b \L)) 10
        (and (= a \X) (= b \C)) 10
        (and (= a \C) (= b \D)) 100
        (and (= a \C) (= b \M)) 100
        :else 0))

(defn roman2int [ls]
    (let [x (first ls) y (s) s (r2i2 x y)]
    (+ (r2i x) (- (* 2 s)) (roman2int (rest ls)))))

(defn roman2int2 [ls]
    (loop [a \P, l ls, acc 0]
        (if (empty? l) acc
            (let [b (first l) s (* 2 (r2i2 a b))]
                (recur b (rest l) (- (+ acc (r2i b)) s))))))
