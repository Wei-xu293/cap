(ns Y93083)

(defn power [x p] (reduce * (repeat p x)))

(defn myc [val elem] (if (nil? elem) val (inc val)))

(defn my-count [ls] (reduce myc 0 ls))

(defn average [ls] (/ (reduce + ls) (my-count ls)))

(defn build-palindrome [ls] (reduce #(conj %1 %2) ls ls))

(defn odds-n-evens [ls] (let [result (reduce (fn [[odds evens] x] (if (odd? x) [(conj odds x) evens] [odds (conj evens x)])) [[] []] ls)] (map sequence result)))

(defn my-filter [f ls] (seq (reduce (fn [l x] (if (f x) (conj l x) l)) [] ls)))

(defn exists [x ls] (not-empty (my-filter #(= x %) ls)))

(defn remove-list [lx ly]
  (if (or (empty? ly) (empty? lx)) lx
      (reverse (reduce (fn [l x] (if (exists x ly) l (conj l x))) '() lx))))

;(defn remove-list [lx ly] 
;  (let [ly-set (set ly)] 
;    (reverse (reduce 
;              (fn [acc x] 
;                (if (ly-set x) acc (conj acc x))) 
;              '() 
;              lx))))
