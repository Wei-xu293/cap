(ns binarySearch)

(defn trivial [l]
  (let [[_ & lst] l]
    (empty? lst)))
  
(defn directe [l] 
  (let [[x & _] l] (true? x)))
  
(defn dividir [l] 
  (let [[x & lst] l 
        n (dec (count lst)) 
        mid (quot n 2) 
        mid-val (nth lst mid)] 
    (cond 
      (= x mid-val) [(list true) (list true)] 
      (< x mid-val) [(list (take mid (drop 1 lst))) (list false)] 
      :else [(list false) (list (conj (drop (inc mid) lst) x))])))

(defn vèncer [_ _ [sol1 sol2]] 
   (or sol1 sol2))

(defn dIv 
  [trivial directe dividir vèncer] 
  (letfn [(dIv' 
           [vct] 
           (if (trivial vct) 
             (directe vct) 
             (let [[x1 x2] (dividir vct) 
                   y1 (dIv' x1) 
                   y2 (dIv' x2)] 
               (vèncer vct [x1,x2] [y1,y2]))))] 
    dIv'))

(def dicotomica (dIv trivial directe dividir vèncer))