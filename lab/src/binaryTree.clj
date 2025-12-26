(ns binaryTree)

(def t1
  {:val 1
   :L {:val 2
       :L {:val 4}
       :R {:val 5}}
   :R {:val 3
       :L {:val 6}
       :R {:val 7}}})

(def t2
  {:val 2
   :L {:val 4}
   :R {:val 5}})

(def t3
  {:val 3
   :L {:val 6}
   :R {:val 7}})

(def t4
  {:val 3
   :L {:val 6}
   :R {:val 7}})

(defn sz [tree]
  (letfn [(tn [t v]
            (cond
              (nil? t) v
              :else (+ v 1 (tn (:L t) 0) (tn (:R t) 0)))
            )]
    (tn tree 0)))

(defn size [t]
  (if (nil? t)
    0
    (+ 1 (size (:L t)) (size (:R t)))))

(defn size-safe [tree]
  (loop [nodes [tree]
         count 0]
    (if (empty? nodes)
      count
      (let [t (first nodes)
            remaining (rest nodes)]
        (if (nil? t)
          (recur remaining count)
          (recur (conj remaining (:L t) (:R t))
                 (inc count)))))))

(defn height [t]
  (if (nil? t)
    0
    (+ 1 (max (height (:L t)) (height (:R t))))))

(defn equal [tree1 tree2]
  (loop [n1 [tree1]
         n2 [tree2]]
    (let [t1 (first n1)
          t2 (first n2)]
      (cond
        (and (empty? n1) (empty? n2))
        true
        
        (and (nil? t1) (nil? t2))
        (recur (rest n1) (rest n2))

        (or (nil? t1) (nil? t2) (not= (:val t1) (:val t2)))
        false

        :else
        (recur (conj (rest n1) (:L t1) (:R t1))
               (conj (rest n2) (:L t2) (:R t2)))))))

(defn post-order [tree]
  (loop [nodes [tree]
         lst '()]
    (if (empty? nodes) 
      lst
      (let [t (first nodes)
            remaining (rest nodes)]
        (if (nil? t)
          (recur remaining lst)
          (recur (conj remaining (:L t) (:R t)) 
                 (conj lst (:val t))))))))

(defn pre-order [tree]
  (loop [nodes (list tree)
         result []]
    (if (empty? nodes)
      (seq result)
      (let [t (first nodes)
            remaining (rest nodes)]
        (if (nil? t)
          (recur remaining result)
          (recur (conj remaining (:R t) (:L t)) 
                 (conj result (:val t))))))))

(defn in-order [tree]
  (loop [curr tree
         stack '()
         result []]
    (cond
      (not (nil? curr))
      (recur (:L curr) (conj stack curr) result)
      
      (not (empty? stack))
      (let [top (first stack)]
        (recur (:R top) 
               (rest stack) 
               (conj result (:val top))))
      
      :else (seq result))))

(defn breadth-first [tree]
  (loop [queue [tree]
         result []] 
    (if (empty? queue)
      (seq result)
      (let [top (first queue)
            q (subvec queue 1)]
        (if (nil? top)
          (recur q result)
          (recur (conj q (:L top) (:R top))
                 (conj result (:val top))))))))
