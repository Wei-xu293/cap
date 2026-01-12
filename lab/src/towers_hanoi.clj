(ns towers-hanoi)

(defn move [n from to aux]
  (if (= n 1)
    [{:from from :to to}]
    (concat
     (move (dec n) from aux to)
     [{:from from :to to}]
     (move (dec n) aux to from))))

(defn torres-de-hanoi [n]
  (move n "A" "C" "B"))

(defn move-cps [n from to aux cont]
  (if (= n 1) 
    (cont [{:from from :to to}])
      (recur (dec n) from aux to 
             (fn [res1]
               (move-cps (dec n) aux to from 
                         (fn [res2] (cont (concat res1
                                                  [{:from from :to to}]
                                                  res2))))))))
(defn hanoi-cps [n]
  (move-cps n "A" "C" "B" identity))

(defn move-t [n from to aux cont]
  (if (= n 1)
    #(cont [{:from from :to to}])
    (recur (dec n) from aux to
            (fn [res1]
              #(move-t (dec n) aux to from
                       (fn [res2] 
                         (fn [] 
                           (cont (concat res1 
                                         [{:from from :to to}] 
                                         res2)))))))))

(defn towers-of-hanoi-t [n] (trampoline move-t n "A" "C" "B" identity))

(comment
  ;; (use 'towers-hanoi :reload-all)
  ;; (torres−de−hanoi 3)
  ;; (towers-of-hanoi-t 3)
  )