(ns trampolins)

;; (use 'trampolins :reload-all)

(defn fibonacci-cps [n cont]
  (if (< n 2) (cont n)
      (recur (dec n) (fn [fn1] 
                       (fibonacci-cps 
                        (- n 2) 
                        (fn [fn2] 
                          (cont (+ fn1 fn2))))))))

;; (fibonacci-cps 40 identity) 👉 Execution error (StackOverflowError)

(defn fib-t [n]
  (letfn [(fib-cps-t 
           [n k] 
           (if (< n 2) #(k n)
               #(fib-cps-t (dec n) (fn [fn1] (fn [] (fib-cps-t (- n 2) 
                                                (fn [fn2] 
                                                  (fn [] (k (+ fn1 fn2))))))))))]
    (trampoline fib-cps-t n identity)))

(defn fib-ta [n]
  (letfn [(fib-cps-t [n k]
            (if (< n 2)
              #(k n) ; Base case: Safe to use #()

              ;; Recursive step: The outer function must return a thunk
              #(fib-cps-t (dec n)
                          (fn [fn1]
                            (fn [] (fib-cps-t (- n 2)
                                              (fn [fn2]
                                                (fn [] (k (+ fn1 fn2))))))))))]
    (trampoline fib-cps-t n identity)))
;; (time (trampoline fibonacci-cps-t 40 identity))
;; (time (fib-t 40))

(defn fib-fast [n]
  (loop [n n a 0N b 1N] ; a and b are the results for F(n-2) and F(n-1)
    (if (zero? n)
      a
      (recur (dec n) b (+ a b)))))

;; (time (fib-fast 40))
;; (time (fib-fast 1000))

(defn sum-cps [n cont]
  (if (= n 0) 
    (cont 0)
    (recur (dec n) 
           (fn [res] (cont (+ res n))))))

;; (sum-cps 10 identity)

(defn sum-cps-t [n cont]
  (if (zero? n)
    #(cont 0)
    (recur (dec n)
           (fn [res]
             (fn [] (cont (+ res n)))))))

(defn sum-t [n] (trampoline sum-cps-t n identity))