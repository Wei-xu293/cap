(ns expressions)

(defn avalua-cps [expr done fail]
  (let [[op a b] expr]
    (case op
      :val (done a)
      
      :div (avalua-cps b 
                       (fn [val-b]
                         (if (zero? val-b)
                           (fail "div0")
                           (avalua-cps a 
                                       (fn [val-a] (done (/ val-a val-b))) fail)))
                       fail)
      
      (let [operation (case op :add + :sub - :mul *)]
        (avalua-cps a 
                    (fn [val-a]
                      (avalua-cps b 
                                  (fn [val-b] (done (operation val-a val-b)))
                                  fail))
                    fail)))))

(defn avalua [expr] (avalua-cps expr
                                (fn [result] result)
                                (fn [err] err)))
