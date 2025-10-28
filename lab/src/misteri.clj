(ns misteri)
(defn misteri [n]
  (let [secret 4
        n (+ n 2)]
    (fn [mult]
      (* secret (* mult n)))))
(defn misteri3 [param]
  (fn [bonus]
    (+ (param 6) bonus)))
(let [h (misteri 3)
      j (misteri3 h)
      result (j 2)]
  (println result))