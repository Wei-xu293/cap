(ns misteri)
(defn misteri [n]
  (let [secret 4
        n (+ n 2)] ; n = 5
    (fn [mult]
      (* secret (* mult n))))) ; 4 * 6 * 5 = 120

(defn misteri3 [param] ; param = misteri 3 = h
  (fn [bonus]
    (+ (param 6) bonus))) ; (param 6) = misteri 3 (mult 6) = 120 + 2 = 122

(let [h (misteri 3)
      j (misteri3 h)
      result (j 2)]
  (println result))