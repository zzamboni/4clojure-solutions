(defn pascal-row [n]
  (condp = n
    1 [1]
    2 [1 1]
    (flatten [1 (let [p (pascal-row (dec n))]
                  (map-indexed (fn [i v] (+ v (nth p (inc i)))) (butlast p))) 1])))

(defn pascal-row2 [n]
  (condp = n
    1 [1]
    2 [1 1]
    (flatten [1 (map #(apply + %)
                     (partition 2 1 (pascal-row2 (dec n))))
              1])))
