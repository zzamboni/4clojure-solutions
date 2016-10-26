(defn mcm [& nums]
  (letfn [(multiples-of [n]
            (iterate (partial + n) n))
          (next-in-order [xs]
            (let [min-elem (first (apply min-key second (map-indexed vector (map first xs))))
                  all-others (map second (remove #(= (first %) min-elem) (map-indexed vector xs)))
                  ]
              (lazy-seq (cons (first (nth xs min-elem)) (next-in-order (cons (drop 1 (nth xs min-elem)) all-others)))))) 
          ]
    (ffirst (filter #(= (count %) (count nums)) (partition-by identity (next-in-order (map multiples-of nums)))))))

(def __ mcm)
