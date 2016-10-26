(defn binary? [t]
  (or (nil? t)
      (and (sequential? t)
           (= (count t) 3)
           (binary? (second t))
           (binary? (last t)))))



(defn symmetric? [t]
  (letfn [(reverse-binary [t]
            (if (nil? t) t
                [(first t)
                 (reverse-binary (last t))
                 (reverse-binary (second t))]))]
    (= (second t) (reverse-binary (last t)))))
