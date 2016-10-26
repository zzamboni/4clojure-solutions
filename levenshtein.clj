(defn foo [& args]
  (letfn [(levenshtein-distance  ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive
                                        ; The hard part was to get the memoization to work when using an anonymous function
                                        ; as required by 4clojure, this is why I'm passing a function as the first argument.
            ([memf s1 s2] (memf memf s1 (count s1) s2 (count s2)))
            ([memf s1 l1 s2 l2]
             (cond
               (zero? l1) l2
               (zero? l2) l1
               :else (let [cost (if (= (nth s1 (dec l1)) (nth s2 (dec l2))) 0 1)]
                       (min (inc (memf memf s1 (dec l1) s2 l2))
                            (inc (memf memf s1 l1 s2 (dec l2)))
                            (+ (memf memf s1 (dec l1) s2 (dec l2)) cost))))))]
    (let [levmem (memoize levenshtein-distance)]
      (apply levmem (cons levmem args)))))

(def __ levenshtein-distance)
