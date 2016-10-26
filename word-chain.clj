(fn word-chain? [words]  ; use observed heuristic: in at least n-1 rows of the full Levenshtein distance matrix
                                        ; there must be at least 2 connections with distance 1
  (letfn [(levenshtein-distance  ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive
            ([s1 s2] (levenshtein-distance s1 (count s1) s2 (count s2)))
            ([s1 l1 s2 l2]
             (cond
               (zero? l1) l2
               (zero? l2) l1
               :else (let [cost (if (= (nth s1 (dec l1)) (nth s2 (dec l2))) 0 1)]
                       (min (inc (levenshtein-distance s1 (dec l1) s2 l2))
                            (inc (levenshtein-distance s1 l1 s2 (dec l2)))
                            (+ (levenshtein-distance s1 (dec l1) s2 (dec l2)) cost))))))
          (levenshtein-matrix [words]  ; levenshtein distance matrix represented as a map of maps {word1 {word2 distance}}
            (apply merge-with merge
                   (apply concat
                          (map (fn [w]
                                 (map (fn [x]
                                        {w {x (levenshtein-distance w x)}}) words))
                               words))))]
    (>= (count (filter #(> % 1)
                       (map (fn [x] (->> x second vals (filter #(= 1 %)) count))
                            (levenshtein-matrix words))))
        (dec (count words)))))
