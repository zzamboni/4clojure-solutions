(fn contains-euler-path? [g]
  (letfn [(connected? [graph]
            (let [g (sort-by first graph)]
              ;; The reduce returns the set of nodes that are reachable from the first one,
              ;; so we compare against the total to see if the graph is connected
              (= (count (reduce (fn [seen n]
                                  (if (contains? seen (first n))
                                    (conj seen (second n)) seen))
                                #{(first (first g))} g))
                 (count (distinct (flatten g))))))]
    (let [d (frequencies (flatten g))
          count-odd  (count (filter #(odd? %) (vals d)))]
      ;; From http://www.ctl.ua.edu/math103/euler/howcanwe.htm:
      ;; if the graph is connected and has at most 2 nodes with odd degree,
      ;; then it contains at least one Euler path
      (if (and (connected? g) (<= count-odd 3))
        true
        false))))
