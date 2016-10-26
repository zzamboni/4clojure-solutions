(defn life-iter [b]
  (letfn [(rowlen [b] (count (first b)))
          (boardsize [b] (* (rowlen b) (count b)))

          (board2seq [b]
            (vec (seq (apply concat b))))

          (seq2board [n s]
            (map clojure.string/join (partition n (map str s))))

          (neighbors [b pos]
            (let [bs (board2seq b)
                  rl (rowlen b)]
              (map #(nth bs % nil)
                   (map (partial + pos)
                        [-1 +1 (- rl) (+ rl) (- 0 rl 1) (- 0 rl -1) (+ rl 1) (+ rl -1)]))))

          (nbcount [b pos]
            (count (filter #(= % \#) (neighbors b pos))))

          (new-state [b pos]
            (let [bs (board2seq b)
                  cell (nth bs pos)
                  nb (nbcount b pos)
                  alive (= cell \#)]
              (cond
                (and alive (< nb 2))      \space
                (and alive (<= 2 nb 3))   \#
                (and alive (> nb 3))      \space
                (and (not alive) (= nb 3)) \#
                :else cell)))
          ]
    (seq2board (rowlen b)
               (map (partial new-state b)
                    (range 0 (boardsize b))))))
