;; #21
#(first (drop %2 %1))

;; #22
(fn [s] (reduce (fn [a b] (inc a)) 0 (seq s)))

;; #23
(fn [s] (reduce (fn [a b] (cons b a)) (empty s) s))

;; #26
#(letfn [(fib [x]
           (if (< x 3)
             1
             (+ (fib (- x 1)) (fib (- x 2)))))]
   (for [n (range 1 (inc %))]
     (fib n)))

;; #28
(fn my-flatten [[s1 & xs]]
  (when s1
    (if (coll? s1)
      (concat (my-flatten s1) (my-flatten xs))
      (conj (my-flatten xs) s1))))

(fn my-flatten [s]
  (filter #(not (coll? %)) (tree-seq coll? seq s)))

;; #29
#(clojure.string/join (re-seq #"[A-Z]" %))

;; #30
#(reduce (fn [a b] (if (= (last a) b) a (concat a (list b)))) '() %)
#(map first (partition-by identity %))

;; #31
(partial partition-by identity)

;; #32
#(interleave % %)

;; #33
#(mapcat (partial repeat %2) %1)

;; #34
(fn my-range [a b] (if (< a b) (lazy-seq (concat (list a) (my-range (inc a) b))) '()))
(fn my-range [a b] (take-while #(< % b) (iterate inc a)))

;; #38
(fn [& s] (reduce #(if (> %2 %) %2 %) s))

;; #39
(fn my-interleave [s1 s2] (mapcat list s1 s2))

;; #40
(fn [sep s] (butlast (reduce #(conj %1 %2 sep) [] s)))
#(butlast (mapcat list %2 (repeat %)))

;; #41
#(->> %1
      (partition-all (dec %2) %2)
      (flatten))
;; Better, will correctly handle lists of lists
#(->> %1
      (partition-all (dec %2) %2)
      (apply concat))

;; #42
#(reduce * (range 1 (inc %)))

;; #43
#(apply (partial map list) (partition-all %2 %1))

;; #44
(fn my-rotate [n s]
  (apply concat
         (reverse (split-at (mod n (count s)) s))))

;; Equivalent, maybe more readable?
#(->> %2
      (split-at (mod % (count %2)))
      reverse
      (apply concat))

;; #46
#(fn [& args] (apply % (reverse args)))

;; #49
#(list (take %1 %2) (drop %1 %2))

;; #50
#(vals (group-by type %))

;; #53
(fn [s]
  (or
   (first
    (filter
     #(> (count %) 1)
     (sort-by count >
              (reduce
               (fn red [a b]
                 (let [curseq (last a)
                       lastnum (last curseq)
                       restseq (or (butlast a) '())]
                   (if (and (not (nil? lastnum))
                            (> b lastnum))
                     (concat restseq [(concat (last a) [b])])
                     (concat a [[b]])))) [] s)))) []))

;; Shorter, more idiomatic
(fn longest-subseq [s]
  (let [l (count s)]
    (or (first
         (sort-by count >
                  (filter #(every? (partial apply <) (partition 2 1 %))
                          (apply concat (map (fn [n] (map #(take % (drop n s))
                                                          (range 2 (inc (- l n)))))
                                             (range (dec l))))))) [])))

;; #54
(fn my-part [n s]
  (filter #(= (count %) n)
          (map #(map second %)
               (vals (group-by #(quot (first %) n)
                               (map-indexed (fn [i x] [i x]) s))))))

(fn my-part [n s] 
  (filter #(= n (count %))
          (reduce (fn [v e] (update-in v [(first e)] #(concat % (second e)))) []
                  (map-indexed (fn [i x] [(quot i n) [x]]) s))))

;; #55
#(reduce (fn [m x] (into m {x (inc (get m x 0))})) {} %)
#(reduce (fn [m x] (merge-with + m {x 1})) {} %)

;; #56
#(reduce (fn [s x]
           (if (some (set s) [x])
             s (conj s x)))
         [] %)

;; #58
(fn my-comp
  ([fun1]        (fn [& args] (apply fun1 args)))
  ([fun1 & funs] (fn [& args] (apply fun1 [(apply (apply my-comp funs) args)]))))

(fn my-comp [& fns]
  (fn [& args] (first (reduce #(vector (apply %2 %)) args (reverse fns)))))

;; #59
(fn my-juxt [& funs]
  (fn [& args] (map #(apply % args) funs)))

;; #60
(fn my-reductions1
  ([fun coll] (my-reductions1 fun (first coll) (rest coll)))
  ([fun init coll] (cons init (map-indexed (fn [n x] (reduce fun init (take (inc n) coll))) coll))))

;; Maybe better performance, not so much reevaluation
(fn my-reductions2
  ([fun coll] (my-reductions2 fun (first coll) (rest coll)))
  ([fun init coll] (cons init (lazy-seq (when (seq coll) (my-reductions2 fun (fun init (first coll)) (rest coll)))))))

;; #61
(fn __ [k v] (into {} (map #(hash-map %1 %2) k v)))

;; #62
(fn my-iterate [fun init] (lazy-seq (cons init (my-iterate fun (fun init)))))
(fn my-iterate [fun init] (cons init (lazy-seq (my-iterate fun (fun init)))))

;; #63
(fn my-group-by [fun seq]
  (apply merge-with concat
         (map #(hash-map (fun %) [%]) seq)))
;; #65
(fn type-detect [obj]
  (if (and (associative? obj) (not (reversible? obj)))         :map
      (if (and (associative? obj) (reversible? obj))           :vector
          (if (= 1 (count (into (empty obj) [:fizz :fizz])))   :set
              (if (= :second (first (into (empty obj) [:first :second]))) :list)))))

;; Same logic, more readable
(fn type-detect [obj]
  (cond
    (and (associative? obj) (not (reversible? obj)))        :map
    (and (associative? obj) (reversible? obj))              :vector
    (= 1 (count (into (empty obj) [:fizz :fizz])))          :set
    (= :second (first (into (empty obj) [:first :second]))) :list))

;; #66
(fn my-gcd [a b] (let [x (min a b)] (apply max (filter #(and (zero? (rem a %)) (zero? (rem b %))) (range 1 (inc x))))))
(fn [a b] (apply max (filter #(zero? (+ (mod a %) (mod b %))) (range 1 (inc (min a b))))))

;; #67
(fn first-primes [n]
  (letfn [(prime? [x] (every? #(not= 0 (mod x %)) (range 2 x)))]
    (->> (range)
         (drop 2)
         (filter prime?)
         (take n))))

;; #69
(fn my-merge-with [fun & maps]
  (reduce (fn [m1 m2]
            (reduce-kv (fn [init k v]
                         (if (contains? init k)
                           (assoc init k (fun (get init k) v))
                           (assoc init k v)))
                       m1 m2))
          maps))

;; Same logic, a bit shorter
(fn my-merge-with [fun & maps]
  (reduce (fn [m1 m2]
            (reduce (fn [m1 [k v]]
                      (if (contains? m1 k)
                        (assoc m1 k (fun (m1 k) v))
                        (assoc m1 k v)))
                    m1 m2))
          maps))

;; #70
#(sort-by clojure.string/lower-case (clojure.string/split % #"\W+"))

;; #73
(fn who-won? [board]
  (let [all-equal? (fn [s] (reduce #(if (= %1 %2) %1 nil) s))
        lines (conj board ; original board
                    (map first board) (map second board) (map last board) ; columns
                    [(first (first board)) (second (second board)) (last (last board))] ; first diagonal
                    [(last (first board)) (second (second board)) (first (last board))] ; second diagonal
                    )
        result (some all-equal? lines)
        ]
    (if (= result :e) nil result)))

;; Slightly shorter, more idiomatic
(fn who-won? [board]
  (let [fns [first second last]
        all-equal? (fn [s] (reduce #(if (= %1 %2) %1 nil) s))
        lines (concat board ; rows
                      (map #(map % board) fns) ; columns
                      [(map #(% %2) fns board) (map #(% %2) (reverse fns) board)]) ; diagonals
        result (some all-equal? lines)]
    (if (#{:e} result) nil result)))

;; #74
(fn [s]
  (->> (clojure.string/split s #",")
       (map read-string)
       (filter #(= % (int (Math/pow (int (Math/sqrt %)) 2))))
       (map str)
       (clojure.string/join ",")))

(fn [s]
  (->> (clojure.string/split s #",")
       (map read-string)
       (filter #(let [q (Math/sqrt %)] (== q (int q))))
       (interpose ",")
       (apply str)))

;; #75
(fn totient [n]
  (letfn [(gcd [a b] (if (zero? b) a (recur b (mod a b))))
          (coprime? [a b] (= (gcd a b) 1))]
    (count (filter #(coprime? % n) (range 1 (inc n))))))

;; #77
(fn anagrams [s]
  (->> s
       (group-by sort)
       (vals)
       (filter #(< 1 (count %)))
       (map set) set))

;; #78
(fn my-trampoline [f & args]
  (loop [ret (apply f args)]
    (if (fn? ret)
      (recur (ret))
      ret)))

;; #79
(fn shortest-path-length [t]
  (letfn [(first-index [row]
            (if (zero? row) 0 (+ (first-index (dec row)) row)))
          (row-of [idx]
            (some #(and (<= (first-index %) idx (dec (first-index (inc %)))) %) (range)))
          (go-right [idx] (+ idx (inc (row-of idx)) 1))
          (go-left [idx] (+ idx (inc (row-of idx))))
          (all-paths
            ([t]
             (all-paths t 0))
            ([t idx]
             (if (>= idx (count t)) '(())
                 (distinct (map #(cons (nth t idx) %)
                                (concat (all-paths t (go-left idx))
                                        (all-paths t (go-right idx))))))))]
    (apply min (map (partial apply +) (all-paths (flatten t))))))

;; Shorter, compute the sums directly instead of the paths
(fn shortest-path-length [t]
  (letfn [(path-sums [t idx]
            (if (>= (first idx) (count t)) [0]
                (map #(+ (get-in (vec t) idx) %)
                     (concat (path-sums t (map + idx [1 0]))
                             (path-sums t (map + idx [1 1]))))))]
    (apply min (path-sums t [0 0]))))

;; #80
(fn perfect? [n]
  (= n (apply + (filter #(zero? (rem n %)) (range 1 n)))))

;; #81
(fn my-intersection [s1 s2]
  (reduce (fn [r s] (if (contains? s1 s) (conj r s) r)) #{} s2))

(fn my-intersection [s1 s2]
  (set (filter s1 s2)))

;; #82
(fn word-chain? [words]
  ;; use observed heuristic: in at least n-1 rows of the full Levenshtein distance matrix
  ;; there must be at least 2 connections with distance 1
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

;; Same algorithm, more concise version
(fn word-chain? [words]
  (letfn [(l-dist [s1 s2]  ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive 
            (let [l1 (count s1) l2 (count s2) dl1 (drop-last s1) dl2 (drop-last s2)]
              (cond (zero? l1) l2
                    (zero? l2) l1
                    :else (min (inc (l-dist dl1 s2)) (inc (l-dist s1 dl2))
                               (+ (l-dist dl1 dl2) (if (= (last s1) (last s2)) 0 1))))))]
    (let [l-matrix  ; levenshtein distance matrix represented as a map of maps {word1 {word2 distance}}
          (apply merge-with merge (for [w words x words] {w {x (l-dist w x)}}))]
      ;; observed heuristic: in at least n-1 rows of the full Levenshtein distance matrix
      ;; there must be at least 2 connections with distance 1
      (<= (dec (count words))
          (count (filter #(>= % 2)
                         (map #(->> % second vals (filter #{1}) count) l-matrix)))))))

;; #83
(fn half-truth? [& args]
  (let [trues (count (filter true? args))]
    (< 0 trues (count args))))

#(< 0 (count (filter true? %&)) (count %&))

;; #84
(fn transitive-closure [r]
  (let [new-r (reduce (fn [res v]
                        (let [newitems (map #(vector (first v) (second %)) (filter #(= (first %) (second v)) res))]
                          (if (seq newitems)
                            (apply conj res newitems)
                            res))) r r)]
    (if (= new-r r) new-r (transitive-closure new-r))))

;; More idiomatic
(fn transitive-closure [r]
  (let [new-r (into r (for [[w x] r [y z] r :when (= x y)]
                        [w z]))]
    (if (= new-r r) new-r (recur new-r))))

;; #85
(fn power-set [s]
  (let [nres (count s)
        nset (Math/pow 2 (count s))
        sseq (seq s)
        n-to-set (fn [n] (reduce (fn [res digit] (if (bit-test n digit) (conj res (nth sseq digit)) res)) #{} (range nres)))]
    (into #{} (map #(n-to-set %) (range nset)))))

;; Same algorithm, more concise
(fn power-set [s]
  (let [n-to-set #(set (remove nil? (map-indexed (fn [n e] (when (bit-test % n) e)) s)))]
    (set (map n-to-set (range (Math/pow 2 (count s)))))))

;; #86
(fn happy-number? [n]
  (letfn [(sum-square-digits [n] (apply + (map (comp #(* % %) read-string str) (seq (str n)))))]
    (loop [cur (sum-square-digits n) seen (hash-set n)]
      (cond (= cur 1)  true
            (seen cur) false
            :else      (recur (sum-square-digits cur) (conj seen cur))))))

;; #88
;; Using stdlib
(fn symm-diff [a b] (let [d clojure.set/difference] (clojure.set/union (d a b) (d b a))))
;; Manually
(fn symm-diff [a b] (set (filter #(and ((into a b) %) (not (and (a %) (b %)))) (into a b))))
(fn symm-diff [a b] (set (concat (filter (complement a) b) (filter (complement b) a))))

;; #89
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

;; Same algorithm, MUCH more concise
(fn contains-euler-path? [g]
  (let [g (sort-by first g) f (flatten g)
        connected? (= (count (reduce (fn [seen [n m]] (if (seen n) (conj seen m) seen)) #{(ffirst g)} g))
                      (count (distinct f)))
        count-odd  (count (filter #(odd? %) (vals (frequencies f))))]
    ;; From http://www.ctl.ua.edu/math103/euler/howcanwe.htm:
    ;; if the graph is connected and has at most 2 nodes with odd degree,
    ;; then it contains at least one Euler path
    (and connected? (<= count-odd 3))))

;; #90
#(set (for [x % y %2] [x y]))

;; #91
;; Original solution
(fn connected? [graph]
  ;; augment graph with reverse edges, and sort by source node to effectively produce a breadth-first traversal
  (let [g (sort-by first (concat graph (map reverse graph)))]
    ;; The reduce returns the set of nodes that are reachable from the first one,
    ;; so we compare against the total to see if the graph is connected
    (= (count (reduce (fn [seen n]
                        (if (contains? seen (first n))
                          (conj seen (second n)) seen))
                      #{(first (first g))} g))
       (count (distinct (flatten g))))))

;; Much more concise
(fn connected? [g]
  (let [g (sort-by first (concat g (map reverse g)))]
    (= (count (reduce (fn [seen [n m]] (if (seen n) (conj seen m) seen)) #{(ffirst g)} g))
       (count (distinct (flatten g))))))

;; #92
(fn roman-to-num [s]
  (let [; we invent some digits N-S to represent valid substractive-principle numbers
        roman-digits (zipmap "IVXLCDMNOPQRS" [1 5 10 50 100 500 1000 4 9 40 90 400 900])
        subsnum {"IV" "N" "IX" "O" "XL" "P" "XC" "Q" "CD" "R" "CM" "S"}]
    (apply + (replace roman-digits
                      (reduce (fn [s [a b]] (clojure.string/replace s a b)) s subsnum)))))

;; #93
;; Original
(fn partially-flatten [s]
  (reduce (fn [v e]
            (if (sequential? e)
              (if (sequential? (first e))
                (concat v (partially-flatten e))
                (concat v [e]))
              v))
          [] s))

;; Same algorithm, more concise
(fn partially-flatten [s]
  (reduce #(concat % (when (coll? %2) (if (coll? (first %2)) (partially-flatten %2) [%2]))) [] s))

;; Using library
(fn partially-flatten [s]
  (->> s (tree-seq coll? seq)
       (filter #(and (coll? %) (every? (complement coll?) %)))))

;; #94
;; First version
(fn life-iter [b]
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

;; Shorter version
(fn life-iter [b]
  (letfn [(new-state [pos]
            (let [cell (get-in b pos) alive (= cell \#)
                  nb (count (filter #{\#} (map #(get-in b % nil)
                                               (map (partial map + pos)
                                                    [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))))]
              (cond (and alive (< nb 2))       \space
                    (and alive (> nb 3))       \space
                    (and alive (<= 2 nb 3))    \#
                    (and (not alive) (= nb 3)) \#
                    :else cell)))]
    (map #(apply str %)
         (reduce #(assoc-in % %2 (new-state %2)) (mapv vec b)
                 (for [x (range (count b)) y (range (count (first b)))] [x y])))))

;; #95
(fn binary? [t]
  (or (nil? t)
      (and (sequential? t)
           (= (count t) 3)
           (every? binary? (rest t)))))

;; #96
(fn symmetric? [t]
  (letfn [(reverse-binary [t] (and t [(first t) (reverse-binary (last t)) (reverse-binary (second t))]))]
    (= t (reverse-binary t))))

;; #97
(fn pascal-row [n]
  (condp = n
    1 [1]
    2 [1 1]
    (flatten [1
              (map #(apply + %)
                   (partition 2 1 (pascal-row (dec n))))
              1])))

(fn pascal-row [n]
  (if (= n 1) [1] 
      (map #(apply + %) (partition 2 1 (concat [0] (pascal-row (dec n)) [0])))))

;; #98
(fn eclasses [f D]
  (set (map set (vals (group-by f D)))))

;; #99
#(map (comp read-string str) (seq (str (* %1 %2))))

;; #100
(fn mcm [& nums]
  (letfn [(multiples-of [n]
            (iterate (partial + n) n))
          (next-in-order [xs]
            (let [min-elem (first (apply min-key second (map-indexed vector (map first xs))))
                  all-others (map second (remove #(= (first %) min-elem) (map-indexed vector xs)))
                  ]
              (lazy-seq (cons (first (nth xs min-elem)) (next-in-order (cons (drop 1 (nth xs min-elem)) all-others)))))) 
          ]
    (ffirst (filter #(= (count %) (count nums)) (partition-by identity (next-in-order (map multiples-of nums)))))))

;; Same algorithm, somewhat clearer
(fn mcm [& nums]
  (letfn [(multiples-of [n] (iterate (partial + n) n))
          (next-in-order [xs]
            (let [os (sort-by first xs)]
              (cons (ffirst os) (lazy-seq (next-in-order (cons (rest (first os)) (rest os)))))))]
    (->> nums
         (map multiples-of)
         next-in-order
         (partition-by identity)
         (filter #(= (count %) (count nums)))
         ffirst)))

;; Effectively same algorithm, but much shorter
(fn mcm [n & nums]
  (let [m (iterate (partial + n) n)]
    (first (filter (fn [x]
                     (every? #(zero? (mod x %)) nums)) m))))

;; #101
(fn foo [& args]
  (let [l-dist (memoize
                (fn [memf s1 s2]  ; recursive algorithm from https://en.wikipedia.org/wiki/Levenshtein_distance#Recursive
                  ;; Same as from #82 but takes as first argument a memoized version of itself, which is used in the recursive calls
                  (let [l1 (count s1) l2 (count s2) dl1 (drop-last s1) dl2 (drop-last s2) l-dist (partial memf memf)]
                    (cond (zero? l1) l2
                          (zero? l2) l1
                          :else (min (inc (l-dist dl1 s2)) (inc (l-dist s1 dl2))
                                     (+ (l-dist dl1 dl2) (if (= (last s1) (last s2)) 0 1)))))))]
    (apply l-dist l-dist args)))

;; #102
(fn intoCamelCase [s]
  (let [[fw & rw] (clojure.string/split s #"-")]
    (apply str (cons fw (map clojure.string/capitalize rw)))))

;; #103
(defn kcomb
  ([n coll] (set (flatten(kcomb n (seq coll) #{}))))
  ([n coll so-far]
   (if (zero? n)
     so-far
     (map #(kcomb (dec n) (drop (inc %) coll) (conj so-far (nth coll %)))
          (range (count coll))))))

;; #104
(fn num-to-roman [n]
  (let [roman-digits (zipmap [900  400   90   40   9    4  1000 500 100 50  10   5   1]
                             ["CM" "CD" "XC" "XL" "IX" "IV" "M" "D" "C" "L" "X" "V" "I"])]
    (first
     (reduce (fn [res d]
               (let [x (quot (second res) d)
                     s (str (first res) (apply str (repeat x (get roman-digits d))))]
                 [s, (- (second res) (* d x))]))
             ["", n] (reverse (sort (keys roman-digits)))))))

;; Slightly rewritten
(fn num-to-roman [n]
  (let [digits (zipmap [900  400   90   40   9    4  1000 500 100 50  10   5   1]
                       ["CM" "CD" "XC" "XL" "IX" "IV" "M" "D" "C" "L" "X" "V" "I"])]
    (apply str (first (reduce (fn [[res n] d]
                                [(concat res (repeat (quot n d) (digits d))) (rem n d)])
                              [[] n] (sort > (keys digits)))))))

;; #105
(fn sep-keywords [s]
  (loop [resto s
         k nil
         res {}]
    (cond
      (empty? resto) res
      (keyword? (first resto)) (recur (rest resto) (first resto) (assoc res (first resto) []))
      :else (recur (rest resto) k (assoc res k (conj (get res k) (first resto)))))))

;; Shorter, recursive
(fn sep-keywords [s]
  (if (empty? s) {}
      (merge {(first s) (take-while (complement keyword?) (rest s))}
             (sep-keywords (drop-while (complement keyword?) (rest s))))))

;; Non recursive
(fn sep-keywords [xs]
  (loop [res {} [k & s] xs]
    (if (nil? k) res
        (let [[s1 s2] (split-with (complement keyword?) s)]
          (recur (assoc res k s1) s2)))))

;; #106
(fn number-maze
  ([n1 n2] (number-maze [n1] n2 1))
  ;; Do a breadth-first search, at each level calculating all
  ;; the results of applying the transformations on each number
  ([c1 n2 x]
   (let [ts [(partial * 2) #(/ % 2) (partial + 2)]]
     (if (some #{n2} c1) x
         (number-maze (for [f ts x c1] (f x)) n2 (inc x))))))

(fn number-maze [n1 n2]
  ;; Generate the sequence of all numbers in each level of the tree
  ;; (as in a breadth-first search), and count how many
  ;; levels until the target number appears
  ((comp inc count take-while)
   #(not (some #{n2} %))
   (iterate
    #(for [f [(partial * 2) (partial * (/ 2)) (partial + 2)]
           x %]
       (f x)) [n1])))

(fn number-maze [n1 n2]
  (->> n1 vector
       (iterate #(for [f [(partial * 2) (partial * (/ 2)) (partial + 2)] x %] (f x)))
       (take-while #(not (some #{n2} %)))
       count inc))

;; #107
(fn pown [n] #(reduce * (repeat n %)))

;; #108
(fn lazy-search [& xs]
  (letfn [(next-in-order [xs]
            (let [min-elem (first (apply min-key second (map-indexed vector (map first xs))))
                  all-others (map second (remove #(= (first %) min-elem) (map-indexed vector xs)))
                  ]
              (lazy-seq (cons (first (nth xs min-elem)) (next-in-order (cons (drop 1 (nth xs min-elem)) all-others)))))) 
          ]
    (ffirst (filter #(= (count %) (count xs)) (partition-by identity (next-in-order xs))))))

(fn lazy-search [& xs]
  (letfn [(next-in-order [xs]
            (let [[fs & rs] (sort-by first xs)]
              (cons (first fs) (lazy-seq (next-in-order (cons (rest fs) rs))))))]
    (->> xs next-in-order
         (partition-by identity)
         (filter #(= (count %) (count xs)))
         ffirst)))

;; No need to define next-in-order, can be handled implicitly
(fn lazy-search [& s]
  (loop [s s] (if (apply = (map first s)) (ffirst s)
                  (let [[fs & rs] (sort-by first s)]
                    (recur (cons (rest fs) rs))))))

;; #110
(defn pronounce [x] (flatten (map (fn [e] [(count e) (first e)]) (partition-by identity x))))

(fn pronounce-seq [xs]
  (let [pronounce #(flatten (map (juxt count first) (partition-by identity %)))]
    (rest (iterate pronounce xs))))

;; #111
(fn crossword-finder [w b]
  (let [r clojure.string/replace b (map #(r (r % #" " "") #"_" ".") b)]
    (boolean (some #(re-find % w)
                   (map #(re-pattern (str "^" % "$")) 
                        (flatten (map #(clojure.string/split % #"#")
                                      (concat b (map #(apply str %) 
                                                     (apply map vector (map seq b)))))))))))

;; #112
(fn sequs
  ([l s] (sequs l s [] 0))
  ([l s res t]
   (if (empty? s) res
       (let [e (first s)]
         (if (number? e)
           (let [t2 (+ t e)]
             (if (<= t2 l)
               (sequs l (rest s) (conj res e) t2)
               res))
           (conj res (sequs l e [] t)))))))

;; Shorter using destructuring and some refactoring
(fn sequs
  ([l s] (sequs l s [] 0))
  ([l [e & r] res t]
   (cond (nil? e) res
         (number? e) (let [t2 (+ t e)]
                       (if (> t2 l) res
                           (sequs l r (conj res e) t2)))
         :else (conj res (sequs l e [] t)))))

;; #113
(fn data-dance [ & args ]
  (reify clojure.lang.Seqable
    (toString [this] (clojure.string/join ", " (sort args)))
    (seq [this] (if (empty? args) nil (distinct args)))))

;; #114
(fn global-take-while [n p s]
  (let [[x y] (split-with (complement p) s)]
    (if (= n 1)
      x
      (lazy-cat x (cons (first y)
                        (global-take-while (dec n) p (rest y)))))))

;; not recursive
(fn global-take-while [n p s]
  (loop [n n s s res []]
    (let [[x y] (split-with (complement p) s)]
      (if (= n 1) (concat res x)
          (recur (dec n) (rest y) (concat res x [(first y)]))))))

;; #115
(fn balanced? [n]
  (let [d (map (comp read-string str) (seq (str n)))
        n1 (quot (count d) 2)]
    (= (apply + (take n1 d)) (apply + (take-last n1 d)))))

;; #116
(fn balanced-prime? [n]
  (letfn [(prime? [x] (every? #(not= 0 (mod x %)) (range 2 x)))]
    (let [[earlier [l1 l2 & _]] (split-with #(< % n) (->> (range) (drop 2) (filter prime?)))]
      (and (> n 2) (= l1 n) (= n (/ (+ (last earlier) l2) 2))))))

;; #117
(fn solve-maze [g]
  (letfn [(coord [c] (first (filter #(>= (second %) 0) (map-indexed #(vector %1 (.indexOf %2 c)) g))))
          (up    [p] (update-in p [0] #(max 0 (dec %))))
          (down  [p] (update-in p [0] #(min (dec (count g)) (inc %))))
          (left  [p] (update-in p [1] #(max 0 (dec %))))
          (right [p] (update-in p [1] #(min (dec (count (first g))) (inc %))))
          (allowed [seen p] (not (or (some #{p} seen) (= (get-in g p) \#))))]
    (let [mouse (coord "M")
          cheese (coord "C")
          moves [up down left right]]
      (loop [cur mouse
             seen []
             next []]
        (let [seen2 (conj seen cur)
              next2 (concat next (filter (partial allowed seen2) (map #(% cur) moves)))]
          (cond
            (= cur cheese) true
            (empty? next2) false
            :else (recur (last next2) seen2 (butlast next2))))))))

(fn solve-maze [g]
  (letfn [(coord [c] (->> g (map-indexed #(vector %1 (.indexOf %2 c))) (remove (comp neg? second)) first))
          (it [t n] (min (dec (count t)) (inc n))) (dt [n] (max 0 (dec n)))
          (up    [p] (update-in p [0] dt))
          (left  [p] (update-in p [1] dt))
          (down  [p] (update-in p [0] #(it g %)))
          (right [p] (update-in p [1] #(it (first g) %)))
          (allowed [seen p] (not (or (some #{p} seen) (= (get-in g p) \#))))]
    (let [mouse (coord "M") cheese (coord "C") moves [up down left right]]
      (loop [cur mouse seen [] next []]
        (let [seen (conj seen cur)
              next (concat next (filter (partial allowed seen) (map #(% cur) moves)))]
          (cond (= cur cheese) true
                (empty? next)  false
                :else          (recur (last next) seen (butlast next))))))))

;; #118
(fn my-map2 [f s]
  (when (not (empty? s))
    (lazy-seq (cons (f (first s)) (my-map2 f (rest s))))))

(fn my-map [f s]
  (reductions #(f %2) (f (first s)) (rest s)))

;; #119
(fn win-tic-tac-toe [play b]
  (letfn [(who-won? [board] ;; From Ex. 73, determine if a board has a win position
            (let [all-equal? (fn [s] (reduce #(if (= %1 %2) %1 nil) s))
                  lines (conj board ; original board
                              (map first board) (map second board) (map last board) ; columns
                              [(ffirst board) (second (second board)) (last (last board))] ; first diagonal
                              [(last (first board)) (second (second board)) (first (last board))] ; second diagonal
                              )
                  result (some all-equal? lines)]
              (if (= result :e) nil result)))
          (empties []  ;; Return coordinates of all the empty positions in the board
            (filter #(not (nil? %)) (apply concat
                                           (map (fn [[x row]] (map-indexed (fn [y e] (when (= e :e) [x y])) row))
                                                (filter #(<= 0 (.indexOf (second %) :e)) (map-indexed vector b))))))
          (update-cell [cell val] (update-in b cell (fn [_] val)))]
    ;; Generate boards with all empties replaced with `play', and check all of them for
    ;; winning positions.
    (set (map first (filter #(second %)
                            (map (fn [[cell board]] [cell (who-won? board)])
                                 (map (fn [cell] [cell (update-cell cell play)])
                                      (empties))))))))

(fn win-tic-tac-toe [play b]
  (letfn [(who-won? [board]
            (let [fns [first second last]
                  all-equal? (fn [s] (reduce #(if (= %1 %2) %1 nil) s))
                  lines (concat board (map #(map % board) fns) [(map #(% %2) fns board) (map #(% %2) (reverse fns) board)])
                  result (some all-equal? lines)]
              (if (#{:e} result) nil result)))]
    (let [r (range 3) empties (for [x r y r :when (= :e (get-in b [x y]))] [x y])]
      (->> empties
           (map (fn [cell] [cell (who-won? (assoc-in b cell play))]))
           (filter second) (map first) set))))

;; #120
(defn sq-of-digits [n]
  (apply + (map #(* % %) (map read-string (map str (seq (str n)))))))

(fn filter-sq-digits [s]
  (letfn [(sqd [n] (->> n str seq (map (comp #(* % %) read-string str)) (apply +)))]
    (count (filter #(< % (sqd %)) s))))

;; #121
(fn ucc [form]
  (fn [args] (clojure.walk/postwalk #(if (seq? %) (apply ({'+ + '- - '/ / '* *} (first %)) (rest %)) %)
                                    (clojure.walk/postwalk-replace args form))))

;; Ex. 122 - different ways to read a binary number
;; Manual way
(defn read-binary [s]
  (reduce (fn [r e] (bit-or (bit-shift-left r 1) e)) (map #(- (int %) (int \0)) (seq s))))

;; Rewritten using ->> for clarity
(fn read-binary [s]
  (->> s
       seq
       (map #(- (int %) (int \0)))
       (reduce (fn [r e] (bit-or (bit-shift-left r 1) e)))))

;; Through java 
#(Integer/parseInt % 2)

;; Through read-string
#(read-string (str "2r" %))

;; #124
(fn analyze-reversi [b c]
  (let [r1 [-1 0 1] r4 [0 1 2 3]]
    (letfn [(is [col cell] (= (get-in b cell) col))
            (opposite [col] ({'b 'w 'w 'b} col))
            (empties [] (filter (partial is 'e) (for [y r4 x r4] [y x])))
            (directions [] (remove #{[0 0]} (for [x r1 y r1] [x y])))
            (line-from [cell dir] (take-while (fn [p] (every? #(<= 0 % 3) p)) (iterate (fn [x] (map + x dir)) cell)))]
      (->>
       ;; Find all empty cells
       (empties)
       ;; Find the lines from each of them in all directions and put them together in a list
       (map (fn [cell] (map #(line-from cell %) (directions))))
       (apply concat)
       ;; Get the color sequence on each line, and split it according to the opponent's color
       (map (fn [line] [line (split-with #(= % (opposite c))
                                         (rest (map (partial get-in b) line)))]))
       (filter   ;; Filter only those lines where...
        (fn [[line [c1 c2]]] (and (> (count c1) 0)  ;; there's at least one pieces of the opposite color...
                                  (every? (partial = (opposite c)) c1)
                                  (> (count c2) 0)  ;; and there's a pieces of our color right after them
                                  (= c (first c2)))))
       ;; Finally, get the coordinates of the affected opposite pieces
       (map
        (fn [[[move & coords] [affected & _]]] {move (set (take (count affected) coords))}))
       ;; and store it all in a map as requested.
       (into {})))))

(fn analyze-reversi [b c]
  (let [r1 [-1 0 1] r4 [0 1 2 3] opposite {'b 'w 'w 'b}]
    (letfn [(clr [c] (get-in b c))
            (is [col cell] (= (clr cell) col)) 
            (line-from [cell dir] (take-while (fn [p] (every? #(<= 0 % 3) p)) (iterate #(map + % dir) cell)))]
      (let [empties (for [y r4 x r4 :when (is 'e [y x])] [y x])
            directions (remove #{[0 0]} (for [x r1 y r1] [x y]))]
        (->> (for [e empties d directions] (line-from e d)) ;; Lines from every empty cell in every direction
             ;; split according to opposite color
             (map (fn [line] [line (split-with #(= % (opposite c)) (rest (map clr line)))]))
             ;; leave those that have at least one opposite cell, and one of ours at the end
             (filter (fn [[line [c1 [c2 & _]]]] (and (seq c1) (= c c2))))
             ;; get the coordinates of the affected cells, and put everything in a map
             (map (fn [[[move & coords] [affected _]]] {move (set (take (count affected) coords))})) 
             (into {}))))))

;; #125
(fn [] (let [s (str (quote (fn [] (let [s (str (quote %s))] (format s s)))))] (format s s)))

;; Original, non-optimized version
(fn triangles [b]
  (letfn [(get-bit [x y] (if (and (< -1 x (count b)) (<= 0 y 4)) (bit-test (nth b x) (- 4 y)) nil))
          (get-bit-10 [x y] (if (get-bit x y) 1 0))
          (bitmap-10 [] (map-indexed (fn [x e] (map (fn [y] (if (get-bit x y) 1 0)) (range 5))) b))
          (bitmap-tf [] (map-indexed (fn [x e] (map (fn [y] (get-bit x y)) (range 5))) b))
          (print-bitmap [] (println (clojure.string/join "\n" (map clojure.string/join (bitmap-10)))))
          (tri-vert [[x y] dir [inc-l inc-r]]
            (take-while (fn [e]
                          (every? true? e))
                        (map (fn [r]
                               (let [diff (Math/abs (- x r))]
                                 (map (fn [c] (get-bit r c))
                                      (range (- y (* diff inc-l)) (+ y (* diff inc-r) 1)))))
                             (if (= dir :up) (range x -1 -1) (range x (count b))))))
          (tri-horiz [[x y] dir [inc-l inc-r]]
            (take-while (fn [e]
                          (every? true? e))
                        (map (fn [c]
                               (let [diff (Math/abs (- y c))]
                                 (map (fn [r] (get-bit r c))
                                      (range (- x (* diff inc-l)) (+ x (* diff inc-r) 1)))))
                             (if (= dir :right) (range y 5) (range y -1 -1)))))
          ]
    (let [apertures [[0 1] [1 0] [1 1]]
          dirs-v [:up :down]
          dirs-h [:left :right]]
      (print-bitmap)
      (first (sort >
                   (filter (partial <= 3)
                           (map (comp count flatten)
                                (filter
                                 (fn [t] (every? true? (flatten t)))
                                 (apply concat
                                        (for [x (range (count b)) y (range 5)]
                                          (concat
                                           (apply concat
                                                  (map (fn [dirv]
                                                         (map (fn [ap] (tri-vert [x y] dirv ap))
                                                              apertures))
                                                       dirs-v))
                                           (apply concat
                                                  (map (fn [dirh]
                                                         (map (fn [ap] (tri-horiz [x y] dirh ap))
                                                              apertures))
                                                       dirs-h)))))))))))))

;; With some optimizations and refactorings
(fn triangles [b]
  (letfn [(get-bit [x y] (if (and (< -1 x (count b)) (<= 0 y 4)) (bit-test (nth b x) (- 4 y)) nil))
          ;; From a starting point [x y], return the all-ones triangle extending in `dir' (up/down for tri-vert,
          ;; left/right for tri-horiz) that at each step extends according to `inc-l' and `inc-r' on each
          ;; side. E.g. 
          ;; (tri-vert [x y] :inc [0 1])    (tri-vert [x y] :dec [1 1])  
          ;;       1    <--[x y]                  11111                  
          ;;       11                              111                   
          ;;       111                              1  <--[x y]          
          (tri-vert [[x y] dir [inc-l inc-r]]
            (take-while (partial every? true?)
                        (map (fn [r]
                               (let [diff (Math/abs (- x r))]
                                 (map (fn [c] (get-bit r c))
                                      (range (- y (* diff inc-l)) (+ y (* diff inc-r) 1)))))
                             (if (= dir :dec) (range x -1 -1) (range x (count b))))))
          (tri-horiz [[x y] dir [inc-l inc-r]]
            (take-while (partial every? true?)
                        (map (fn [c]
                               (let [diff (Math/abs (- y c))]
                                 (map (fn [r] (get-bit r c))
                                      (range (- x (* diff inc-l)) (+ x (* diff inc-r) 1)))))
                             (if (= dir :dec) (range y -1 -1) (range y 5)))))
          ]
    (->> (for [x (range (count b))
               y (range 5)                 ; from each point in the grid...
               trifn [tri-horiz tri-vert]  ; compute
               dir [:inc :dec]             ; all the triangles 
               ap [[0 1] [1 0] [1 1]]]     ; that start there
           (trifn [x y] dir ap))
         (map (comp count flatten))        ; count their size
         (filter (partial <= 3))           ; remove too-small ones
         (sort >)                          ; sort by largest
         first)))                          ; and return the first

;; More optimizations
(defn triangles [b]
  (let [rows (count b) cols 5]
    (letfn [(get-bit [x y] (bit-test (nth b x 0) (- cols y 1))) 
            (triangle [[x y] dir [inc-l inc-r]]
              (let [outer-range   (case dir
                                    :up   (range x -1 -1), :down  (range x rows)
                                    :left (range y -1 -1), :right (range y cols))
                    inner-rng-gen (fn [x y] #(let [diff (Math/abs (- x %))] (range (- y (* diff inc-l)) (+ y (* diff inc-r) 1))))
                    inner-rng-fn  (case dir (:up :down) (inner-rng-gen x y), (:left :right) (inner-rng-gen y x))
                    get-bit-fn    (case dir (:up :down) #(get-bit %1 %2),    (:left :right) #(get-bit %2 %1))]
                (take-while (partial every? true?) (map (fn [d1]
                                                          (map (fn [d2] (get-bit-fn d1 d2)) (inner-rng-fn d1)))
                                                        outer-range))))]
      (->> (for [x (range rows), y (range cols) ; from each point in the grid,
                 ap [[0 1] [1 0] [1 1]]         ; compute all the triangles 
                 dir [:up :down :left :right]]  ; in all directions
             (triangle [x y] dir ap))
           (map (comp count flatten))           ; count their size
           (filter (partial <= 3))              ; remove too-small ones
           (sort >) first))))                   ; sort by largest and take the first one

;; Testing the triangles function
(let [__ triangles]
  [(= 10 (__ [15 15 15 15 15]))
   (= 15 (__ [1 3 7 15 31]))
   (= 3 (__ [3 3]))
   (= 4 (__ [7 3]))
   (= 6 (__ [17 22 6 14 22]))
   (= 9 (__ [18 7 14 14 6 3]))
   (= nil (__ [21 10 21 10]))
   (= nil (__ [0 31 0 31 0]))
   ])

;; #128
(defn cards [s]
  (let [suits {"D" :diamond "H" :heart "C" :club "S" :spade}
        ranks (apply merge {"T" 8 "J" 9 "Q" 10 "K" 11 "A" 12} (map #(hash-map (str %) (- % 2)) (range 2 10)))
        [suit rank] (map str (seq s))]
    {:suit (suits suit) :rank (ranks rank)}))

;; #130
(defn build-tree-graph
  ([t] (build-tree-graph t {} nil))
  ([[r & c] graph parent]
   (let [graph (merge graph {r (concat (map first c) (if (nil? parent) parent [parent]))})]
     (apply merge graph (map #(build-tree-graph % graph r) c)))))

(defn traverse [n t g seen]
  (let [seen (conj seen n)
        children (remove seen (get g n))
        _ (println {'n n 't t 'g g 'seen seen 'children children})]
    (apply list n (map (fn [e] (traverse e t g seen)) children))))

(defn reparent-tree [n t]
  (letfn [(tree-graph [[r & c] parent]
            (apply merge {r (into (mapv first c) (when parent [parent]))} 
                   (map #(tree-graph % r) c)))
          (traverse [g seen n]
            (let [seen (conj seen n) children (remove seen (g n))]
              (apply list n (map (partial traverse g seen) children))))]
    (traverse (tree-graph t nil) #{} n)))

(let [__ reparent-tree]
  [(= '(n)
      (__ 'n '(n)))
   (= '(a (t (e)))
      (__ 'a '(t (e) (a))))
   (= '(e (t (a)))
      (__ 'e '(a (t (e)))))
   (= '(a (b (c)))
      (__ 'a '(c (b (a)))))
   (= '(d 
        (b
         (c)
         (e)
         (a 
          (f 
           (g) 
           (h)))))
      (__ 'd '(a
               (b 
                (c) 
                (d) 
                (e))
               (f 
                (g)
                (h)))))
   (= '(c 
        (d) 
        (e) 
        (b
         (f 
          (g) 
          (h))
         (a
          (i
           (j
            (k)
            (l))
           (m
            (n)
            (o))))))
      (__ 'c '(a
               (b
                (c
                 (d)
                 (e))
                (f
                 (g)
                 (h)))
               (i
                (j
                 (k)
                 (l))
                (m
                 (n)
                 (o))))))])

;; #131
(defn subset-sums [& xs]
  (letfn [(kcomb
            ([n coll] (flatten (kcomb n (seq coll) #{})))
            ([n coll so-far] (if (zero? n) so-far
                                 (map #(kcomb (dec n) (drop (inc %) coll) (conj so-far (nth coll %)))
                                      (range (count coll))))))]
    (let [sums (map (fn [s] (into #{} (map #(apply + %)
                                           (apply concat (map (fn [n] (kcomb (inc n) s))
                                                              (range (count s))))))) xs)]
      (or (some true? (map (fn [n] (every? #(% n) sums)) (first sums))) false))))

(fn subset-sums [& xs]
  (letfn [(kcomb-sums
            ([n coll] (map #(apply + %) (flatten (kcomb-sums n (seq coll) #{}))))
            ([n coll so-far] (if (zero? n) so-far
                                 (map #(kcomb-sums (dec n) (drop (inc %) coll) (conj so-far (nth coll %)))
                                      (range (count coll))))))]
    (let [sums (map (fn [s] (into #{} (apply concat (map (fn [n] (kcomb-sums (inc n) s))
                                                         (range (count s)))))) xs)]
      (or (some true? (map (fn [n] (every? #(% n) sums)) (first sums))) false))))

(let [__ subset-sums]
  [(= true  (__ #{-1 1 99} 
                #{-2 2 888}
                #{-3 3 7777})) ; ex. all sets have a subset which sums to zero
   (= false (__ #{1}
                #{2}
                #{3}
                #{4}))
   (= true  (__ #{1}))
   (= false (__ #{1 -3 51 9} 
                #{0} 
                #{9 2 81 33}))
   (= true  (__ #{1 3 5}
                #{9 11 4}
                #{-3 12 3}
                #{-3 4 -2 10}))
   (= false (__ #{-1 -2 -3 -4 -5 -6}
                #{1 2 3 4 5 6 7 8 9}))
   (= true  (__ #{1 3 5 7}
                #{2 4 6 8}))
   (= true  (__ #{-1 3 -5 7 -9 11 -13 15}
                #{1 -3 5 -7 9 -11 13 -15}
                #{1 -1 2 -2 4 -4 8 -8}))])

;; #132
(fn ins-between [pred val [c1 & crest :as coll]]
  (when (seq coll)
    (lazy-seq (cond
                (empty? crest)          [c1]
                (pred c1 (first crest)) (concat [c1 val] (ins-between pred val crest))
                :else                   (concat [c1] (ins-between pred val crest))))))

;; #134
#(and (contains? %2 %1) (nil? (%2 %1)))
#(nil? (get %2 %1 :not-nil))

;; #135
(fn infix [& [n1 op n2 & exp]]
  (if (nil? op) n1
      (apply infix (cons (op n1 n2) exp))))

;; #137
(fn base-n [n b]
  (loop [res [] n n]
    (if (< n b) (conj res n)
        (recur (cons (rem n b) res) (quot n b)))))

;; #138
(fn square-sqs [start end]
  (let [dirs [[1 1] [1 -1] [-1 -1] [-1 1]]
        sqr #(* % %)
        digits (flatten (map (comp seq str) (take-while #(<= % end) (iterate sqr start))))
        side (-> digits count Math/sqrt Math/ceil int)
        chart-size (dec (* 2 side)), cs2 (quot chart-size 2)
        chars (take (sqr side) (lazy-cat digits (repeat \*)))
        steps (take (count chars) (mapcat #(repeat % %2) (drop 2 (interleave (range) (range))) (cycle dirs)))
        init-coord [(- cs2 (if (even? side) 1 0)) cs2]
        empty-chart (vec (repeat chart-size (vec (repeat chart-size \space))))
        plan (loop [res {}, xs (interleave chars steps), coord init-coord]
               (if-not (seq xs) res
                       (recur (merge res {coord (first xs)}) (drop 2 xs) (mapv + coord (second xs)))))] 
    (map (fn [row] (apply str row))
         (reduce (fn [c p] (assoc-in c (first p) (second p))) empty-chart plan))))

;; Shorter version
(defn square-sqs [s e]
  (let [q #(* % %) rp repeat i interleave rn (range) mc mapcat
        d (mc (comp seq str) (take-while #(<= % e) (iterate q s)))
        sd (-> d count Math/sqrt Math/ceil int) cs (dec (* 2 sd)) cs2 (quot cs 2)
        ch (take (q sd) (lazy-cat d (rp \*)))] 
    (map #(apply str %) (reduce #(assoc-in % (first %2) (second %2))
                                (vec (rp cs (vec (rp cs \space))))
                                (loop [r {} c [(- cs2 (if (even? sd) 1 0)) cs2]
                                       xs (i ch (take (count ch) (mc #(rp % %2) (drop 2 (i rn rn)) (cycle [[1 1] [1 -1] [-1 -1] [-1 1]]))))]
                                  (if-not (seq xs) r
                                          (recur (merge r {c (first xs)}) (mapv + c (second xs)) (drop 2 xs))))))))

(let [__ square-sqs]
  [(= (__ 2 2) ["2"])
   (= (__ 2 4) [" 2 "
                "* 4"
                " * "])
   (= (__ 3 81) [" 3 "
                 "1 9"
                 " 8 "])
   (= (__ 4 20) [" 4 "
                 "* 1"
                 " 6 "])
   (= (__ 2 256) ["  6  "
                  " 5 * "
                  "2 2 *"
                  " 6 4 "
                  "  1  "])
   (= (__ 10 10000) ["   0   "
                     "  1 0  "
                     " 0 1 0 "
                     "* 0 0 0"
                     " * 1 * "
                     "  * *  "
                     "   *   "])])

;; #140
(defn veitch [mt] 
  (letfn [(kcomb ([n coll] (flatten (kcomb n (seq coll) #{})))
            ([n coll so-far] (if (zero? n) so-far (map #(kcomb (dec n) (drop (inc %) coll) (conj so-far (nth coll %)))
                                                       (range (count coll))))))
          (coord [ax t] (->> ax (map-indexed #(vector %1 (count (clojure.set/intersection t %2))))
                             (filter #(= (second %) (count (first ax)))) ffirst))]
    (let [pairs [#{'A 'a} #{'B 'b} #{'C 'c} #{'D 'd}] rp [0 1 3]
          xc [#{'a 'b} #{'a 'B} #{'A 'B} #{'A 'b}] xl 4
          yc (if (= (count (first mt)) 4) [#{'c 'd} #{'c 'D} #{'C 'D} #{'C 'd}] [#{'c} #{'C}]) yl (count yc) 
          coords (set (map #(vector (coord yc %) (coord xc %)) mt))
          sq (fn [[y x] [h w]] (set (for [i (range y (+ y h 1)) j (range x (+ x w 1))] (coords [(mod i yl) (mod j xl)]))))
          sqs (->> (for [y (range yl) x (range xl) h rp w rp] (sq [y x] [h w]))
                   (remove (partial some nil?))
                   (sort-by count >)
                   (reduce (fn [r e] (if (some #(= (into % e) %) r) r (conj r e))) []))]
      (->> (for [n (range (count sqs) 0 -1)] (kcomb n sqs))
           (apply concat)
           (filter #(= coords (apply clojure.set/union %)))
           (sort-by count) first
           (map (fn [e] (apply clojure.set/union (map (fn [f] (into (into #{} (nth yc (first f))) (nth xc (second f)))) e))))
           (map (fn [e] (reduce (fn [r p] (if (every? r p) (apply disj r p) r)) e pairs)))
           set))))

(let [__ veitch]
  [(= (__ #{#{'a 'B 'C 'd}
            #{'A 'b 'c 'd}
            #{'A 'b 'c 'D}
            #{'A 'b 'C 'd}
            #{'A 'b 'C 'D}
            #{'A 'B 'c 'd}
            #{'A 'B 'c 'D}
            #{'A 'B 'C 'd}})
      #{#{'A 'c} 
        #{'A 'b}
        #{'B 'C 'd}})
   (= (__ #{#{'A 'B 'C 'D}
            #{'A 'B 'C 'd}})
      #{#{'A 'B 'C}})
   (= (__ #{#{'a 'b 'c 'd}
            #{'a 'B 'c 'd}
            #{'a 'b 'c 'D}
            #{'a 'B 'c 'D}
            #{'A 'B 'C 'd}
            #{'A 'B 'C 'D}
            #{'A 'b 'C 'd}
            #{'A 'b 'C 'D}})
      #{#{'a 'c}
        #{'A 'C}})
   (= (__ #{#{'a 'b 'c} 
            #{'a 'B 'c}
            #{'a 'b 'C}
            #{'a 'B 'C}})
      #{#{'a}})
   (= (__ #{#{'a 'B 'c 'd}
            #{'A 'B 'c 'D}
            #{'A 'b 'C 'D}
            #{'a 'b 'c 'D}
            #{'a 'B 'C 'D}
            #{'A 'B 'C 'd}})
      #{#{'a 'B 'c 'd}
        #{'A 'B 'c 'D}
        #{'A 'b 'C 'D}
        #{'a 'b 'c 'D}
        #{'a 'B 'C 'D}
        #{'A 'B 'C 'd}})
   (= (__ #{#{'a 'b 'c 'd}
            #{'a 'B 'c 'd}
            #{'A 'B 'c 'd}
            #{'a 'b 'c 'D}
            #{'a 'B 'c 'D}
            #{'A 'B 'c 'D}})
      #{#{'a 'c}
        #{'B 'c}})
   (= (__ #{#{'a 'B 'c 'd}
            #{'A 'B 'c 'd}
            #{'a 'b 'c 'D}
            #{'a 'b 'C 'D}
            #{'A 'b 'c 'D}
            #{'A 'b 'C 'D}
            #{'a 'B 'C 'd}
            #{'A 'B 'C 'd}})
      #{#{'B 'd}
        #{'b 'D}})
   (= (__ #{#{'a 'b 'c 'd}
            #{'A 'b 'c 'd}
            #{'a 'B 'c 'D}
            #{'A 'B 'c 'D}
            #{'a 'B 'C 'D}
            #{'A 'B 'C 'D}
            #{'a 'b 'C 'd}
            #{'A 'b 'C 'd}})
      #{#{'B 'D}
        #{'b 'd}})])

;; #141
(fn trickfn [trump]
  (fn [cards]
    (letfn [(maxsuit [suit] (->> cards (filter #(= suit (:suit %))) (sort-by :rank >) first))]
      (or (when trump (maxsuit trump)) (maxsuit (:suit (first cards)))))))

;; Shorter
(fn trickfn [t]
  (fn [c]
    (->> c (filter #(= (if t t (:suit (first c))) (:suit %))) (sort-by :rank >) first)))

;; #143
(fn dotp [v1 v2]
  (reduce + (map * v1 v2)))

;; #144
(fn oscilrate [v & fns]
  (reductions #(%2 %) v (cycle fns)))

;; #146
#(into {} (for [x (keys %) y (keys (% x))] {[x y] (get-in % [x y])}))
;; Use destructuring, more elegant
#(into {} (for [[x v] % [y w] v] {[x y] w}))

;; #147
(fn pascal [v]
  (iterate #(->> %
                 (partition-all 2 1)
                 (map (partial apply +'))
                 (cons (first %))) v))

(fn pascal [v]
  (iterate #(map +' (cons 0 %) (concat % [0])) v))

;; #148
(fn bigdivide [n a b]
  (letfn [(rsum [t] (/ (*' t (inc t)) 2))
          (divsum [x] (*' x (rsum (quot (dec n) x))))]
    (-' (+' (divsum a) (divsum b)) (divsum (*' a b)))))

;; #150
(defn next-palyndromes [n]
  (letfn [(n-to-dig [n] (vec (map (comp read-string str) (seq (str n)))))
          (dig-to-n [d] (read-string (apply str (map str d))))
          (middle [s] (quot (count s) 2))
          (next-paly [n]
            (loop [dig (n-to-dig n)
                   pos (middle dig)]
              (let [len (count dig)
                    mid (middle dig)
                    d1 (nth dig pos)
                    d2 (nth dig (- len pos 1)) 
                    half (subvec dig 0 mid)
                    hm (concat half (when (odd? len) [(nth dig mid)]))
                    nn (n-to-dig (inc (dig-to-n hm)))
                    nd (vec (concat nn (reverse (if (odd? len) (butlast nn) nn))))]
                (if (= dig (reverse dig))
                  (dig-to-n dig)
                  (condp = (compare d1 d2) 
                    0  (recur dig (inc pos))
                    -1 (dig-to-n (concat hm (reverse half)))
                    1  (dig-to-n nd))))))]
    (let [np (next-paly n)]
      (cons np (lazy-seq (next-palyndromes (inc np)))))))

(let [__ next-palyndromes]
  [(= (take 26 (__ 0))
      [0 1 2 3 4 5 6 7 8 9 
       11 22 33 44 55 66 77 88 99 
       101 111 121 131 141 151 161])
   (= (take 16 (__ 162))
      [171 181 191 202 
       212 222 232 242 
       252 262 272 282 
       292 303 313 323])
   (= (take 6 (__ 1234550000))
      [1234554321 1234664321 1234774321 
       1234884321 1234994321 1235005321])
   (= (first (__ (* 111111111 111111111)))
      (* 111111111 111111111))
   (= (set (take 199 (__ 0)))
      (set (map #(first (__ %)) (range 0 10000))))
   (= true 
      (apply < (take 6666 (__ 9999999))))
   (= (nth (__ 0) 10101)
      9102019)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #152
(defn lss1 [v & opt]
  (letfn [(contains-latin-square? [b c [y x] w]
            (let [elems (set (subvec (nth b y) x (+ x w)))]
              (when (and (= (count elems) w) (not (elems :e)))
                (let [rows (map #(subvec (nth b (+ y %)) x (+ x w)) (range w))
                      cols (map #(subvec (nth c (+ x %)) y (+ y w)) (range w))
                      lines (map set (concat rows cols))]
                  (when (every? #(= % elems) lines) rows)))))
          (cart [colls]
            (if (empty? colls)
              '(())
              (for [x (first colls)
                    more (cart (rest colls))]
                (cons x more))))
          ]
    (let [colsfn (memoize (fn [m] (map (fn [c] (vec (map #(nth % c) m))) (range (count (first m))))))
          maxln (apply max (map count v))
          steps (cart (map range (map #(inc (- maxln (count %))) v)))
          matrices (map (fn [offset]
                          (map (fn [e i] (vec (concat (repeat i :e) e (repeat (- maxln (+ i (count e))) :e)) )) v offset)) steps)
          ;;_ (println (count matrices) "matrices")
          allsquares (remove nil?
                             (distinct (for [m matrices
                                             :let [hm (count m) wm (count (first m))
                                                   cols (colsfn m)]
                                             y (range hm)
                                             x (range wm)
                                             w (range 2 (inc hm))
                                             :when (and (<= (+ y w) hm) (<= (+ x w) wm))]
                                         (contains-latin-square? m cols [y x] w)))) 
          ] 
      (into {}
            (map (fn [[p1 p2]] {p1 (count p2)})
                 (group-by count
                           allsquares))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lss2 [v & opt]
  (let [v (vec (remove empty? v))]
    (letfn [(contains-latin-square? [o [y x] w]
              (let [frw (- x (nth o y))
                    elems (set (subvec (nth v y) frw (+ frw w)))]
                (when (= (count elems) w)
                  (let [yw (+ y w)
                        rows (map #(let [xo (- x %2)]
                                     (subvec % xo (+ xo w))) (subvec v y yw) (subvec o y yw))
                        cols (apply map vector rows)
                        lines (map set (concat rows cols))]
                    (when (every? #(= % elems) lines) rows)))))
            (cart [colls]
              (if (empty? colls)
                '(())
                (for [x (first colls)
                      more (cart (rest colls))]
                  (cons x more))))]
      (let [cv (count v) rcv (range cv)
            maxln (apply max (map count v))
            steps (map vec (cart (map range (map #(inc (- maxln (count %))) v))))
            allsquares (distinct (remove nil?
                                         (apply concat
                                                (for [o steps
                                                      y rcv
                                                      :let [oy (nth o y)
                                                            ends (map + o (map count v))]
                                                      x (range oy (+ oy (count (nth v y))))
                                                      ]
                                                  (for [w (range (inc cv) 1 -1)
                                                        :let [yw (+ y w)]
                                                        :when (and (<= yw cv) (every? #(and (>= x (nth o %)) (<= (+ x w) (nth ends %))) (range y yw)))
                                                        :let [result (contains-latin-square? o [y x] w)]
                                                        :while [(not (nil? result))]]
                                                    result))))) 
            ] 
        (into {}
              (map (fn [[p1 p2]] {p1 (count p2)})
                   (group-by count
                             allsquares)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lss3 [v & opt]
  (letfn [(contains-latin-square? [b o [x y] w]
            (let [;;_ (println "b=" b "o=" o "[y x]=" [y x] "w=" w)
                  v1 (for [y (range y (+ y w))
                           :let [xo (- x (nth o y))]
                           x (range xo (+ xo w)) 
                           :let [;;_ (println "b=" b "[y x]=" [y x])
                                 e (get-in b [y x] nil)
                                 ;;_ (println "e=" e)
                                 ]]
                       {:rows #{[y e]} :cols #{[x e]} :values #{e}} )
                  ;;_ (println "v1=" v1)
                  v2 (apply merge-with into v1)
                  ;;_ (println "v2=" v2)
                  v3 (reduce-kv #(assoc % %2 (count %3)) {} v2)
                  ;;_ (println "v3=" v3)
                  w2 (* w w)]
              (and (= v3 {:rows w2 :cols w2 :values w}) v2)))
          (cart [colls]
            (if (empty? colls)
              '(())
              (for [x (first colls)
                    more (cart (rest colls))]
                (cons x more))))]
    (let [v (vec (remove empty? v))
          cv (count v) rcv (range cv)
          maxln (apply max (map count v))
          maxw (Math/min cv maxln)
          steps (cart (map range (map #(inc (- maxln (count %))) v)))          
          allsquares (distinct (remove false?
                                       (for [o steps
                                             y rcv
                                             x (range (dec maxln))
                                             w (range (Math/min (- maxw x) (- maxw y)) 1 -1)
                                             :let [result (contains-latin-square? v o [y x] w)]
                                             :while [(not (false? result))]
                                             ]
                                         result)))] 
      (into {}
            (map (fn [[p1 p2]] {p1 (count p2)})
                 (group-by count
                           allsquares))))))

;; Same as lss2 but replacing for with mapcat/map, as suggested by
;; https://github.com/4clojure/4clojure/issues/66 and
;; https://github.com/4clojure/4clojure/issues/211
(fn lss4 [v & opt]
  (let [v (vec (remove empty? v))]
    (letfn [(contains-latin-square? [o [y x] w]
              (let [frw (- x (nth o y))
                    elems (set (subvec (nth v y) frw (+ frw w)))]
                (when (= (count elems) w)
                  (let [yw (+ y w)
                        rows (map #(let [xo (- x %2)]
                                     (subvec % xo (+ xo w))) (subvec v y yw) (subvec o y yw))
                        cols (apply map vector rows)
                        lines (map set (concat rows cols))]
                    (when (every? #(= % elems) lines) rows)))))
            (cart [colls]
              (if (empty? colls)
                '(())
                (for [x (first colls)
                      more (cart (rest colls))]
                  (cons x more))))]
      (let [cv (count v) rcv (range cv)
            maxln (apply max (map count v))
            steps (map vec (cart (map range (map #(inc (- maxln (count %))) v))))
            allsquares (distinct (remove nil?
                                         (mapcat
                                          (fn [o]
                                            (mapcat
                                             (fn [y]
                                               (let [oy (nth o y)
                                                     ends (map + o (map count v))]
                                                 (mapcat
                                                  (fn [x]
                                                    (map
                                                     (fn [w]
                                                       (let [yw (+ y w)]
                                                         (when (and (<= yw cv) (every? #(and (>= x (nth o %)) (<= (+ x w) (nth ends %))) (range y yw)))
                                                           (contains-latin-square? o [y x] w))))
                                                     (range (inc cv) 1 -1)))
                                                  (range oy (+ oy (count (nth v y)))))))
                                             rcv))
                                          steps)))]
        (frequencies (map count
                          allsquares))))))

(defn lss5 [v & opt]
  (let [v (vec (remove empty? v))]
    (letfn [(contains-latin-square? [o [y x] w]
              (let [frw (- x (nth o y))
                    elems (set (subvec (nth v y) frw (+ frw w)))]
                (when (= (count elems) w)
                  (let [yw (+ y w)
                        rows (map #(let [xo (- x %2)]
                                     (subvec % xo (+ xo w))) (subvec v y yw) (subvec o y yw))
                        cols (apply map vector rows)
                        lines (map set (concat rows cols))]
                    (when (every? #(= % elems) lines) rows)))))
            (cart [colls]
              (if (empty? colls)
                '(())
                (for [x (first colls)
                      more (cart (rest colls))]
                  (cons x more))))]
      (let [cv (count v) rcv (range cv)
            maxln (apply max (map count v))
            steps (map vec (cart (map range (map #(inc (- maxln (count %))) v))))
            allsquares (distinct (remove nil?
                                         (mapcat
                                          (fn [o]
                                            (mapcat
                                             (fn [y]
                                               (let [oy (nth o y)
                                                     ends (map + o (map count v))]
                                                 (mapcat
                                                  (fn [x]
                                                    (map
                                                     (fn [w]
                                                       (let [yw (+ y w)]
                                                         (when (and (<= yw cv) (every? #(and (>= x (nth o %)) (<= (+ x w) (nth ends %))) (range y yw)))
                                                           (contains-latin-square? o [y x] w))))
                                                     (range (inc cv) 1 -1)))
                                                  (range oy (+ oy (count (nth v y)))))))
                                             rcv))
                                          steps)))]
        (frequencies (map count
                          allsquares))))))

(defn lss6 [v & opt]
  (let [v (vec (remove empty? v)), cv (count v), rcv (range cv), mcv (map count v), maxln (apply max mcv), mc mapcat]
    (letfn [(take-part [n1 n2 s] (take n2 (drop n1 s)))
            (latin? [[o y x w]]
              (let [elems (set (take-part (- x (nth o y)) w (nth v y)))]
                (when (= (count elems) w)
                  (let [rows (map #(take-part (- x %2) w %) (take-part y w v) (take-part y w o))
                        cols (apply map vector rows)]
                    (when (every? #(= (set %) elems) (concat rows cols))
                      rows)))))
            (cart [colls] (if (empty? colls) '(())
                              (for [x (first colls) more (cart (rest colls))]
                                (cons x more))))]
      (let [steps (cart (map range (map #(inc (- maxln (count %))) v)))
            ;; Ugly, but a for loop triggers bug https://github.com/4clojure/4clojure/issues/211
            allsquares (mc (fn [o] (mc (fn [y] (let [oy (nth o y)] (mc (fn [x] (map (fn [w] [o y x w]) (range 2 (inc cv)))) (range oy (+ oy (count (nth v y))))))) rcv)) steps)]
        (->> allsquares
             (map latin?)
             (remove nil?)
             distinct
             (map count)
             frequencies)))))

(let [__ lss6]
  (do
    [(= (__ '[[A B C D]
              [A C D B]
              [B A D C]
              [D C A B]])
        {})
     (= (__ '[[A B C D E F]
              [B C D E F A]
              [C D E F A B]
              [D E F A B C]
              [E F A B C D]
              [F A B C D E]])
        {6 1})
     (= (__ '[[A B C D]
              [B A D C]
              [D C B A]
              [C D A B]])
        {4 1, 2 4})
     (= (__ '[[B D A C B]
              [D A B C A]
              [A B C A B]
              [B C A B C]
              [A D B C A]])
        {3 3})
     (= (__ [[2 4 6 3]
             [3 4 6 2]
             [6 2 4]  ])
        {})
     (= (__ [[1]
             [1 2 1 2]
             [2 1 2 1]
             [1 2 1 2]
             []       ])
        {2 2})
     (= (__ [[3 1 2]
             [1 2 3 1 3 4]
             [2 3 1 3]    ])
        {3 1, 2 2})
     (= (__ [[8 6 7 3 2 5 1 4]
             [6 8 3 7]
             [7 3 8 6]
             [3 7 6 8 1 4 5 2]
             [1 8 5 2 4]
             [8 1 2 4 5]])
        {4 1, 3 1, 2 7})]
    ))

;; #153
(fn pds [xs]
  (every? empty? (for [x xs y xs :when (not= x y)] (clojure.set/intersection x y))))

;; #156
(fn map-def [v ks]
  (apply hash-map (interleave ks (repeat v))))
(fn map-def [v ks]
  (zipmap ks (repeat v)))

;; #157
(fn index-pairs [s]
  (map-indexed #(vector %2 %) s))
#(map vector % (range))

;; #158
(fn decurry [f]
  (fn [& args] (reduce #(% %2) f args)))

;; #164
(defn accept? [dfa state]
  ((:accepts dfa) state))

(defn valid-chars [dfa state]
  (keys ((:transitions dfa) state)))

(defn transition [dfa cur char]
  ((cur (:transitions dfa)) char))

(defn next-states [dfa cur]
  (map (partial transition dfa cur) (valid-chars dfa cur)))

;; First PoC solution, prints the results but doesn't return them, uses functions above
(defn dfa-walk1 [{:keys [states alphabet start accepts transitions] :as dfa}]
  (let [;;_ (println dfa)
        ]
    (if (accept? dfa start) (println (:last dfa)))
    (map #(cons % (dfa-walk1 (assoc dfa :last (str (:last dfa) %) :start (transition dfa start %)))) (valid-chars dfa start))))

;; First working solution
(defn dfa-walk2 [{:keys [states alphabet start accepts transitions result counts] :as dfa}]
  (letfn [(accept? [state]
            ((:accepts dfa) state))
          (valid-chars [state]
            (keys ((:transitions dfa) state)))
          (transition [cur char]
            ((cur (:transitions dfa)) char))
          ]
    (distinct
     (let [;;_ (println dfa)
           res (when (accept? start) (:last dfa))
           chars (valid-chars start)
           ]
       (if (or (empty? chars) (> (get counts start 0) (* 2 (count states)))) (cons res result)
           (remove nil? (mapcat #(dfa-walk2 (assoc dfa
                                                   :result (cons res result)
                                                   :last (str (:last dfa) %)
                                                   :start (transition start %)
                                                   :counts (update-in counts [start] (fn [o] (inc (or o 0)))))) chars)))))))

;; Simplified
(fn dfa-walk [{:keys [start accepts transitions result counts laststr] :as dfa}]
  (distinct (remove nil? 
                    (let [res (if (accepts start) (cons laststr result) result)
                          chars (keys (transitions start))]
                      (if (or (empty? chars) (> (get counts start 0) (count transitions)))
                        res
                        (mapcat (fn [c] (dfa-walk (assoc dfa
                                                         :result res
                                                         :laststr (str laststr c)
                                                         :start ((transitions start) c)
                                                         :counts (update-in counts [start] #(inc (or % 0)))))) chars))))))

(let [__ dfa-walk]
  [(= #{"a" "ab" "abc"}
      (set (__ '{:states #{q0 q1 q2 q3}
                 :alphabet #{a b c}
                 :start q0
                 :accepts #{q1 q2 q3}
                 :transitions {q0 {a q1}
                               q1 {b q2}
                               q2 {c q3}}})))
   (= #{"hi" "hey" "hello"}
      (set (__ '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
                 :alphabet #{e h i l o y}
                 :start q0
                 :accepts #{q2 q4 q7}
                 :transitions {q0 {h q1}
                               q1 {i q2, e q3}
                               q3 {l q5, y q4}
                               q5 {l q6}
                               q6 {o q7}}})))
   (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
      (set (__ '{:states #{q0 q1 q2 q3 q4}
                 :alphabet #{v w x y z}
                 :start q0
                 :accepts #{q4}
                 :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                               q1 {v q2, w q2, x q2, y q2, z q2}
                               q2 {v q3, w q3, x q3, y q3, z q3}
                               q3 {v q4, w q4, x q4, y q4, z q4}}})))
   (let [res (take 2000 (__ '{:states #{q0 q1}
                              :alphabet #{0 1}
                              :start q0
                              :accepts #{q0}
                              :transitions {q0 {0 q0, 1 q1}
                                            q1 {0 q1, 1 q0}}}))]
     (and (every? (partial re-matches #"0*(?:10*10*)*") res)
          (= res (distinct res))))
   (let [res (take 2000 (__ '{:states #{q0 q1}
                              :alphabet #{n m}
                              :start q0
                              :accepts #{q1}
                              :transitions {q0 {n q0, m q1}}}))]
     (and (every? (partial re-matches #"n*m") res)
          (= res (distinct res))))
   (let [res (take 2000 (__ '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                              :alphabet #{i l o m p t}
                              :start q0
                              :accepts #{q5 q8}
                              :transitions {q0 {l q1}
                                            q1 {i q2, o q6}
                                            q2 {m q3}
                                            q3 {i q4}
                                            q4 {t q5}
                                            q6 {o q7}
                                            q7 {p q8}
                                            q8 {l q9}
                                            q9 {o q6}}}))]
     (and (every? (partial re-matches #"limit|(?:loop)+") res)
          (= res (distinct res))))
   ])

;; #166
(fn [op a b]
  (cond (op a b) :lt
        (op b a) :gt
        :else    :eq))

;; #168
(fn infm
  ([f] (infm f 0 0))
  ([f m n] (letfn [(row [f i j] (lazy-seq (cons (f i j) (row f i (inc j)))))]
             (lazy-seq (cons (row f m n) (infm f (inc m) n)))))
  ([f m n s t] (take s (map #(take t %) (infm f m n)))))

;; #171
(fn intervals [s]
  (if (empty? s) []
      (let [[f & r] (sort s)]
        (reduce (fn [v e] (if (< (- e (last (last v))) 2)
                            (concat (butlast v) [[(first (last v)) e]])
                            (concat v [[e e]])))
                [[f f]] r))))

;; Simpler using more destructuring
(fn intervals [s]
  (if (empty? s) []
      (let [[f & r] (sort s)]
        (reverse (reduce (fn [[[v w] & x :as y] e]
                           (if (< (- e w) 2) (conj x [v e]) (conj y [e e])))
                         [[f f]] r)))))

;; #177
#(let [c {\( \) \[ \] \{ \}}]
   (empty? (reduce (fn [[v & w :as x] e] (if (= e (c v)) w (conj x e))) '()
                   (filter (set (flatten (seq c))) %))))

;; #178
(fn high-hand [cards]
  (letfn [(card [acelow? s]
            (let [suits {"D" :diamond "H" :heart "C" :club "S" :spade}
                  ranks (if acelow? (apply merge {"T" 9 "J" 10 "Q" 11 "K" 12 "A" 0} (map #(hash-map (str %) (- % 1)) (range 2 10))) (apply merge {"T" 8 "J" 9 "Q" 10 "K" 11 "A" 12} (map #(hash-map (str %) (- % 2)) (range 2 10)))) 
                  [suit rank] (map str (seq s))]
              {:suit (suits suit) :rank (ranks rank)}))
          (flush? [cards] (apply = (map :suit cards)))
          (straight? [cards] (let [rs (map :rank cards)] (and (= 4 (- (last rs) (first rs)))
                                                              (apply = (map (partial apply -) (partition 2 1 rs))))))
          (straight-flush? [cards] (and (flush? cards) (straight? cards)))
          (four-of-a-kind? [cards] (let [by-rank (group-by :rank cards)] (some #(= 4 (count %)) (vals by-rank))))
          (full-house? [cards] (let [by-rank (group-by :rank cards)] (= #{3 2} (set (map count (vals by-rank))))))
          (three-of-a-kind? [cards] (let [by-rank (group-by :rank cards)] (some #(= 3 (count %)) (vals by-rank))))
          (pair? [cards] (let [by-rank (group-by :rank cards)] (some #(= 2 (count %)) (vals by-rank))))
          (two-pair? [cards] (let [by-rank (group-by :rank cards)] (= 2 (count (filter #(= 2 (count %)) (vals by-rank))))))
          (high-card? [cards] true)
          ]
    (let [cards1 (sort-by :rank (map (partial card false) cards))
          cards2 (sort-by :rank (map (partial card true) cards))
          fns [[:straight-flush straight-flush?]
               [:four-of-a-kind four-of-a-kind?]
               [:full-house full-house?]
               [:flush flush?]
               [:straight straight?]
               [:three-of-a-kind three-of-a-kind?]
               [:two-pair two-pair?]
               [:pair pair?]
               [:high-card high-card?]]
          ] 
      (first (for [[hand check] fns cards [cards1 cards2]
                   :when (check cards)] hand)))
    ))

;; A bit shorter and clearer
(fn high-hand [cards]
  (letfn [(card [acelow? s]
            (let [suits {"D" :diamond "H" :heart "C" :club "S" :spade}
                  ranks (apply merge {"T" 8 "J" 9 "Q" 10 "K" 11 "A" (if acelow? -1 12)} (map #(hash-map (str %) (- % 2)) (range 2 10))) 
                  [suit rank] (map str (seq s))]
              {:suit (suits suit) :rank (ranks rank)}))
          (by-rank [cards] (vals (group-by :rank cards)))
          (n-o-a-k? [cards n] (some #(= n (count %)) (by-rank cards)))
          (flush? [cards] (apply = (map :suit cards)))
          (straight? [cards] (let [rs (map :rank cards)] (= rs (range (first rs) (inc (last rs))))))
          (straight-flush? [cards] (and (flush? cards) (straight? cards)))
          (four-of-a-kind? [cards] (n-o-a-k? cards 4))
          (full-house? [cards] (= #{3 2} (set (map count (by-rank cards)))))
          (three-of-a-kind? [cards] (n-o-a-k? cards 3))
          (pair? [cards] (n-o-a-k? cards 2))
          (two-pair? [cards] (= 2 (count (filter #(= 2 (count %)) (by-rank cards)))))
          (high-card? [cards] true)]
    (let [cards1 (sort-by :rank (map (partial card false) cards))
          cards2 (sort-by :rank (map (partial card true) cards))
          fns [[:straight-flush  straight-flush?]
               [:four-of-a-kind  four-of-a-kind?]
               [:full-house      full-house?]
               [:flush           flush?]
               [:straight        straight?]
               [:three-of-a-kind three-of-a-kind?]
               [:two-pair        two-pair?]
               [:pair            pair?]
               [:high-card       high-card?]]]
      (first (for [[hand check] fns cards [cards1 cards2]
                   :when (check cards)] hand)))))

;; #195
(fn pars
  ([n] (set (pars n 0)))
  ([n c] (if (= n c 0) [""]
             (concat (when (pos? n) (map #(str "(" %) (pars (dec n) (inc c))))
                     (when (pos? c) (map #(str ")" %) (pars n (dec c))))))))
