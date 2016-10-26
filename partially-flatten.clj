(defn partially-flatten [s]
  (reduce (fn [v e]
            (if (sequential? e)
              (if (sequential? (first e))
                (concat v (partially-flatten e))
                (concat v [e]))
              v))
          [] s))
