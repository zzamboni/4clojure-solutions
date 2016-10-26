(defn eclasses [f D]
  (set (map set (vals (group-by f D)))))
