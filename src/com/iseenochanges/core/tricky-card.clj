(defn game [s]
  (fn [v]
    (let [suit (if (nil? s)
                 (:suit (first v))
                 s)]
    (last (sort #(compare (:rank %) (:rank %2))
            (filter #(= (:suit %) suit) v))))))
    