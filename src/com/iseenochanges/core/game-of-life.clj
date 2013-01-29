(defn ff [board]
  (letfn [(conj-in
            [coll [k & ks] v]
            (if ks
              (assoc coll k (conj-in (get coll k []) ks v))
              (assoc coll k v)))
          (get-cell 
            ([board x y]
              (get-cell board x y \space))
            ([board x y df]
              (let [c (count board)]
                (if (or (< x 0)
                        (>= x c) 
                        (< y 0)
                        (>= y c))
                  df
                  ((board x) y)))))
          (neirs 
            [board x y]
            (for [dx '(-1 0 1)
                  dy '(-1 0 1)
                  :when (not (= 0 dx dy))]
              (get-cell board (+ x dx) (+ y dy))))
          (count-neirs [board x y]
                       (count (filter #(= % \#) (neirs board x y))))
          (result 
            [board x y]
            (if (or (= (count-neirs board x y) 3)
                    (and (= (count-neirs board x y) 2)
                         (= (get-cell board x y) \#)))
              \#
              \space))
          (get-all-cords [size]
                         (for [x (range 0 size)
                               y (range 0 size)]
                           (vector x y)))
          (step [board]
                (loop [coord (get-all-cords (count board))
                       res board]
                  (if (nil? coord)
                    res
                    (let [crd (first coord)
                          nw  (result board (first crd) (second crd))]
                      (recur (next coord) 
                             (conj-in res crd nw)
                             )))))
          (parse [col]
                 (into [] (map (comp (partial into []) seq) col)))
          (com [board]
               (into [] (map #(reduce str %) board)))
          (pre-final [board]
                     (com (step (parse board))))]
         (pre-final board)))