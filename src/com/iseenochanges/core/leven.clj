(defn super [w1 w2]
  (letfn [(init-matrix [res w]
            (if (> (count w) 0)
              (init-matrix (conj res {
                                      [(count w) 
                                       0]
                                      (count w)
                                      }) (rest w ))
              (conj res {[0 0] 0} )))
          (init-matrix2 [res w]
            (if (> (count w) 0)
              (init-matrix2 (conj res {
                                       [
                                        0
                                        (count w) 
                                        ]
                                       (count w)
                                       }) (rest w))
              (conj res {[0 0] 0} )))
          (mat [w1 w2]
            (conj (init-matrix {} w1) (init-matrix2 {} w2)))
          (final [mat w1 w2 i j ]
            (if (<= i (count w1))
              (if (<= j (count w2))
                (let [df (if (=
                               ((vec w1) (dec i))
                               ((vec w2) (dec j))
                               )
                           0
                           1)]
                  (final (conj mat {[i j] (min
                                            (inc (get mat [(dec i) j] 0))
                                            (inc (get mat [i (dec j)] 0))
                                            (+ df (get mat [(dec i) (dec j)] 0))
                                            )})
                         w1 
                         w2
               i
               (inc j)
               ))
                (final mat w1 w2 (inc i) 1))
              mat))
          ]
         (let [m (mat w1 w2)
               f (final m w1 w2 1 1)]
           (get f [(count w1) (count w2)]))))
