(fn final [matrix me]
  (letfn [(mat-to-map [mat]
            (into {} (reduce concat  (for [i (range (count mat))]
                                       (for [j (range (count (mat 0)))]
                                         [[i j] ((mat i) j)]
                                         )))))
          (get-all [mp key]
            (filter #(= key (second %)) 
                    (seq mp)))
          (trv [mp x1 y1 x2 y2 fx fy op]
            (let [col (map #(vector % (mp %)) 
                           (rest (take-while #(not= % [x2 y2]) (map vector (iterate fx x1) (iterate fy y1)))))]
              (if (and (every? #(= op (second %)) col)
                       (not (empty? col)))
                col
                nil)))
          (met-cell [c]
            (+ (first (first c)) (* 2 (second (first c)))))
          (min-cell [c1 c2]
            (let [m1 (met-cell c1)
                  m2 (met-cell c2)]
              (if (< m1 m2)
                c1
                c2)))
          (max-cell [c1 c2]
            (if (= c1 (min-cell c2 c1))
              c2 
              c1))
          (opponent [me]
            (if (= me 'w) 'b 'w))
          (trav [mp from to op]
            "Return collection of opponent staff if is possible"
            (let [w (min-cell from to)
                  e (max-cell from to)
                  x1 (first (first w))
                  y1 (second (first w))
                  x2 (first (first e))
                  y2 (second (first e))
                  colection (cond 
                              (= x1 x2) (trv mp x1 y1 x2 y2 identity inc op)
                              (= y1 y2) (trv mp x1 y1 x2 y2 inc identity op)
                              (= (- x1 y1) (- x2 y2)) (trv mp x1 y1 x2 y2 inc inc op)
                              (= (+ x1 y1) (+ x2 y2)) (trv mp x1 y1 x2 y2 dec inc op)
                              :else nil)]
              (if (empty? colection)
			      :empty
			      [(first to) (into #{} (map first colection))]
			      )))
          ]
         (let [mapa (mat-to-map matrix)
        emptys (get-all mapa 'e)
        mines  (get-all mapa me)
        op (opponent me)]
           (into {} 
                 (filter #(not= % :empty) 
                         (reduce concat 
                                 (for [from mines]
                                   (for [to emptys]
                                     (trav mapa from to op)))))))))