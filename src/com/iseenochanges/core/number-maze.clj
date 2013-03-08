(defn final [start end]
  (let [funs [#(* % 2) #(/ % 2) #(+ % 2)]]
    (letfn [
            (app [start v]
                 (reduce #((funs %2) %
                                     ) 
                         start 
                         v))
            (vec-step [v]
                      (if (< (first v) 2)
                        (concat (vector (inc (first v))) (rest v))
                        (concat (vector 0) (vec-step (rest v)))))
            (vec-seq [v]
                     (if (every? #(= 2 %) v)
                       (cons v nil)
                       (cons v (lazy-seq (vec-seq (vec-step v))))))
            (fnl [start end len]
                 (if (= start end)
                   true
                   (let [s (vec-seq (into [] (repeat len 0)))]
                     (some true? (map #(= (app start %) end) s)))))
            ]
           (inc (first (filter #(not (nil? %)) (map #(if (fnl start end % )
                                                       %
                                                       nil)
                                                    (iterate inc 0))))))))
    