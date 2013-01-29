(defn FFF [from to]
  (letfn [(abs [x]
               (if (< x 0)
                 (- x)
                 x))
          (sign [x]
                (if (< x 0)
                  -1
                  +1))
            
          (f [x]
             (if (zero? x)
               (list (vector 0 0))
               (concat (into [] (map vector (repeat (abs x) (sign x)) (repeat (abs x) (sign x)) ))
                       (into [] (map vector (repeat (abs x) (- (sign x ))) (repeat (abs x) (sign x)))))))                           
          (s [ ]
            (mapcat #(f (* % %2)) (iterate #(- %) 1) (iterate dec 0)))
          (reallen [len]
                   (let [x (first (take 1 (filter #(>= (* % %) len) (iterate inc 1))))]
                     (* x x)))
          (chars [from to]
                 (let [ss (mapcat (comp seq str)
                                  (take-while #(<= % to ) (iterate #(* % %) from)))]
                   (take (reallen (count ss))
                         (concat ss
                                 (repeat \*)))))
          (adding [v1 v2]
                  (into [] (map + v1 v2)))
          (fstart
            ([ ]
              (fstart (s)))
            ([v]
              (fstart [0 0] v))
            ([start v ]
              (if (empty? v)
                nil
                (let [ad (adding start (first v))]
                  (cons ad (lazy-seq (fstart ad (rest v))))))))
          (get45map [from to]
                    (zipmap (fstart) (chars from to)))
          (empty-string [s]
                        (if (string? s)
                          (empty? (filter #(not= % \space) (seq s)))
                          false))
          (final [from to]
            (let [mp (get45map from to)
                  r (int (Math/sqrt (count mp)))]
              (map #(if (not (string? %))
                      (str %)
                      %)
                   (filter (comp false? empty-string) (map #(reduce str %)
                                                           (map (fn [item] (map #(get mp [% item ] " ") (range (-  (dec r))   r))) (range (- (dec r)) (inc r) )))))))]
         (final from to)))
        
        
        