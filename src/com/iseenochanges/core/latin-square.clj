(defn final [V]
  (letfn [(transpose [m]
                     (apply map vector m))
          (latin? [sq]
            (letfn [(is-square [dim alls] (every? #(= (count %) dim) alls))
                  (is-only-elements [dim sq] (= (count (reduce into #{} sq)) dim))
                  (is-not-duplicated [dim alls] (every? #(= dim (count %)) (map #(into #{} %) alls))) 
                  (is-not-nils [sq] (not-any? false? (map #(not-any? nil? %) sq)))
                    ]     
            (let [dim (count (first sq))
                  alls (into sq (transpose sq))
                  ]
              (if (not (is-not-nils sq))
                false
                (if (not (is-only-elements dim sq))
                  false
                  (if (not (is-square dim alls))
                    false
                    (is-not-duplicated dim alls)))))))
          (shift [v sh len]
            (let [moar (- len (+ sh (count v)))]
              (into (into (vec (repeat sh nil)) v) (repeat moar nil))))
          (shifts [v len]
            (let [c (count v)
                  d (- len c)]
              (for [x (range 0 (inc d))]
                (shift v x len))))
          (longest [V]
            (apply max (map count V)))
          (all [V]
            (map #(shifts % (longest V)) V))
          (without-empty [V]
            (filter #(not (empty? %)) V))
          (alignments 
            ([V]
              (alignments [[]] (all (without-empty V)) (longest V)))
            ([result V maxlen]
              (if (empty? V)
                result
                (alignments
                  (mapcat #(map (fn [it] (concat % [it])) (first V)) result)
                  (rest V)
                  maxlen))))
          (g [A y x]
            (let [A' (into [] A)]
              ((A' y) x)))
          (get-sq [A dim h w]
            (into [] (for [h' (range 0 dim)]
                       (into [] (for [w' (range 0 dim)]
                                  (g A (+ h h') (+ w w'))) ))))
          (squares 
            ([A]
              (let [d (count A)
                    d' (count (first A))]
                (reduce concat (map #(squares A %) (range 2 (inc (min d d')))))))
            ([A dim]
              (let [width (count (first A))
                    height (count A)]
                (into #{} (reduce concat  
                                  (for [h (range 0 (inc (- height dim)))]
                                    (for [w (range 0 (inc (- width dim)))]
                                      (get-sq A dim h w))))))))
          
          (latin-sqs [A]
            (into #{} (filter latin? (squares A ))))
          
          (vec-to-map
            ([v]
              (vec-to-map {} v))
            ([res v]
              (if (empty? v)
                res
                (if (nil? (res (first v)))
                  (vec-to-map (conj res {(first v) 1}) (rest v))
                  (vec-to-map (conj res {(first v) (inc (res (first v)))}) (rest v))))))
          ]
         (vec-to-map (map (comp count first) (into #{} (reduce concat (map latin-sqs (alignments V))))))))

(defmacro timing [expr]
  `(let [start# (System/currentTimeMillis)
         ret# ~expr]
     (println (str "Duration: " (- (System/currentTimeMillis) start#)))
     ret#))
    

    
    
                  
    