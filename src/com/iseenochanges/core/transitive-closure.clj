(def st #{[8 4] [9 3] [4 2] [27 9]})
(defn trans? [v s]
  (or 
    (not (empty? (take 1 (filter #(= % v) s))))
    (not (empty? (take 1 (filter #(and
                                    (= (first %) (first v))
                                    (trans? (vector (second %) (second v)) (disj s %))
                                    ) s))))))
(defn final [st]
  (letfn [(trans? [v s]
                  (or 
                    (not (empty? (take 1 (filter #(= % v) s))))
                    (not (empty? (take 1 (filter #(and
                                                    (= (first %) (first v))
                                                    (trans? (vector (second %) (second v)) (disj s %))
                                                    ) s))))))]
         (let [plain (reduce into #{} st)]
           (into #{} (filter #(trans? % st)(mapcat #(map (fn[item] (vector % item)) plain) plain))))))

(defn connect? [g]
  (letfn [(trans? [v s]
                  (or 
                    (not (empty? (take 1 (filter #(= % v) s))))
                    (not (empty? (take 1 (filter #(and
                                                    (= (first %) (first v))
                                                    (trans? (vector (second %) (second v)) (disj s %))
                                                    ) s))))))
          (final [st]
            (letfn [(trans? [v s]
                            (or 
                              (not (empty? (take 1 (filter #(= % v) s))))
                              (not (empty? (take 1 (filter #(and
                                                              (= (first %) (first v))
                                                              (trans? (vector (second %) (second v)) (disj s %))
                                                              ) s))))))]
                   (let [plain (reduce into #{} st)]
                     (into #{} (filter #(trans? % st)(mapcat #(map (fn[item] (vector % item)) plain) plain))))))]
         (let [graph (reduce conj g (into #{} (map reverse g)))
               nodes (into [] (into #{} (mapcat identity g)))
               transitive-closure (final graph)]
           (not (some nil? (for [x (range 0 (dec (count nodes)))
                                 y (range (inc x) (count nodes))]
                             (if (transitive-closure [(nodes x) (nodes y)])
                               [(nodes x) (nodes y)]
                               nil)))))))

(defn path 
  ([not-vsisited]
    (not (nil? (some true? (map #(path % not-vsisited) (into #{} (mapcat identity not-vsisited)))))))
  ([cur-node not-vsisited]
    (letfn [(remove-single [v item]
                           (if (empty? v)
                             nil
                             (if (= (first v) item)
                               (rest v)
                               (concat (vector (first v)) (remove-single (rest v) item)))))
            (dao [cr-node cr-edge nvisited]
                 (if (= (first cr-edge) cr-node)
                   (path (second cr-edge) (remove-single not-vsisited cr-edge ))
                   (path (first cr-edge) (remove-single not-vsisited cr-edge ))))]
           (if (empty? not-vsisited)
             true
             (let [possible (filter #(or (= (first %) cur-node) (= (second %) cur-node)) not-vsisited)]
               (if (empty? possible)
                 false
                 (some true? (map #(dao cur-node % not-vsisited) possible)))))))) 
    
  (defn remove-single [v item]
                           (if (empty? v)
                             nil
                             (if (= (first v) item)
                               (rest v)
                               (concat (vector (first v)) (remove-single (rest v) item)))))
  


(defn final-pascal [v]
  (letfn [(pascal [v]
                  (let [f (bigint (first v))
                        l (bigint (last v))]
                    (mapcat #(if (seq? %)
                               %
                               (list %))
                            (vector f (map #(+ (bigint %) (bigint %2)) v (rest v)) l))))]
         (cons v (lazy-seq (final-pascal (pascal v))))))


(defn roma [se]    
    (let [rd {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}
          sus {"IV" 2 "IX" 2 "XL" 20 "XC" 20 "CD" 200 "CM" 200}
          amount (reduce + (map rd (seq se)))
          s (reduce + (filter #(not (nil? %)) (map sus (for [i (range 0 (dec (count se)))]
                                 (.substring se i (+ i 2))))))]
      (- amount s)))
    
(defn happy? 
  ([n]
    (happy? n #{}))
  ([n s]
    (let [n' (reduce + (map (comp #(* % %) read-string str) (seq (str n))))]
      (cond 
        (= 1 n') true
        (contains? s n') false
        :else (happy? n' (conj s n'))))))
  
(defn flatt [v]
  (letfn [(ww [s] 
              (if (coll? (first s))
                (first (vector s))
                (vector s)))]
         (let [v' (mapcat ww v)]
           (if (= v v')
             v
             (flatt v')))))
  
(def roms {1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M})
(defn fin-rom [x]
  {:pre [(<= x 3999)]}
  (letfn [(roms [x]
                ({1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M} x))
          (topars [x]
                  (reverse (map vector (reverse (into [] (map (comp read-string str) (seq (str x)))))  (iterate #(* % 10) 1))))
          (translate [v]
                     (let [f (first v)]
                       (cond
                         (<= f 3) (reduce str  (repeat f (roms (second v))))
                         (= f 4)  (str (translate [1 (second v)]) (translate [(inc f) (second v)]))
                         (= f 5) (str  (roms (* (first v) (second v))))
                         (<= f 8) (str (translate [5 (second v)]) (translate [(- f 5) (second v)]))
                         (= f 9) (str (translate [1 (second v)]) (translate [1 (* 10 (second v))])))))]
           (str (reduce str (map translate (topars x))))))

(defn final-set [stt]
  (letfn [(lo [x len]
              (str (reduce str (repeat (- len (count x)) "0")) x))
          (jay [v sr]
            (into #{} (filter identity (map #(if (= \0 %2)
                                               nil
                                               %) v (seq sr)))))]
         (let [st (into [] stt)
               co (count st)
               all (for [i (range (int (Math/pow 2 co)))]
                     (Integer/toString i 2))
               ready (map #(lo % co) all)]
           (into #{} (map (partial jay st) ready))))) 
    
    
(defn m-w [op & maps]
  (letfn [(count? [k]
                  (count (filter #(not (nil? %))
                                  (map #(% k) maps))))
          (single [k]
                  (some identity (map #(% k) maps)))]
  (let [ks (into #{} (reduce concat [] (map keys maps)))]
    (map #(if (= 1 (count? %))
            (single %)
            (apply op (filter (fn[i] (not (nil? i))) (map (fn [m] (m %)) maps)) ks))))))
    
    
(defn piarwise [SS]
  (let [W (into [] SS)
        C (count W)]
    (nil? (some #(not (empty? %)) (for [i (range 0 (dec C))
          j (range (inc i) C)]
      (clojure.set/intersection (W i) (W j)))))))

    

        
(defn intervals [x]
  (letfn [(lol [op k v]
            (cond
              (empty? v) v
              (nil? (second v)) (cons (first v) nil)
              (op (first v) (second v)) (cons (first v) (cons k (lazy-seq (lol op k (rest v)))))
              :else  (cons (first v) (lazy-seq (lol op k (rest v))))))]
         (map #(vector (first %)
                       (last %)) (filter #(number? (first %))
                                         (partition-by keyword? 
                                                       (lol #(not= (inc %) %2) 
                                                            :less 
                                                            (sort (distinct x))))))))
        
(defn redd 
  ([op col]
    (redd op (first col) (rest col)))
  ([op init col]
    (if (empty? col)
      (vector init)
      (let [new (op init (first col))]
        (cons init (lazy-seq (redd op new (rest col))))))))

(defn get-in-maze
  ([maze x y]
    (get-in-maze maze x y \#))
  ([maze x y def]
    (if (or (>= x (count maze)) (< x 0))
      def
      (if (or (>= y (count (maze x))) (< y 0))
        def
        (.charAt (maze x) y)))))
(defn free-neirs 
  ([maze lst]
    (free-neirs maze (first lst) (second lst)))
  ([maze x y]
    (filter #(not (nil? %))
            (for [i [-1 0 1]
                  j [-1 0 1]
                  :when (or (zero? i) (zero? j))]
              (let [cell (get-in-maze maze (+ x i) (+ y j))]
                (if (or (= cell \space) (= cell \M) (= cell \C))
                  [(+ x i) (+ y j)]
                  nil))))))
(defn expand-maze [maze visited]
  (into #{} (mapcat (partial free-neirs maze) visited)))

(defn run-maze [maze visited]
  (let [visited' (expand-maze maze visited)]
    (if (= visited visited')
      visited
      (run-maze maze visited'))))

(defn find-start [maze x ]
    (let [f (first maze)
          ind (.indexOf f "M")]
      (if (> ind -1)
        [x ind]
        (find-start (rest maze) (inc x)))))
(defn find-end [maze x]
    (let [f (first maze)
          ind (.indexOf f "C")]
      (if (> ind -1)
        [x ind]
        (find-end (rest maze) (inc x)))))
(defn traverse [maze]
  (let [start (find-start maze 0)
        end (find-end maze 0)]
    (not (nil? ((run-maze maze #{start}) end)))))
(defn final-traverse [maze]
  (letfn [(get-in-maze
            ([maze x y]
              (get-in-maze maze x y \#))
            ([maze x y def]
              (if (or (>= x (count maze)) (< x 0))
                def
                (if (or (>= y (count (maze x))) (< y 0))
                  def
                  (.charAt (maze x) y)))))
          (free-neirs 
            ([maze lst]
              (free-neirs maze (first lst) (second lst)))
            ([maze x y]
              (filter #(not (nil? %))
                      (for [i [-1 0 1]
                            j [-1 0 1]
                            :when (or (zero? i) (zero? j))]
                        (let [cell (get-in-maze maze (+ x i) (+ y j))]
                          (if (or (= cell \space) (= cell \M) (= cell \C))
                            [(+ x i) (+ y j)]
                            nil))))))
          (expand-maze [maze visited]
            (into #{} (mapcat (partial free-neirs maze) visited)))
          (run-maze [maze visited]
            (let [visited' (expand-maze maze visited)]
              (if (= visited visited')
                visited
                (run-maze maze visited'))))
          (find-start [maze x ]
            (let [f (first maze)
                  ind (.indexOf f "M")]
              (if (> ind -1)
                [x ind]
                (find-start (rest maze) (inc x)))))
          (find-end [maze x]
            (let [f (first maze)
                  ind (.indexOf f "C")]
              (if (> ind -1)
                [x ind]
                (find-end (rest maze) (inc x)))))]
         (let [start (find-start maze 0)
               end (find-end maze 0)]
           (not (nil? ((run-maze maze #{start}) end))))))
  
  
  
(defn polindrome? [x]
  (let [s (seq (str x))
        r (reverse s)]
    (= s r)))
(defn max? [x]
  (not= (count (str x)) (count (str (inc x)))))
        
(defn final-set [stt]
  (letfn [(lo [x len]
              (str (reduce str (repeat (- len (count x)) "0")) x))
          (jay [v sr]
            (into #{} (filter identity (map #(if (= \0 %2)
                                               nil
                                               %) v (seq sr)))))]
         (let [st (into [] stt)
               co (count st)
               all (for [i (range (int (Math/pow 2 co)))]
                     (Integer/toString i 2))
               ready (map #(lo % co) all)]
           (into #{} (map (partial jay st) ready)))))  
  
(defn sums [stt]
  (into #{} (map #(reduce + %) (final-set stt))))
(defn giper-sum [& giper]
  (map sums giper))
  

(defn fff [st]
  (letfn [(lev? [w0 w1]
                {:pre [(= (count w0) (count w1))]}
                (< (count (filter false? (map = (seq w0) (seq w1)))) 2))
          (all-strs [w]
                    (let [c (count w)]
                      (for [i (range 0 (inc c))]
                        (str (.substring w 0 i) " " (.substring w i c)))))
          (LEV? [w0 w1]
                (let [c0 (count w0)
                      c1 (count w1)]
                  (cond 
                    (= c0 c1) (lev? w0 w1)
                    (= (inc c0) c1) (some true? (map #(lev? w1 %) (all-strs w0)))
                    (= c0 (inc c1)) (some true? (map #(lev? w0 %) (all-strs w1)))
                    :else false)))
          (path? [start r]
            (if (< (count r) 2)
              true
              (let [dif (clojure.set/difference r #{start})
                    start' (filter #(if (LEV? start %)
                                      %
                                      nil) dif)]
                (if (empty? start')
                  false
                  (not (nil? (some true? (map #(path? 
                                                 %
                                                 dif)
                                              start'))))))))]
         (not (nil? (some true?
                          (map #(path? % st) st))))))
      
      

(defn mer-with [op & maps]
  (letfn [(get-keys [ & maps]
                    (reduce into #{} (map keys maps)))
          (single? [key maps]
                   (= (count (filter #(not (nil? %)) (map #(% key) maps))) 1)) 
          (vl [key maps]
              (filter #(not (nil? %)) (map #(% key) maps)))]
         (let [ks (apply get-keys maps)]
           (into {} (for [k ks]
                      (if (single? k maps)
                        [k (first (vl k maps))]
                        [k (apply op (vl k maps))]))))))
   
(defn leven [w0 w1]
  (let [c0 (count w0)
        c1 (count w1)]
    (cond 
      (zero? c0) c1
      (zero? c1) c0
      :else (min (inc (leven (.substring w0 0 (dec c0)) w1))
                 (inc (leven w0 (.substring w1 0 (dec c1))))
                 (+ (if (= (.charAt w0 (dec c0)) 
                           (.charAt w1 (dec c1)))
                      1
                      0) (leven (.substring w0 0 (dec c0)) (.substring w1 0 (dec c1))))))))

        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
      
  
  
  
  
  
                
                
                
                
          
  
        
        
        
        
        
    
    

