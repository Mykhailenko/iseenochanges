
(defn big-devide [n a b]
  (letfn [(sum-under [x a]
                     (let [a0 a
                           k (if (zero? (mod x a))
                               (dec (int (/ x a)))
                               (bigint (/ x a)))
                           an (* a0 k)]
                       (* k (bigdec (/ (+ a0 an) 2)))))]
         (let [sa (sum-under n a)
               sb (sum-under n b)
               sab (sum-under n (* a b))]
           (bigint (- (+ sa sb) sab)))))

(defn power-set [stt]
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
          
(defn k-combinations [n st]
  (letfn [(power-set [stt]
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
                              (into #{} (map (partial jay st) ready)))))]
  (into #{} (filter #(= (count %) n) (power-set st)))) )

(defn dfa 
  ([automat]
    (dfa "" (:start automat) automat))
  ([w start automat]
    (if ((:accepts automat) start)
      ""
      (into [] 
            (map #(dfa
                    (str w (first %))
                    (second %)
                    automat) ((:transitions automat) start))))))
    
(defn fff[x r]
  (println x )
  (if (empty? r)
    nil
    (let [f (first r)]
      (if (number? f)
        (cond 
          (< (- x f) 0) (concat [] [] ) 
          :else (concat [f] (fff (- x f) (rest r)))
          )
        [(fff x (first r))]))))
        
  
  
  
  
  
          