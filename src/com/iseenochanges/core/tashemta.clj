

(defn osci [init & funs]
  (do 
    (cons init (lazy-seq (apply 
                           osci 
                           ((first funs) init) 
                           (rest (cycle funs)))))))

(defn gcd [a b]
     (if (zero? b)
       a
       (gcd b (mod a b))))
(defn coprime? [a b]
  (= (gcd a b) 1))
	(defn totient [x]
	  (letfn [(gcd [a b]
	               (if (zero? b)
	                 a
	                 (gcd b (mod a b))))
	          (coprime? [a b]
	            (= (gcd a b) 1))]
	         (if (= x 1)
	           1
	           (count (filter (partial coprime? x) (range 1 x))))))
 
 
(defn mp 
  ([tr]
    (mp (into [] tr) 0 0 0))
  ([tr init x y]
    (if (= (inc x) (count tr))
      (+ init ((tr x) y))
        (min (mp tr (+ init ((tr x) y)) (inc x) y)
             (mp tr (+ init ((tr x) y)) (inc x) (inc y))))))
     
    
(defn tic-tac [board]
  (letfn [(line? [key line]
                 (= 3 (count (filter #(= key %) (map #((board (first %)) (second %)) line)))))
          (who-win? [line]
                    (cond 
                      (line? :x line) :x
                      (line? :o line) :o
                      :else nil))]
         (let [lines [[[0 0] [0 1] [0 2]]
                      [[1 0] [1 1] [1 2]]
                      [[2 0] [2 1] [2 2]]
                      [[0 0] [1 0] [2 0]]
                      [[0 1] [1 1] [1 2]]
                      [[0 2] [1 2] [2 2]]
                      [[0 0] [1 1] [2 2]]
                      [[2 0] [1 1] [0 2]]]]
           (first (filter #(not (nil? %)) (map who-win? lines))))))
               
(defn abs [x]
  (if (< x 0)
    (- x)
    x))
(defn world-differ [w1 w2]
  (cond 
    (= (count w1) (count w2)) (<= (dec (count w1)) (count (filter true? (map = (seq w1) (seq w2)))))
    (= (abs (- (count w1) (count w2)))) 1
    ))
(defn differ [w1 w2]
  {:pre [(= (inc (count w1)) (count w2))]}
  (let [r (range 0 (count w2))]
    (map 
      #(let [w1' (str 
                   (.substring w1 0 %)
                   \space
                   (.substring w1 % (count w1)))]
         (count (filter true? (map = (seq w1') (seq w2)))))
      r)))
(defn global-take-while [n p col]
    (if (empty? col)
      nil
      (let [t (and (not (nil? (p (first col) ))) (not (false? (p (first col)))))
            n' (if t (dec n) n)]
        (if (and (true? t) (zero? n'))
          nil
          (cons (first col) (lazy-seq (global-take-while n' p (rest col))))))))

   
(defn lazy-serch [& cols]
  (if
    (some empty? cols) 
    nil
    (if (apply = (map first cols))
      (ffirst cols)
      (let [max (reduce max (map first cols))]
        (apply lazy-serch (map #(if (= (first %) max)
                                  %
                                  (rest %)) cols))))))
(defn prononciation [v]
  (let [v' (mapcat #(list (count %) (first %)) (partition-by identity v))]
    (cons v' (lazy-seq (prononciation v')))))
    
    
(defn sum-of-d 
  ( [n a b] 
    (- (+ (sum-of-d (bigint n) (bigint a)) (sum-of-d (bigint n) (bigint b))) (sum-of-d n (* (bigint a) (bigint b)))))
  ( [n a]
    (let [Xn (if (< (* (bigint (/ n a)) a) n)
               (* (bigint (/ n a)) a)
               (* (dec (bigint (/ n a))) a))]
      (* (bigint (/ (+ a Xn) 2)) (bigint (/ Xn a))))))
    
        
               
               
               
               
   
   
   
   
  