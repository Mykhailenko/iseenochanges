(def b [[:o :e :e] 
              [:o :x :o] 
              [:x :x :e]])

(def gg [[2 0] [2 1] [2 2]])

(defn m2 [k b]
  (let [merg (fn [board line]
               (map #(vector % ((b (first %)) (second %))) line))
        coun (fn [lined key]
               (count (filter #(= (second %) key ) lined)))
        result (fn [lined]
                 (first (first (filter #(= (second %) :e ) lined))))
        exam (fn exam [board k line]
               (let [lined (merg board line)]
                 (if (and (= 1 (coun lined :e))
                          (= 2 (coun lined k)))
                   (result lined)
                   nil)))]
  (filter identity  (map (partial exam b k) 
                         [
                          [[0 0] [0 1] [0 2]]
                          [[1 0] [1 1] [1 2]]
                          [[2 0] [2 1] [2 2]]
                          
                          [[0 0] [1 0] [2 0]]
                          [[0 1] [1 1] [2 1]]
                          [[0 2] [1 2] [2 2]]
                          
                          [[0 0] [1 1] [2 2]]
                          [[2 0] [1 1] [0 2]]
                          ])))) 
(defn fun [col]
  (letfn [(smaller? [x]
                    (< x (reduce + (map (comp #(* % %) read-string str) (seq (str x))))))]
                    (count  (filter smaller? col))))

(defn tramp [f & args]
  (let [r (apply f args)]
    (if (fn? r)
      (tramp r)
      r)))
    
(defn changes [num basis]
  (let [d (int (/ num basis))
        r (rem num basis)]
    (cond
      (= d 0) (list r)
      (< d basis) (list d r)
      :else (concat (changes d basis) (list r)))))

(def board 
     (vec (repeat 6 (vec (repeat 6 nil)) )))
(defn line-coords 
  ([x]
    (line-coords 6 x))
  ([size x]
    (into [] (map vector (repeat size x) (range 0 size)))))
(defn get-coords-board [size]
  (map (partial line-coords size) (range 0 size)))

(defn conj-in
  [coll [k & ks] v]
  (if ks
    (assoc coll k (conj-in (get coll k []) ks v))
    (assoc coll k v)))
(defn get-cell 
  ([board x y]
    (get-cell board x y \space))
  ([board x y def]
    (let [c (count board)]
      (if (or (< x 0)
               (>= x c) 
               (< y 0)
               (>= y c))
        def
        ((board x) y)))))
(defn neirs 
  [board x y]
  (for [dx '(-1 0 1)
        dy '(-1 0 1)
        :when (not (= 0 dx dy))]
    (get-cell board (+ x dx) (+ y dy))))
(defn count-neirs [board x y]
  (count (filter #(= % \#) (neirs board x y))))
(defn result 
  [board x y]
  (if (= (count-neirs board x y) 3)
    \#
    \space))
(defn get-all-cords [size]
  (for [x (range 0 size)
        y (range 0 size)]
    (vector x y)))
(defn step [board]
  (loop [coord (get-all-cords (count board))
         res board]
    (if (nil? coord)
      res
      (let [crd (first coord)
            nw  (result board (first crd) (second crd))]
        (recur (next coord) 
             (conj-in res crd nw)
             )))))
(defn parse [col]
  (into [] (map (comp (partial into []) seq) col)))
(defn com [board]
  (into [] (map #(reduce str %) board)))
(defn pre-final [board]
  (com (step (parse board))))
(defn final [board]
  (map #(str \" % \") (pre-final board)))
(defmacro put-to-board [v]
  `(def board (conj-in board ~v \#)))
(put-to-board [1 1])  
(put-to-board [1 2])  
(put-to-board [2 1])  
(put-to-board [2 2])  
(put-to-board [3 3])  
(put-to-board [3 4])  
(put-to-board [4 3])  
(put-to-board [4 4]) 

(defn tr? [tree]
  (and
    (coll? tree)
    (= 3 (count tree))
    (or (= nil (nth tree 1)) (tr? (nth tree 1)))
    (or (= nil (nth tree 2)) (tr? (nth tree 2)))))
            
(defn perf-square2 [x] 
                       (loop [s (map #(* % %) (range))]
                         (let [sq (first s)]
                           (if (= sq x)
                             true
                             (if (< sq x)
                               (recur (rest s))
                               false)))))
(defn ps [st]
  (letfn [(perf-square [x] 
                       (loop [s (map #(* % %) (range))]
                         (let [sq (first s)]
                           (if (= sq x)
                             true
                             (if (< sq x)
                               (recur (rest s))
                               false)))))]
           (reduce  str 
                    (drop 1 
                          (interleave  (repeat ",")
                                       (filter perf-square (map (comp read-string str) (re-seq #"\d+" st))))))))
                                  
      
      
      

(defn gcd [a b]
     (if (zero? b)
       a
       (gcd b (mod a b))))
(defn lcm [a b]
  (letfn [(gcd [a b]
     (if (zero? b)
       a
       (gcd b (mod a b))))]
         (/ (abs (* a b)) (gcd a b))))
(defn final-lcm [lst]
  (letfn [(gcd [a b]
               (if (zero? b)
                 a
                 (gcd b (mod a b))))
          (lcm [a b]
               (/ (abs (* a b)) (gcd a b))) ]
  (reduce lcm lst)))
(defn qqq [op k v]
  (if (empty? v)
    v
    (concat (map #(if (true? %)
                    k
                    %)
                 (filter #(if (false? %) 
                            nil
                      		    true) (interleave v (map op v (rest v)))))
            (vector (last v)))))
      
(defn lol [op k v]
  (cond
    (empty? v) v
    (nil? (second v)) (cons (first v) nil)
    (op (first v) (second v)) (cons (first v) (cons k (lazy-seq (lol op k (rest v)))))
    :else  (cons (first v) (lazy-seq (lol op k (rest v))))))
    
      
                                  

(defn eq-tr? 
  ([root]
    (eq-tr? (nth root 1) (nth root 2)))
  ([one two]
    (cond 
      (and (nil? one) (nil? two)) true
      (not= (first one) (first two)) false
      :else (or (eq-tr? (nth one 1) (nth two 2))
                (eq-tr? (nth one 2) (nth two 1)) ))))

(defn hof [a & sq]
  (do 
    (if (empty? sq)
      a
      (apply hof (( nth sq 0) a (nth sq 1))  (drop 2 sq)))))


      
                                  