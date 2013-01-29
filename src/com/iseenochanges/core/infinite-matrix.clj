(defn infinite-row [n f]
  (lazy-seq  (cons (f n) (lazy-seq (infinite-row (inc n) f)))))

(defn infinite-matrix 
  ([f]
    (infinite-matrix  f 0 0  ))
  ([f m n]
    (letfn [(infinite-row [n f]
                          (lazy-seq  (cons (f n) (lazy-seq (infinite-row (inc n) f)))))]
           (lazy-seq (cons (infinite-row n (partial f m)) (lazy-seq (infinite-matrix  f (inc m) n ))))))
  ([f m n s t ]
    (letfn [(infinite-row [n f]
                          (lazy-seq  (cons (f n) (lazy-seq (infinite-row (inc n) f)))))]
    (lazy-seq (take s (cons (take t (infinite-row n (partial f m))) (lazy-seq (infinite-matrix  f (inc m) n (dec s) t))))))))