    
(defn fnl [x]
  (letfn
    [(digits [x]
       (str x))
     (digits-without-last [x]
       (let [s (str x)]
         (.substring s 0 (dec (count s))))) 
     (reverse-digits [x]
       (str (reduce str (reverse (digits x)))))
     (digits-count [x]
       (count (digits x)))
     (split1 [x]
       (str (digits x) (reverse-digits x)))
     (split2 [x]
       (str (digits-without-last x) (reverse-digits x)))
     (get-half [x]
       (let [s (str x)]
         (if (even? (count s))
           (read-string (.substring s 0 (/ (count s) 2)))
           (read-string (.substring s 0 (inc (/ (count s) 2)))))))
     (first-number [len]
       (reduce * (repeat (- len 1) 10)))
     (poli? [x]
       (= (digits x) (reverse-digits x)))
     (next-polinfrome [x]
       (let [c (digits-count x)
             h (get-half x)
             h' (if (poli? x)
                  (inc h)
                  h)
             split-fn (if (even? c)
                        split1
                        split2)
             next-poli (split-fn h')
             c' (digits-count next-poli)]
         (if (< (read-string next-poli) x)
           (next-polinfrome (read-string (split-fn h)))
           (if (> c' c)
             (next-polinfrome (first-number (inc c)))
             (read-string next-poli)))))
     (final [x]
       (cons x (lazy-seq (final (next-polinfrome x)))))]
    (if (poli? x)
      (final x)
      (final (next-polinfrome x)))))
  
  
      
  
  