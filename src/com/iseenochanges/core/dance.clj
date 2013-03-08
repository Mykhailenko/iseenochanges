(defn dance [& args]
  (letfn [(str-case [& args]
            (let [s (reduce str (interleave (sort args) (repeat ", ")))]
              (if (> (count s) 0)
                (.substring s 0 (- (count s) 2))
                "")))
          (seq-case [& args]
            (if (empty? args)
              nil
              (distinct args)))]
  (reify clojure.lang.Seqable 
    (toString  [this] (apply str-case args))
    (seq [this] (apply seq-case args)))))