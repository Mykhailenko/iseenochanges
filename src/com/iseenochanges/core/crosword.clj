    
(defn final [word col]
  (letfn [(prepare [in-vector]
            (into [] (map 
                       #(into [] (filter (fn[item] (not= item \space)) (seq %))) 
                       in-vector)))
          (tranpose [m]
            (apply map vector m))
          (try-to-put [line w prey]
            (let [y (dec prey)
                  word (str \# w \#)]
              (every? true?
                      (for [i (range (count word))]
                        (let [word-char (.charAt word i)
                              char (get line (+ y i) \#)]
                          (or 
                            (and (= char \_) (not= word-char \#))
                            (= char word-char)))))))
          (finl [word v]
            (= true (some true? (mapcat (fn[line] (map 
                                                    #(try-to-put line word %) 
                                                    (range (count line))))
                                        v))))]
  (let [pre (prepare col)]
    (or (finl word pre)
        (finl word (tranpose pre))))))
    
    
    

    