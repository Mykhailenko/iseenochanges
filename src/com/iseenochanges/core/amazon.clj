(defn remove-all-bad-characters [s]
  (clojure.string/replace s #"^a-zA-Z " ""))
  
(def text "This is a test. This is a programming test. This is a programming test in any language")
(def w (re-seq #"[a-zA-Z]+" (remove-all-bad-characters text)))
(def example 
  "This is a test. This is a programming test. This is a programming test in any language.\n4\nthis\na\ntest\nprogramming")
(def R (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.ByteArrayInputStream. (. example getBytes)))))

(defn f [col w]
  (let [c (map-indexed vector col)]
    (first (first (filter #(.equalsIgnoreCase w (second %)) c))))) 
  
(defn minindex [col words i]
   (sort (map #(let [v (f col %)]
           (if (nil? v)
             v
             (+ i v)
           )) words)))
(defn countWords [col]
  (if (some #(= % nil) col)
    Integer/MAX_VALUE
    (inc (- (last col) (first col)))))
  
(defn make-str [sol words]
  (let [col (first sol)
        from (first col)
        to (last col)]
    (reduce str (rest (interleave 
            (repeat " ")
            (for [i (range from (inc to))]
              (words i))))
                )))
  
(defn solution []
  (let [text (. R readLine)
        quantity (Integer/parseInt (. R readLine))
        words (into [] (for [i (range quantity)]
                         (. R readLine)))
        all-words (vec (re-seq #"[a-zA-Z]+" text))
        minest    (apply min-key second 
                         (map #(vector % (countWords %)) 
                              (for [i (range (count all-words))]
                                (minindex (subvec all-words i) words i))))
        ]
    (make-str minest all-words)
    ))
