(use 'clojure.java.io)
    
(defn get-sub-by-regexp [regexp line]
  (let [prefix (re-find regexp line)]
    (if (not (nil? prefix))
      (.substring line (count prefix) (dec (count line)))
      nil)))

(defn is-package? [line]
  (get-sub-by-regexp #"package[ ]+" line))
(defn is-import? [line]
  (get-sub-by-regexp #"import[ ]+" line))


(defn find-rules [path all-rules]
  (let [packge-name (with-open [rdr (reader path)]
                      (some is-package? (line-seq rdr)))]
    (some #(if (.startsWith packge-name (first %))
             (second %)
             nil)
          all-rules)))

(defn analyze-file [path all-rules]
  "path is path to file with extension 'java'."
  (let [all-imports (with-open [rdr (reader path)]
                      (doall (filter #(not (nil? %)) (map is-import? (line-seq rdr)))))
        rules (find-rules path all-rules)]
    (distinct (filter #(not (nil? %))
                      (mapcat #(map
                                 (fn[rl]
                                   (if (.startsWith % rl)
                                     (str (make-path-fine path) " ПОЛЕЗ В " rl)
                                     nil))
                                 rules) 
                              all-imports)))))
(defn make-path-fine [path]
  (let [dirty-hack "src\\main\\java"
        len (count dirty-hack)
        index (.indexOf path dirty-hack)]
    (.substring (.replaceAll (.substring path (+ index len) ) "\\\\" ".") 1)))
      
(defn get-all-java-files [root-path]
  (let [directory (clojure.java.io/file root-path)
        files (file-seq directory)]
    (map #(.getPath %) (filter #(.endsWith (.getName %) ".java") files))))
    
(defn checker [root-path]
  (let [forb-rules (read-from-file-safely "rules")
        paths (get-all-java-files root-path)]
    (filter #(not (empty? %))(for [path paths]
      (analyze-file path forb-rules)))))


(defn read-from-file-safely [filename]
  (with-open
    [r (java.io.PushbackReader.
         (clojure.java.io/reader filename))]
      (binding [*read-eval* false]
        (read r))))

