(ns com.iseenochanges.core.fswatcher)

(import '(java.nio.file FileSystems 
                        WatchService 
                        Paths 
                        Path
                        StandardWatchEventKinds
                        WatchEvent
                        ))

(defn getInfo 
  [path]
  (str "All good with " path))

(defn watchDir
     [dirName]
     (let [watchService (.newWatchService (FileSystems/getDefault))
           path (Paths/get dirName (into-array String []))]
       (.register path watchService (into-array [StandardWatchEventKinds/ENTRY_MODIFY]))
       (while true
         (let [key (.take watchService)
               events (.pollEvents key)
               fst (first events)]
           (println "Something changed ")
           (println (.toString (.kind fst)))
           (println (.toString (.watchable key)))
           (.reset key)
           ))))
        
    
    