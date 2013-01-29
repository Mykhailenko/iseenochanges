(ns com.iseenochanges.core.main
  (:gen-class :main true)
  (:require [com.iseenochanges.core.fswatcher :as watcher]
            ))

(defn -main [& arg]
  (do 
    (println "I see no changes!" arg)
    (println (watcher/watchDir "d:\\ant"))
    ))
