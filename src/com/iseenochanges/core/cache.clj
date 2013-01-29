(ns com.iseenochanges.core.cache)

(import '(java.lang Thread ))

(defmacro with-new-thread [& body]
  `(.start (Thread. (fn [] ~@body))))

(defmacro cache [& id]
  `(if (cache-contain ~@id)
     (get-from-cache ~@id)
     (get-from-persistence ~@id)))
