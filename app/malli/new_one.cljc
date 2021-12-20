(ns malli.new-one)
(def init)
(defmacro a-var [s]
  (println "replacing sym: " s)
  (println "ENV: " (keys &env))
  `(do
     [:val '~s]
     (def ~'hi300 0x300080)

     [(keys ) (var ~'init)]))
