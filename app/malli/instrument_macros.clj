(ns malli.instrument-macros
  (:require
    [clojure.pprint :refer [pprint]]
    [cljs.analyzer.api :as ana-api]
    [cljs.compiler :as com]
    [cljs.analyzer :as ana]
    [malli.core :as m]
    [malli.registry :as mr]
    [cljs.env :as env]
    ))
(comment
  ;(::ana/namespaces @env/*compiler*)
  )

(def init)

;(defn get-compiler [env]
;  (::ana/namespaces env/*compiler*)
;  )

;; keys of of compiler:
;; (:cljs.analyzer/namespaces :shadow/ns-roots :shadow.build.cljs-bridge/state
;; :cljs.analyzer/data-readers :shadow/goog-provides
;; :shadow/protocol-prefixes :cljs.analyzer/externs
;; :shadow/js-properties :js-module-index :goog-names :cljs.analyzer/constant-table
;; :shadow/cljs-provides :shadow.lazy/ns->mod :shadow/js-namespaces
;; :options :goog-modules)
(defmacro replace-var [s]
  (println "keys of of compiler: " (keys @env/*compiler*))
  (println "PLAY compiler: " (with-out-str (pprint (sort (keys (::ana/namespaces @env/*compiler*))))))

  (println "replacing sym: " s)
  (println "ENV: " (keys &env))
  ;`(def ~'hi 500)
  ; `(def '~s 40)
  ;`(def '~s "3000")
  (println "op: " (:op (ana-api/resolve &env s)))
  (println "type of ns: " (type (:ns (ana-api/resolve &env s))))
  (println "ns: " (:ns (ana-api/resolve &env s)))
  (println "info: " (keys (ana-api/resolve &env s)))
  ;(println "compiler keys:\n" (with-out-str (pprint (sort (keys @env/*compiler*)))))
  ;(println "compiler keys:\n" (with-out-str (pprint
  ;                                            (sort (keys (::ana/namespaces @env/*compiler*))))))
  `(do
     [:val '~s]
     ;(def ~s 3030)
     (def ~'hi300 0x300080)
     [
      ;~(pr-str (keys (ana-api/resolve &env s)))
      ;~(:meta (ana-api/resolve &env s))
      ~(:ns (ana-api/resolve &env s))
      ;~(:op (ana-api/resolve &env s))
      ;(var ~'init)
      ]
     )

  ;`(set! ~s 300000)
  ;`(def `'~s )
  ;`('hi 5)
  ;`(do
  ;   (def ~'abcd 500)
  ;   `[:the-sym ~s])
  )
