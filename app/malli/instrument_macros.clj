(ns malli.instrument-macros
  (:require
    [clojure.pprint :refer [pprint]]
    [cljs.analyzer.api :as ana-api]
    [cljs.compiler :as com]
    [cljs.analyzer :as ana]
    [malli.core :as m]
    [malli.instrument :as inst]
    [malli.registry :as mr]
    [malli.dev]
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
  ;(println "PLAY compiler: " (with-out-str (pprint (sort (keys (::ana/namespaces @env/*compiler*))))))
  (println "replacing sym: " s)
  ;(println "ENV: " (keys &env))
  ;`(def ~'hi 500)
  ; `(def '~s 40)
  ;`(def '~s "3000")
  (println "op:      " (:op (ana-api/resolve &env s)))
  (println "type of ns: " (type (:ns (ana-api/resolve &env s))))
  (println "ns: " (:ns (ana-api/resolve &env s)))
  (println "info: " (keys (ana-api/resolve &env s)))
  (println "ns publics: " (ns-publics 'malli.instrument))
  ;(println "malli.dev/hello: " (malli.dev/hello))
  ;(println "compiler keys:\n" (with-out-str (pprint (sort (keys @env/*compiler*)))))
  ;(println "compiler keys:\n" (with-out-str (pprint
  ;                                            (sort (keys (::ana/namespaces @env/*compiler*))))))
  `(do
     [:val '~s]
     ;(def ~s 3030)
     (def ~'hi300 0x300080)
     [
      10000
      5
      ~(str (type '123))
      ~(str (type 'abcd))
      ~(str *ns*)
      ~(symbol (str *ns*))
      (ns-publics '~(symbol (str *ns*)))
      ;~(str *ns*)
      ;~(symbol "malli.instrument" "ANOTHER-VAR3")
      ;~(malli.dev/hello)
      ;~(pr-str (keys (ana-api/resolve &env s)))
      ;~(:meta (ana-api/resolve &env s))
      ;~(:ns (ana-api/resolve &env s))
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
;
;(defmacro instrument []
;  `(let [fn-schemas# (m/function-schemas)]
;     ;; for each ns, replace each var in the ns with (-instrument {:schema schema} orig-fn)
;     (for [ns# (keys fn-schemas#)]
;       (for [[k# v#] (get fn-schemas# ns#)]
;         (m/-instrument {:schema (:schema v#)
;                         (symbol (ns# ))})
;       ;(dossym# (keys (get fn-schemas# ns#))]
;       ;  (let [sym2# (get ns# sym#)]
;       ;    (m/-instrument {:schema (:schema sym2#)}
;       ;                   (symbol ~(:ns sym2#) ~(:name sym2#))))
;       ;  )
;       )
;    )))

(defn emit-instrument-fn [schema-vector fn-sym]
  ;(.log js/console "instrumenting: " ~(str fn-sym))
  ;`[schema-vector]
  (println "fn-sym: " (pr-str fn-sym))
  `(set!

     ~fn-sym
     (m/-instrument {:schema (m/function-schema ~schema-vector)} ~fn-sym))
  )

(defmacro instrument2 []
  ;; for each ns -> for each sym, invoke instrument -> except return this as data not as code
  ;; so the function schema is created at runtime
  (let [ns->syms (m/function-schemas)
        ret      (reduce
                   (fn [acc [[_quote ns-sym] sym-map]]
                     (println "name ns-sym: " ns-sym)
                     (reduce-kv
                       (fn [acc' [_quote fn-sym] inst-map]
                         (println "fn-sym: " fn-sym)
                         (conj acc' (emit-instrument-fn (:schema inst-map)
                                                        (symbol (str ns-sym) (str fn-sym)))))
                       acc sym-map))
                   ['do]
                   ns->syms
                   )]
    (println "would return: ")
    (prn (seq ret))
    (seq ret)

    ;(for [ns-sym ns->syms]
    ;  (for [fn-sym ns-sym]
    ;    (into '(do)
    ;          (emit-instrument-fn (:schema fn-sym) (symbol (str (:ns fn-sym)) (str fn-sym))))))
    ))
