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
      ]))

(defn emit-instrument-fn [{:keys [schema scope report filters gen]} ns-sym fn-sym]
  (let [opts (cond-> {:schema `(m/function-schema ~schema)}
                     gen (assoc :gen gen)
                     scope (assoc :scope scope)
                     report (assoc :report report))]
    (if filters
      `(do
         (when (some #(% ~ns-sym (var ~fn-sym) ~opts) ~filters)
           (.log js/console "instrumenting FN: " '~fn-sym)
           (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
           '~fn-sym))
      `(do
         (.log js/console "instrumenting FN: " '~fn-sym)
         (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
         '~fn-sym))))

(defn -filter-ns [& ns] (fn [n _ _] ((set ns) n)))
(defn -filter-schema [f] (fn [_ _ {:keys [schema]}] (f schema)))

(defmacro instrument2
  "Instruments all functions in the function-schemas atom."
  [{:keys [scope report filters gen] :as opts}]
  ;; for each ns -> for each sym, invoke instrument -> except return this as data not as code
  ;; so the function schema is created at runtime
  (let [ns->syms (m/function-schemas)
        ret      (reduce
                   (fn [acc [[_quote ns-sym] sym-map]]
                     (reduce-kv
                       (fn [acc' [_quote fn-sym] inst-map]
                         (conj acc'
                               (emit-instrument-fn (assoc opts :schema (:schema inst-map))
                                                   ns-sym
                                                   (symbol (str ns-sym) (str fn-sym)))))
                       acc sym-map))
                   []
                   ns->syms)]
    (println "Returning: ") (prn ret)
    ret))
