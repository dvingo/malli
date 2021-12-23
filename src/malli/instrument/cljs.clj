(ns malli.instrument.cljs
  (:require
    [clojure.pprint :refer [pprint]]
    [malli.core :as m]))

(defn emit-instrument-fn [{:keys [schema filters] :as opts} ns-sym fn-sym]
  (let [opts (assoc (select-keys opts [:gen :scope :report])
               ;; At macroexpansion time the schema will be a JVM object, this converts it to a JS object.
               :schema `(m/function-schema ~(m/form schema)))]
    (if filters
      `(when (some #(% ~ns-sym (var ~fn-sym) ~opts) ~filters)
         (.log js/console "instrumenting FN: " '~fn-sym)
         (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
         (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
         '~fn-sym)
      `(do
         (.log js/console "instrumenting FN: " '~fn-sym)
         (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
         (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
         '~fn-sym))))

(defmacro instrument!
  "Instruments all functions in the function-schemas atom."
  [{:keys [_scope _report _filters _gen] :as opts}]
  (let [ret (reduce
              (fn [acc [ns-sym sym-map]]
                (reduce-kv
                  (fn [acc' fn-sym schema-map]
                    (conj acc'
                      (emit-instrument-fn (assoc opts :schema (:schema schema-map))
                        ns-sym
                        (symbol (str ns-sym) (str fn-sym)))))
                  acc sym-map))
              []
              (m/function-schemas))]
    (println "Returning: ") (pprint ret)
    ret))

(defmacro unstrument!
  [])
