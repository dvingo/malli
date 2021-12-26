(ns malli.instrument.cljs
  (:require
    [cljs.analyzer.api :as ana-api]
    [malli.core :as m]
    [malli.generator :as mg]))

(defn emit-instrument-fn [{:keys [schema filters] :as opts} ns-sym fn-sym]
  (let [opts (assoc (select-keys opts [:gen :scope :report])
               ;; At macroexpansion time the schema will be a JVM object, this converts it to a JS object.
               :schema `(m/function-schema ~(m/form schema)))]
    (if filters
      `(when (some #(% '~ns-sym (var ~fn-sym) ~opts) ~filters)
         (.log js/console "instrumenting FN: " '~fn-sym)
         (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
         (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
         '~fn-sym)
      `(do
         (.log js/console "instrumenting FN: " '~fn-sym)
         (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
         (set! ~fn-sym (m/-instrument ~opts ~fn-sym))
         '~fn-sym))))

(defn instrument*
  [{:keys [_scope _report _filters _gen data] :or {data (m/function-schemas)} :as opts}]
  (let [r
        (reduce
          (fn [acc [ns-sym sym-map]]
            (reduce-kv
              (fn [acc' fn-sym schema-map]
                (conj acc'
                  (emit-instrument-fn (assoc opts :schema (:schema schema-map))
                    ns-sym
                    (symbol (str ns-sym) (str fn-sym)))))
              acc sym-map))
          []
          data)]
    `(filterv some? ~r)))

(defmacro instrument!
  "Instruments all functions in the function-schemas atom."
  [{:keys [_scope _report _filters _gen] :as opts}]
  (instrument* opts))

(defn emit-unstrument-fn [{:keys [schema filters] :as opts} ns-sym fn-sym]
  (let [opts (assoc (select-keys opts [:gen :scope :report])
               ;; At macroexpansion time the schema will be a JVM object, this converts it to a JS object.
               :schema `(m/function-schema ~(m/form schema)))
        replace-with-orig
             `(when-let [orig-fn# (get @instrumented-vars '~fn-sym)]
                (.log js/console "unstrumenting FN: " '~fn-sym)
                (swap! instrumented-vars #(dissoc % '~fn-sym))
                (set! ~fn-sym orig-fn#)
                '~fn-sym)]
    (if filters
      `(when (some #(% ~ns-sym (var ~fn-sym) ~opts) ~filters)
         ~replace-with-orig)
      replace-with-orig)))

(defn unstrument*
  [opts]
  (let [r (reduce
            (fn [acc [ns-sym sym-map]]
              (reduce-kv
                (fn [acc' fn-sym schema-map]
                  (conj acc'
                    (emit-unstrument-fn (assoc opts :schema (:schema schema-map))
                      ns-sym
                      (symbol (str ns-sym) (str fn-sym)))))
                acc sym-map))
            []
            (m/function-schemas))]
    `(filterv some? ~r)))

(defmacro unstrument! [opts] (unstrument* opts))

(defn -collect! [simple-name {:keys [meta] :as var-map}]
  (let [ns     (symbol (namespace (:name var-map)))
        schema (:malli/schema meta)]
    (when schema
      (m/-register-function-schema! ns simple-name schema (m/-unlift-keys meta "malli"))
      `(do (m/-register-function-schema! '~ns '~simple-name ~schema ~(m/-unlift-keys meta "malli"))
           '~(:name var-map)))))

(defn -sequential [x] (cond (set? x) x (sequential? x) x :else [x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generative testing function return values vs parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn emit-check [{:keys [schema]} fn-sym]
  `(let [schema# (m/function-schema ~(m/form schema))
         fn#     (or (get @instrumented-vars '~fn-sym) '~fn-sym)]
     (when-let [err# (mg/check schema# fn#)]
       ['~fn-sym err#])))

(defn check* []
  (let [r (reduce (fn [acc [ns-sym sym-map]]
                    (reduce-kv (fn [acc' fn-sym schema-map]
                                 (conj acc' (emit-check schema-map (symbol (str ns-sym) (str fn-sym)))))
                      acc sym-map))
            []
            (m/function-schemas))]
    `(into {} ~r)))

(defmacro check []
  (check*))

(defmacro collect!
  "Adds all functions that have :malli/schema metadata to the function schemas atom."
  ([] `(collect! ~{:ns (symbol (str *ns*))}))
  ([{:keys [ns]}]
   (reduce (fn [acc [var-name var-map]] (let [v (-collect! var-name var-map)] (cond-> acc v (conj v))))
     #{}
     (mapcat (fn [n]
               (let [ns-sym (cond (symbol? n) n
                                  ;; handles (quote ns-name) - quoted symbols passed  to cljs macros show up this way.
                                  (list? n) (second n)
                                  :else (symbol (str n)))]
                 (ana-api/ns-publics ns-sym)))
       (-sequential ns)))))

(defn collect-all-ns* []
  `(collect! {:ns ~(ana-api/all-ns)}))
