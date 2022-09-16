(ns malli.instrument.cljs
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.walk :as walk]
            [malli.core :as m]))

;;
;; Collect metadata declared function schemas - register them into the known malli.core/-function-schemas* atom based on their metadata.
;;

(defn -collect! [env simple-name {:keys [meta] :as var-map}]
  (let [ns     (symbol (namespace (:name var-map)))
        schema (:malli/schema meta)]
    (when schema
      (let [-qualify-sym (fn [form]
                           (if (symbol? form)
                             (if (simple-symbol? form)
                               (let [ns-data     (ana-api/find-ns ns)
                                     intern-keys (set (keys (ana-api/ns-interns ns)))]
                                 (cond
                                   ;; a referred symbol
                                   (get-in ns-data [:uses form])
                                   (let [form-ns (str (get-in ns-data [:uses form]))]
                                     (symbol form-ns (str form)))

                                   ;; interned var
                                   (contains? intern-keys form)
                                   (symbol (str ns) (str form))

                                   :else
                                   ;; a cljs.core var, do not qualify it
                                   form))
                               (let [ns-part   (symbol (namespace form))
                                     name-part (name form)
                                     full-ns   (get-in (ana-api/find-ns ns) [:requires ns-part])]
                                 (symbol (str full-ns) name-part)))
                             form))
            schema*      (walk/postwalk -qualify-sym schema)
            metadata     (assoc
                           (walk/postwalk -qualify-sym (m/-unlift-keys meta "malli"))
                           :metadata-schema? true)]
        (m/-register-function-schema! ns simple-name schema* metadata :cljs identity)
        `(do
           (m/-register-function-schema! '~ns '~simple-name ~schema* ~metadata :cljs identity)
           '~(:name var-map))))))

(defn -sequential [x] (cond (set? x) x (sequential? x) x :else [x]))

(defn -collect!*
  [env {:keys [ns]}]
  (reduce (fn [acc [var-name var-map]] (let [v (-collect! env var-name var-map)] (cond-> acc v (conj v))))
    #{}
    (mapcat (fn [n]
              (let [ns-sym (cond (symbol? n) n
                                 ;; handles (quote ns-name) - quoted symbols passed to cljs macros show up this way.
                                 (list? n) (second n)
                                 :else (symbol (str n)))]
                (ana-api/ns-publics ns-sym)))
      (-sequential ns))))

;; intended to be called from a cljs macro
(defn -collect-all-ns [env]
  (-collect!* env {:ns (ana-api/all-ns)}))

;;
;; instrument
;;

"
 - single fixed arity
    - var metadata has no :top-fn key
    - set! symbol itself :=> schema

 - single variadic (pure variadic)
    - only need to overwrite (.cljs$core$IFn$_invoke$arity$variadic f)
    - set! (.cljs$core$IFn$_invoke$arity$variadic f) as instrument fn :=> schema

 - multi fixed arity
    - do not set! symbol itself, that fn just dispatches to the arity fns
    - :top-fn exists (-> m :top-fn :variadic) == false
    - need to loop over each arity - for this you would only instrument the arities that the user provided schema for.
    - verify there is one :=> schema per arity, how? all we get is max-fixed-arity
    - so you can just loop the schemas and only instrument those. This way you could support some of the arities not being instrumented
    ex:
    :top-fn {:variadic? false, :fixed-arity 3, :max-fixed-arity 3, :method-params '[[a] [a b c]],  :arglists      '([a] [a b c]), :arglists-meta (nil nil)}

 - multi with variadic
    - do not set! symbol itself, that fn just dispatches to the arity fns
    - set! the arity fns and also set! the variadic fn
    ex: :top-fn {:variadic? true, :fixed-arity 2, :max-fixed-arity 2, :method-params [[x] [x y]], :arglists ([x] [x y] [x y & z]), :arglists-meta (nil nil nil)}

"
(def default-schema-keys (set (filter keyword? (keys (m/default-schemas)))))

(defn mock-cljs-schema
  "Takes malli schema data and replaces all non default schemas with :any"
  [schema]
  (walk/postwalk (fn [form] (if (or (coll? form) (contains? default-schema-keys form))
                              form :any))
    schema))
(comment
  [(mock-cljs-schema [:=> [:cat :string 'my-schema] :string]) [:=> [:cat :string 'my-schema] :string]]
  )

(defn emit-multi-arity-inst-code [fn-sym schema-map schema]
  (when-not (= (first schema) :function) (throw (IllegalArgumentException. (str "Multi-arity function " fn-sym " must have :function schema. You provided: "
                                                                             (pr-str schema)))))
  (let [schema-tuples (map (fn [s] [(mock-cljs-schema s) s]) (rest schema))
        arity->info   (into {} (map (fn [[mock-schema schema]]
                                      (let [arity (:arity (m/-function-info (m/schema mock-schema)))]

                                        [(if (= arity :varargs) "variadic" arity) schema])) schema-tuples))]
    ;arity->info
    `(do
       ~@(map (fn [[arity fn-schema]]
                ;; todo - need to use the same technique as below for pure variadic functions
                (let [arity-fn-sym `(~(symbol (str ".-cljs$core$IFn$_invoke$arity$" arity)) ~fn-sym)]
                  (if (= arity "variadic")
                    `(set! ~arity-fn-sym
                       (let [instrumented# (m/-instrument ~(assoc schema-map :schema fn-schema) ~arity-fn-sym)]
                         (fn ~(symbol (str (name fn-sym) "-variadic")) [& args#]
                           (.log js/console "YYY in multi-arity variadic " args#)
                           (.log js/console "YYY args#: " args#)
                           ;(.log js/console "into [] args#: " (into [] cat args#))
                           ;args#
                           (.log js/console "calling instrumented fn ")
                           (apply instrumented# args#)
                           )))
                    `(set! ~arity-fn-sym (m/-instrument ~(assoc schema-map :schema fn-schema) ~arity-fn-sym)))))
           arity->info))))

(comment
  (emit-multi-arity-inst-code 'hell nil nil [:function [:=> [:cat [:* :int]] :int]])

  (emit-multi-arity-inst-code 'hell nil nil
    [:function
     [:=> [:cat [:and :int [:> 500]]] :int]
     ;[:=> [:cat :string 'my-schema] :string]
     [:=> [:cat :int :string [:* :int]] [:sequential :int]]])
  )

(defn emit-replace-var-code [fn-sym fn-var-meta schema-map schema]
  (println "------------------")
  (println "fn var: " fn-sym (pr-str fn-var-meta))
  (println "schema: " schema)
  ;; todo so here you have to branch.
  ;; for single arity functions it is fine to keep this code.
  ;; for multi-arity and variadic this will not work
  (let [variadic?      (-> fn-var-meta :top-fn :variadic?)
        max-fixed-args (-> fn-var-meta :top-fn :max-fixed-arity)
        ; parse arglists, it comes in with this shape: (quote ([a b]))
        [_ arglists] (:arglists fn-var-meta)
        single-arity?  (= (count arglists) 1)]
    `(do
       (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
       (.log js/console "..instrumented" '~fn-sym)
       ~(cond
          ;; these first 2 should work
          (and (not variadic?) single-arity?)
          `(do
             (set! ~fn-sym (m/-instrument ~schema-map ~fn-sym))
             '~fn-sym)

          (and variadic? single-arity?)
          (do
            `(do
               (set! (.-cljs$core$IFn$_invoke$arity$variadic ~fn-sym)
                 (let [orig-fn#      (.-cljs$core$IFn$_invoke$arity$variadic ~fn-sym)
                       instrumented# (m/-instrument (assoc ~schema-map :scope #{:input :output}) (fn [& args#]
                                                                                                   (let [[fixed-args# rest-args#] (split-at ~max-fixed-args (vec args#))]
                                                                                                     ;(.log js/console "IN INSTRUMENTED FUNCTION " args#)
                                                                                                     ;(.log js/console "ORIG fn: " orig-fn#)
                                                                                                     ;(.log js/console "FIXED ARGS " fixed-args#)
                                                                                                     ;(.log js/console "REST ARGS " rest-args#)
                                                                                                     (apply orig-fn# (into (vec fixed-args#) [(not-empty rest-args#)])))))]
                   (fn ~(symbol (str (name fn-sym) "-variadic"))
                     [& args#]
                     (let [args2# (apply list* args#)]
                       ;(.log js/console "in single arity variadic")
                       ;(.log js/console "ALL ARGS: " args2#)
                       (apply instrumented# args2#)))))
               '~fn-sym))

          ;; multi-arity non-variadic
          :else
          `(do ~(emit-multi-arity-inst-code fn-sym schema-map schema) '~fn-sym)))))

(defn -emit-instrument-fn [env {:keys [gen filters report] :as instrument-opts}
                           {:keys [schema] :as schema-map} ns-sym fn-sym]
  ;; gen is a function
  (let [schema-map       (-> schema-map
                           (select-keys [:gen :scope :report])
                           ;; The schema passed in may contain cljs vars that have to be resolved at runtime in cljs.
                           (assoc :schema `(m/function-schema ~schema))
                           (cond-> report
                             (assoc :report `(cljs.core/fn [type# data#] (~report type# (assoc data# :fn-name '~fn-sym))))))
        schema-map-with-gen
                         (as-> (merge (select-keys instrument-opts [:scope :report :gen]) schema-map) $
                           ;; use the passed in gen fn to generate a value
                           (if (and gen (true? (:gen schema-map)))
                             (assoc $ :gen gen)
                             (dissoc $ :gen)))

        replace-var-code (when-let [fn-var (ana-api/resolve env fn-sym)]
                           (emit-replace-var-code fn-sym (:meta fn-var) schema-map-with-gen schema))]
    (println "----------------------------------------------------------------------------------------")
    (println "REPLACE VAR CODE")
    (clojure.pprint/pprint replace-var-code)
    (if filters
      `(when (some #(% '~ns-sym (var ~fn-sym) ~schema-map) ~filters)
         ~replace-var-code)
      replace-var-code)))

(defn -instrument [env {:keys [data] :or {data (m/function-schemas :cljs)} :as opts}]
  (let [r
        (reduce
          (fn [acc [ns-sym sym-map]]
            (reduce-kv
              (fn [acc' fn-sym schema-map]
                (conj acc' (-emit-instrument-fn env opts schema-map ns-sym (symbol (str ns-sym) (str fn-sym)))))
              acc sym-map)) [] data)]
    `(filterv some? ~r)))

;;
;; unstrument
;;

(defn -emit-unstrument-fn [env {:keys [schema filters] :as opts} ns-sym fn-sym]
  (let [opts              (-> opts
                            (select-keys [:gen :scope :report])
                            ;; The schema passed in may contain cljs vars that have to be resolved at runtime in cljs.
                            (assoc :schema `(m/function-schema ~schema)))
        replace-with-orig (when (ana-api/resolve env fn-sym)
                            `(when-let [orig-fn# (get @instrumented-vars '~fn-sym)]
                               (swap! instrumented-vars #(dissoc % '~fn-sym))
                               (set! ~fn-sym orig-fn#)
                               (.log js/console "..unstrumented" '~fn-sym)
                               '~fn-sym))]
    (if filters
      `(when (some #(% '~ns-sym (var ~fn-sym) ~opts) ~filters)
         ~replace-with-orig)
      replace-with-orig)))

(defn -unstrument [env opts]
  (let [r (reduce
            (fn [acc [ns-sym sym-map]]
              (reduce-kv
                (fn [acc' fn-sym schema-map]
                  (conj acc' (-emit-unstrument-fn env (assoc opts :schema (:schema schema-map))
                               ns-sym (symbol (str ns-sym) (str fn-sym)))))
                acc sym-map)) [] (m/function-schemas :cljs))]
    `(filterv some? ~r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generative testing, check function return values vs their parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -emit-check [{:keys [schema]} fn-sym]
  `(let [schema# (m/function-schema ~schema)
         fn#     (or (get @instrumented-vars '~fn-sym) ~fn-sym)]
     (when-let [err# (perform-check schema# fn#)]
       ['~fn-sym err#])))

(defn -check []
  (let [r (reduce (fn [acc [ns-sym sym-map]]
                    (reduce-kv (fn [acc' fn-sym schema-map]
                                 (conj acc' (-emit-check schema-map (symbol (str ns-sym) (str fn-sym)))))
                      acc sym-map)) [] (m/function-schemas :cljs))]
    `(into {} ~r)))

;;
;; public api
;;

(defmacro check
  "Checks all registered function schemas using generative testing.
   Returns nil or a map of symbol -> explanation in case of errors."
  [] (-check))

(defmacro collect!
  "Reads all public Vars from a given namespace(s) and registers a function (var) schema if `:malli/schema`
   metadata is present. The following metadata key can be used:

   | key             | description |
   | ----------------|-------------|
   | `:malli/schema` | function schema
   | `:malli/scope`  | optional set of scope definitions, defaults to `#{:input :output}`
   | `:malli/report` | optional side-effecting function of `key data -> any` to report problems, defaults to `m/-fail!`
   | `:malli/gen`    | optional value `true` or function of `schema -> schema -> value` to be invoked on the args to get the return value"
  ([] `(collect! ~{:ns (symbol (str *ns*))}))
  ([args-map] (-collect!* &env args-map)))

(defmacro instrument!
  "Applies instrumentation for a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (-instrument &env {}))
  ([opts] (-instrument &env opts)))

(defmacro unstrument!
  "Removes instrumentation from a filtered set of function Vars (e.g. `defn`s).
   See [[malli.core/-instrument]] for possible options."
  ([] (-unstrument &env {}))
  ([opts] (-unstrument &env opts)))

(defn multi-and-vary2
  ([x] x)
  ([x y] y)
  ([x y & z] z))

;; so i have a strategy. now the trouble is lining up the malli schemas for each function with the arity.
;; this is parsing essentially.

;; can use the malli regex schemas to parse

"
The idea is to use the existing -instrument code for verifying the function schemas by removing any cljs
symbols in order to make valid Clojure malli schemas during compilation

- need to check if malli validates your arities match the function's
- so they don't verify for clojure
- but for cljs we need to match up each :=> with its arity so that we can set! that arity with the instrumented version
"
(comment
  (
   (m/-instrument {:schema
                   [:function
                    [:=> [:cat :int] :int]
                    ;[:=> [:cat :string :string] :string]
                    [:=> [:cat :int :string [:* :int]] [:sequential :int]]]
                   }
     multi-and-vary2)
   5 "a" 5)
  )

;; okay this is working
;; now need to take :function schema in cljs with invalid clojure schema symbols and update it to work for clojure

(def seq-schema-keys (set (keys (m/sequence-schemas))))


(comment
  (mock-cljs-schema
    [:function
     [:=> [:cat :int] :int]
     [:=> [:cat :string 'my-schema] :string]
     [:=> [:cat :int :string [:* :int]] [:sequential :int]]]))

(comment
  (let [schema      (m/schema
                      [:function
                       [:=> [:cat :int] :int]
                       [:=> [:cat :string :string] :string]
                       [:=> [:cat :int :string [:* :int]] [:sequential :int]]])
        options     {}
        f           (fn [])
        props       {}
        arity->info (->> (m/children schema)
                      (map (fn [s] (assoc (m/-function-info s) :f (m/-instrument (assoc props :schema s) f options)))))
        ]
    arity->info
    )
  [fn-sym fn-var schema-map schema]
  )

(comment
  (m/type [:=> [:cat :int] 'abcd])
  (m/-regex-min-max [:=> [:cat :int] 'abcd])
  (m/-regex-min-max [:cat :string :string :string :string])
  (m/-regex-min-max [:* int?])
  (m/-instrument
    ))
