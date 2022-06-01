(ns malli.instrument.cljs
  (:import
    [java.time Instant]
    [java.util.jar JarInputStream])
  (:require [cljs.analyzer.api :as ana-api]
            [clojure.walk :as walk]

            [clojure.java.io :as io]
            [malli.core :as m]))

;;
;; Collect schemas - register them into the known malli.core/-function-schemas[-cljs]* atom based on their metadata.
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
            metadata     (walk/postwalk -qualify-sym (m/-unlift-keys meta "malli"))]
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

(defn get-url-protocol [ns-sym]
  (when-let [interns (ana-api/ns-interns ns-sym)]
    (let [file (:file (first (vals interns)))]
      (.getProtocol (io/resource file)))))

(defn is-jar? [ns-sym]
  (println "check " ns-sym)
  (let [proto (get-url-protocol ns-sym)]
    (println "proto is: " proto)
    (= "jar" proto)))

(defn is-file? [ns-sym]
  (= "file" (get-url-protocol ns-sym)))

(defn ns-sym->file [ns-sym]
  (when-let [interns (ana-api/ns-interns ns-sym)]
    (let [file (:file (first (vals interns)))]
      (io/file (io/resource file)))))

;; here you can add logic to loop over only those namespaces that have been modified since last collection
(def collect-state_ (atom {}))

(defn get-nses-to-collect [s_]
  (let [state @s_
        {:keys [last-collect-time ns->file]} state]
    (println "\n\nLAST COLLECT TIME: " last-collect-time)
    (if last-collect-time
      (map first (filter (fn [[_ v]] (.isBefore last-collect-time (Instant/ofEpochMilli (.lastModified v)))) ns->file))
      (ana-api/all-ns))))

(defn update-state! [s_]
  (let [r (filter is-file? (ana-api/all-ns))
        o (zipmap r (map ns-sym->file r))]
    ;(println " in filter swap" r)
    (swap! s_
      (fn [s]
        (-> s
          (assoc :last-collect-time (Instant/now))
          (assoc :ns->file o)
          (assoc :source-files r))))))

;; intended to be called from a cljs macro
(defn -collect-all-ns [env]
  (println "-collect-all-ns")
  (let [nses (get-nses-to-collect collect-state_)]
    (println "COLLECTING : " nses)
    (update-state! collect-state_)
    (-collect!* env {:ns nses})))

;;
;; instrument
;;

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
        replace-var-code (when (ana-api/resolve env fn-sym)
                           `(do
                              (swap! instrumented-vars #(assoc % '~fn-sym ~fn-sym))
                              (set! ~fn-sym (m/-instrument ~schema-map-with-gen ~fn-sym))
                              (.log js/console "..instrumented" '~fn-sym)
                              '~fn-sym))]
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


(defonce some-state_ (atom {:num 1}))

(comment
  (:all-ns @some-state_)
  (:10-interns @some-state_)
  (sort (:non-jars @some-state_))
  (sort (:source-files @some-state_))
  (:file-map @some-state_)
  (:is-jar? @some-state_)
  (io/resource (:file (get (:first-interns @some-state_) 'body)))
  (str (io/resource (:file (get (:first-interns @some-state_) 'body))))
  (type (.openConnection (io/resource (:file (get (:first-interns @some-state_) 'body)))))
  (:file (first (vals (:first-interns @some-state_))))
  (.getProtocol (io/resource (:file (get (:first-interns @some-state_) 'body))))
  (io/file (.toURI (io/resource (:file (get (:first-interns @some-state_) 'body)))))
  (let [b (byte-array 100)]
    (.read (JarInputStream. (io/input-stream (io/resource (:file (get (:first-interns @some-state_) 'body)))))
      b 0 30
      ))
  (ana-api/all-ns)
  (ana-api/ns-interns 'malli.sci)
  (nth (:all-ns @some-state_) 10)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; this is the way to get timestamps for files namespaces
  (java.time.Instant/ofEpochMilli (.lastModified (get (:file-map @some-state_) 'malli.generator)))
  (java.time.Instant/ofEpochMilli (.lastModified (get (:file-map @some-state_) 'malli.instrument-app)))
  )


(defmacro try-it []
  ;(println "state: " @some-state_)
  (swap! some-state_ assoc :all-ns (ana-api/all-ns))
  (swap! some-state_ assoc :first-interns (ana-api/ns-interns (first (ana-api/all-ns))))
  (swap! some-state_ assoc :10-interns (ana-api/ns-interns (nth (ana-api/all-ns) 10)))
  (swap! some-state_ update :num inc)

  (println "try it")
  (swap! some-state_
    (fn [s]
      (println " in swap files only")
      (let [r (filter is-file? (ana-api/all-ns))
            o (zipmap r (map ns-sym->file r))]
        (println " in filter swap" r)
        (-> s
          (assoc :file-map o)
          (assoc :source-files r)))))
  (swap! some-state_ assoc :ns-jar (nth (ana-api/all-ns) 10))
  ;(swap! some-state_ assoc :is-jar? (is-jar (nth (ana-api/all-ns) 10)))
  100
  )

