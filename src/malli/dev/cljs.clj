(ns malli.dev.cljs
  (:require
    [malli.clj-kondo :as clj-kondo]
    [malli.core :as m]
    [malli.instrument.cljs :as mi]
    [cljs.analyzer.api :as ana-api]))

(defmacro watch-impl [ns-sym]
  (ana-api/remove-ns ns-sym))

(defmacro start2! [options]
  ;; ok this is not possible with normal cljs macros.
  ;; one idea is to see if you can invoke the compiler directly from the macro since this code will only be run
  ;; during development.
  ;; what I'm thinking is
  ;; add a watch on the function registry atom in clj in the macro
  ;; when the watch fires generate the clojurescript code you want to execute and submit it to the cljs repl
  ;; as if the person submitted a form to the repl.
  ;; The thing that you need to do is get literal symbol for namespaces and vars in order for cljs to work when
  ;; replacing the var values with functions at runtime
  ;; you can do this though because the malli.core/=> macro updates the fn schema registry in clojure, so you have
  ;; the data available in the clojure callback/watch on the atom.

  (let [watch
        `(~'fn [~'_ ~'_ old# new#]
           (.log js/console "IN WATCH FUNCTION")
           (let [data#
                  (->> (for [[n# d#] new#
                                                            :let [no# (get old# n#)]
                                                            [s# d#] d#
                                                            :when (not= d# (get no# s#))]
                                                        [[n# s#] d#])
                                                   (into {})
                                                   (reduce-kv assoc-in {}))]
             (.log js/console "the data is: " (ffirst data#))
             (.log js/console "XXXX the data is: " (watch-impl (ffirst data#)))

             )
           ;(mi/instrument!
          ;  (assoc ~options :data (->> (for [[n# d#] new#
          ;                                    :let [no# (get old# n#)]
          ;                                    [s# d#] d#
          ;                                    :when (not= d# (get no# s#))]
          ;                                [[n# s#] d#])
          ;                           (into {})
          ;                           (reduce-kv assoc-in {}))))
          ;(clj-kondo/emit!)
           )]
    ;(add-watch @#'m/-function-schemas* ::watch
    ;  (fn [_ _ old new]
    ;    (mi/instrument! (assoc options :data (->> (for [[n d] new
    ;                                                    :let [no (get old n)]
    ;                                                    [s d] d
    ;                                                    :when (not= d (get no s))]
    ;                                                [[n s] d])
    ;                                           (into {})
    ;                                           (reduce-kv assoc-in {}))))
    ;    (clj-kondo/emit!)))

    `(do
       ~(mi/unstrument* nil)
       ~(do (clj-kondo/save! {}) nil)
       ;; malli.dev/stop ^^

       ;; register all function schemas
       ~(mi/collect-all-ns*)

       ;; watch the function schemas atom
       ;(add-watch @#'m/-function-schemas* ::watch ~watch)

       [:dan-change-here]
       ~(mi/instrument* options)
       ~(do (clj-kondo/emit!) nil)
       (.log js/console "started instrumentation"))))
