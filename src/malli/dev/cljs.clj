(ns malli.dev.cljs
  (:require
    [malli.clj-kondo :as clj-kondo]
    [malli.instrument.cljs :as mi]))

(defmacro start! [options]
  `(do
     ~(mi/unstrument* nil)
     ~(do (clj-kondo/save! {}) nil)
     ;; malli.dev/stop ^^

     ;; register all function schemas and instrument them based on the options
     ~(mi/collect-all-ns*)
     ~(mi/instrument* options)
     ~(do (clj-kondo/emit!) nil)))
