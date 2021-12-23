(ns malli.instrument.cljs
  (:require-macros [malli.instrument.cljs]))

(defonce instrumented-vars (atom {}))

(defn filter-var [f] (fn [_ s _] (f s)))
