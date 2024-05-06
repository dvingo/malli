(ns malli.dev-preload
  {:dev/always true}
  (:require
   [malli.instrument-app]
   [malli.dev.dom-reporter :as dom-reporter]
   [malli.dev.cljs :as malli.dev]))

(malli.dev/start!
  {:report (dom-reporter/dom-reporter-with-thrower)
   :skip-instrumented? true})
