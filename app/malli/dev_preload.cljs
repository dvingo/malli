(ns malli.dev-preload
  {:dev/always true}
  (:require
   [malli.instrument-app]
   [malli.dev.cljs-dom-reporter :as dom-reporter]
   [malli.dev.cljs :as dev]))

(dev/start!
  {:report (dom-reporter/dom-reporter) :skip-instrumented? true}
  )
