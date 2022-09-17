(ns user
  (:require
    [shadow.cljs.devtools.server :as server]
    [shadow.cljs.devtools.config :as config]
    [shadow.cljs.devtools.api :as sh]))

(comment
  (update-in (config/load-cljs-edn!) [:deps :aliases] conj :test)
  (server/start!
    (update-in (config/load-cljs-edn!) [:deps :aliases] conj :test))
  (server/stop!)
  (sh/watch :instrument)
  )
