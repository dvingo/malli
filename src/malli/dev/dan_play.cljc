(ns malli.dev.dan-play
  (:require
    [malli.core :as m]
    [malli.registry :as mr]))

(comment
  (m/ast (m/schema [:map [:a :int]]))
  )
