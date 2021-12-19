(ns malli.node-app
  (:require
    [malli.provider :as mp]
    [malli.core :as m]
    [malli.registry :as mr]))

(defn init []
  (js/console.log "INIT!"))

(defn add-things [a b]
  (+ a b))
;(m/=> add-things [:=> [:tuple [:int] [:int]] [:int]])
(comment


  (m/schema [:=> [:tuple :int :int] :int])
  (m/schema [fn?]
            )
  (m/validate fn? +)

  (m/schema [:=> :cat :nil])
  (m/validate [fn?] +)

  (mp/provide [[5 3]
               [5 3]
               [5 3]
               [5 3]
               ])

  (m/schema [:vector [double?]])

  (m/schema [:=> [:tuple [int?] [int?]] [int?]])
  (m/schema [:tuple [int?] [int?]])
  )
