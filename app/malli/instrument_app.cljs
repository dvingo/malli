(ns malli.instrument-app
  (:require
    [malli.provider :as mp]
    [malli.core :as m]
    [malli.registry :as mr]))

(defn init []
  (js/console.log "INIT!"))

(defn add-things [a b]
  (+ a b))
(m/=> add-things [:=> [:cat :int :int] :int])

(comment
  (meta #'add-things)
  (m/schema [:=> [:cat :int :int] :int])
  (m/function-schemas)
  )

(comment
  (mr/-schema (m/-registry) :int)
  (satisfies? malli.core.Schema (mr/-schema (m/-registry) :int))
  (implements? malli.core.Schema (mr/-schema (m/-registry) :int))
  (implements? malli.core.IntoSchema (mr/-schema (m/-registry) :int))
  (m/schema (mr/-schema (m/-registry) :int))
  (m/schema (mr/-schema (m/-registry) :int))
  )

(comment
  (m/schema [:=> [:cat :int :int] :int] {:registry m/default-registry})
  (m/schema [:=> [:cat :int :int] :int])
  (m/-lookup! :int nil nil)

  (m/schema? (m/-lookup! :int nil nil))
  (type (m/-lookup! :int nil nil))

  (m/validate [:maybe string?] "kikka")
  (m/validate [:maybe string?] "kikka" {:registry m/default-registry})
  (m/schema [:=> [:cat :int :int] :int])
  (m/validate [:=> [:cat :int :int] :int] +)
  (m/schema [:=> [:cat :int] :int])
  (m/schema [:tuple :int :int])

  (m/schema [:maybe string?])

  (m/validate [:tuple string?] ["kikka"])

  (m/-lookup! [:maybe string?] nil nil)

  (m/-lookup [:maybe string?] nil)

  (mr/-schemas (m/-registry nil))

  (m/-lookup [:maybe string?] nil)

  )

(comment
  (m/-registry)
  (m/schema :nil {:registry (m/-registry)})
  (m/schema :nil {:registry m/default-registry})

  (m/schema
    (mr/-schemas
      (m/-registry)
      ))
  (type m/default-registry)
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
