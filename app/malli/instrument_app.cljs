(ns malli.instrument-app
  (:require-macros
    ;[malli.new-one :refer [a-var]]
    [malli.instrument-macros :as im :refer [instrument2 replace-var]
     ])
  (:require
    [helix.core :as h :refer [defnc $]]
    [helix.hooks :as hooks]
    [helix.dom :as d]
    [malli.dev.pretty :as pretty]
    ["react-dom" :as rdom]
    ;[malli.provider :as mp]
    [malli.core :as m]
    ;[malli.registry :as mr]
    ))

(defnc greeting
  [{:keys [name]}] (d/div "Hello, " (d/strong name) "!"))

(defnc app []
  (let [[{:keys [name]}  set-state] (hooks/use-state {:name "Helix User"})]
    (d/div
      (d/h1 "Welcome!")
      ;; create elements out of components
      ($ greeting {:name name})
      (d/input {:value name
                :on-change #(set-state assoc :name (.. % -target -value))}))))

(defn render
  {:dev/after-load true}
  []
  (.log js/console "Render called")
  (rdom/render ($ app) (js/document.getElementById "app")))

(defn init []
  (js/console.log "INIT!")
  (render)
  )
(defn sum [a b] (+ a b))

(def sum2
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
                 sum))
(m/=> sum [:=> [:cat :int :int] :int])
;(set! sum

;      (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
;                      :report (pretty/reporter)}
;                     sum))

(m/function-schemas)
;(def fn-schemas (malli.instrument-macros/instrument))
;(def x (im/instrument2))

(comment (im/instrument2))
(comment
  (sum 5 10)
  (sum "5" 10)
  (sum2 5 10)
  (sum2 "5" 10)
  )

(def replace-me 5)

(defn replace-it []
  (replace-var replace-me))

(comment (replace-it))

;(comment
;  (cljs.core/unchecked-get malli.instrument-app "replace_me")
;  (.-replace_me malli.instrument-app)
;  (a-var replace-me)
;  replace-me
;  hi300
;  (replace-it)
;
;  v
;  )
;(defn add-things [a b]
;  (+ a b))
;(m/=> add-things [:=> [:cat :int :int] :int])
;
;(comment
;  (meta #'add-things)
;  (m/schema [:=> [:cat :int :int] :int])
;  (m/function-schemas)
;  )
;
;(comment
;  (.-_function_schemas_STAR_ malli.core)
;  (.-__GT_t_malli$core54631 malli.core)
;  (js-keys malli.core)
;  (mr/-schema (m/-registry) :int)
;  (satisfies? malli.core.Schema (mr/-schema (m/-registry) :int))
;  (implements? malli.core.Schema (mr/-schema (m/-registry) :int))
;  (implements? malli.core.IntoSchema (mr/-schema (m/-registry) :int))
;  (m/schema (mr/-schema (m/-registry) :int))
;  (m/schema (mr/-schema (m/-registry) :int))
;  )
;
;(comment
;  (m/schema [:=> [:cat :int :int] :int] {:registry m/default-registry})
;  (m/schema [:=> [:cat :int :int] :int])
;  (m/-lookup! :int nil nil)
;
;  (m/schema? (m/-lookup! :int nil nil))
;  (type (m/-lookup! :int nil nil))
;
;  (m/validate [:maybe string?] "kikka")
;  (m/validate [:maybe string?] "kikka" {:registry m/default-registry})
;  (m/schema [:=> [:cat :int :int] :int])
;  (m/validate [:=> [:cat :int :int] :int] +)
;  (m/schema [:=> [:cat :int] :int])
;  (m/schema [:tuple :int :int])
;
;  (m/schema [:maybe string?])
;
;  (m/validate [:tuple string?] ["kikka"])
;
;  (m/-lookup! [:maybe string?] nil nil)
;
;  (m/-lookup [:maybe string?] nil)
;
;  (mr/-schemas (m/-registry nil))
;
;  (m/-lookup [:maybe string?] nil)
;
;  )
;
;(comment
;  (m/-registry)
;  (m/schema :nil {:registry (m/-registry)})
;  (m/schema :nil {:registry m/default-registry})
;
;  (m/schema
;    (mr/-schemas
;      (m/-registry)
;      ))
;  (type m/default-registry)
;  (m/schema [:=> [:tuple :int :int] :int])
;  (m/schema [fn?]
;            )
;  (m/validate fn? +)
;
;  (m/schema [:=> :cat :nil])
;  (m/validate [fn?] +)
;
;  (mp/provide [[5 3]
;               [5 3]
;               [5 3]
;               [5 3]
;               ])
;
;  (m/schema [:vector [double?]])
;
;  (m/schema [:=> [:tuple [int?] [int?]] [int?]])
;  (m/schema [:tuple [int?] [int?]])
;  )
