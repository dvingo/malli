(ns malli.instrument-app
  (:require
    [malli.clj-kondo :as mari]
    [malli.instrument.cljs :as im2]
    [malli.helpers :as helpers]
    [helix.core :as h :refer [defnc $]]
    [helix.hooks :as hooks]
    [helix.dom :as d]
    [malli.dev.pretty :as pretty]
    ["react-dom" :as rdom]
    [malli.core :as m]))

(comment
  (type (first (keys @im2/instrumented-vars)))
  (mari/emit!)
  (mari/collect)

  )
;(mari/linter-config (mari/collect) )

(defnc greeting
  [{:keys [name]}] (d/div "Hello, " (d/strong name) "!"))

(defnc app []
  (let [[{:keys [name]} set-state!] (hooks/use-state {:name "Helix User"})]
    (d/div
      (d/h1 "Welcome!")
      ;; create elements out of components
      ($ greeting {:name name})
      (d/button {:on-click #(do
                              (set-state! {:name "changed"})
                              (.log js/console "clicked"))} "button")
      (d/input {:value     name
                :on-change #(set-state! assoc :name (.. % -target -value))}))))

(defn render
  {:dev/after-load true}
  []
  (.log js/console "Render called")
  (rdom/render ($ app) (js/document.getElementById "app")))

(defn init []
  (js/console.log "INIT!")
  (render))

(defn sum [a b] (+ a b))

(comment
  (sum 1000)
  )

(def sum2
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
    sum))
(m/=> sum [:=> [:cat :int :int] :int])
(comment (sum 1 2))
(comment (sum "1" 2))
;(set! sum
;      (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
;                      :report (pretty/reporter)}
;                     sum))

(defn minus
  "a normal clojure function, no dependencies to malli"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]}
  [x]
  (dec x))

(comment
  @im2/instrumented-vars
  ((get @im2/instrumented-vars `sum) 1 "2")
  (sum 1 "2")
  (sum 1 2)
  (sum 2)

  )
;(comment (im2/instrument2))
(comment
  (def
    my-thing
    (im2/play))
  (first my-thing)
  (im2/collect!)
  (im2/collect! {:ns ['malli.instrument-app]})
  (minus 5)
  (m/function-schemas)
  )
(comment (im2/instrument! {:report (pretty/reporter)
                          ;:filters [(im2/filter-var #{#'sum})]
                          }))
(comment (im2/unstrument! nil))
(comment (helpers/helper1 "a" 5) )
