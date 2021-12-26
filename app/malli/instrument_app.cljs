(ns malli.instrument-app
  (:require-macros [malli.dev.cljs :as dev])
  (:require
    [malli.clj-kondo :as mari]
    [malli.instrument.cljs :as im2]
    [helix.core :as h :refer [defnc $]]
    [helix.hooks :as hooks]
    [malli.helpers]
    [helix.dom :as d]
    [malli.dev.pretty :as pretty]
    [malli.generator :as mg]
    ["react-dom" :as rdom]
    [malli.core :as m]))

(comment
  (type (first (keys @im2/instrumented-vars)))
  (mari/emit-cljs!)
  (mari/collect)
)

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
  (sum 1 1)
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
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/gen true
   :malli/scope #{:input :output}}
  [x]
  (dec x))

(defn plus-gen
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

(defn plus1 [a] (inc a))
(m/=> plus1 [:=> [:cat :int] :int])

(defn plus2
  {:validate? true}
  [a b]
  (+ a b))
(m/=> plus2 [:=> [:cat :string :int] :int])

;; multi-arity function
(defn plus-many
  ([a] (inc a))
  ([a b & others]
   (apply + a b others)))

(m/=> plus-many
  [:function
   [:=> [:cat :int] :int]
   [:=> [:cat :int :int [:* :int]] :int]])

(def pow-gen
  (m/-instrument
    {:schema [:function
              [:=> [:cat :int] [:int {:max 6}]]
              [:=> [:cat :int :int] [:int {:max 6}]]]
     :gen mg/generate}))

(comment (im2/check))
(comment
  (macroexpand '(im2/check))
  )


(comment (pow-gen 10))

(comment
  (plus-many 5)
  (plus-many 5 8 1 0 20)
  (plus-many "hi")
  )

(comment
  (im2/unstrument! nil)
  (im2/instrument! {:gen mg/generate})
  (im2/collect!)
  (minus 8)
  ;; should always be generated
  (pow-gen 10)
  ;; should not be generated
  (plus-many 10)



  (im2/instrument! {:report  (pretty/reporter)
                    :filters [
                              ;(im2/filter-var #{#'sum})

                              (im2/-filter-var (fn [x]
                                                (println "Checking var: " x)
                                                (println "meta: " (:validate? (meta x)))
                                                (:validate? (meta x))
                                                ))
                              ;(im2/-filter-ns 'malli.instrument-app 'malli.helpers)
                              ;(im2/-filter-ns 'malli.instrument-app)

                              ]})
  (macroexpand '(dev/start2! {}))
  (dev/start2! {})
  (dev/start2! {:report (pretty/reporter)})
  (im2/collect!)
  (im2/collect! {:ns ['malli.instrument-app]})
  (minus 5)
  (minus "5")
  (m/function-schemas))

(defn minus2
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus2)
(defn minus-test [x] (dec x))
(comment (im2/instrument! {}))
(comment ((->minus) 5))
(defn plus-it [x] (inc x))
(m/=> plus-it [:=> [:cat :int] [:int {:max 6}]])
(comment
  (try
    (plus-it 6)
    (catch js/Error e
      (println "got err: " e)))
  (im2/instrument! {}))

(comment
  (im2/instrument! {:report (pretty/reporter)
                           :filters [(im2/-filter-var #{#'sum})]
                           }))

