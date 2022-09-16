(ns malli.instrument-app
  (:require
    [malli.clj-kondo :as mari]
    ;[malli.helpers2 :as h2]
    [malli.core :as m]
    [malli.dev.cljs :as dev]
    [malli.dev.pretty :as pretty]
    [malli.generator :as mg]
    [malli.dev.cljs :as md]
    [malli.experimental :as mx]
    [malli.instrument.cljs :as mi]))

(defn my-function-bad
  {:malli/schema [:=> [:cat :int [:* :any]] :any]}
  [x & args]
  (prn "X is " x " args are " args)
  123)

(defn pure-vary
  {:malli/schema [:=> [:cat [:* :any]] some?]}
  [& x] x)

(defn multi-and-vary
  {:malli/schema [:function
                  [:=> [:cat :int] :int]
                  [:=> [:cat :string :string] :string]
                  [:=> [:cat :int :string [:* int?]] :vector]]}
  ([x] x)
  ([x y] y)
  ([x y & z] z))

(defn multi-and-vary2
  {:malli/schema [:function
                  [:=> [:cat :int] :int]
                  [:=> [:cat :string :string :string :string] :string]
                  [:=> [:cat :int :string [:* int?]] :vector]]}
  ([x] x)
  ([x y z a] y)
  ([x y & z] z))

(defn func2
  [x y & args]
  (println "in func2"))

(defn solo-arity [x] (println "x"))

(defn two-arity
  ([x] (println "just x"))
  ([x y] (println "x and y"))
  )

(defn init []
  (js/console.log "INIT!"))

;(comment
;  (meta #'h2/f3))

(defn refresh {:dev/after-load true} []
  ;(.log js/console "hot reload")
  )


(defn x+y
  {:malli/schema [:=> [:cat float? float?] :double]}
  [x y]
  (+ x y))

(comment
  (x+y 5 "10"))

(defn sum [a b] (+ a b))

(def sum2
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
    sum))

(m/=> sum [:=> [:cat :int :int] :int])

(comment
  (sum 1 2)
  (sum "1" 2))

(set! sum
  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
                  :report (pretty/reporter)}
    sum))

(defn minus
  "a normal clojure function, no dependencies to malli"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/gen    true
   :malli/scope  #{:input :output}}
  [x]
  (dec x))

(defn works?
  ([x] x)
  ([x & y] y)
  ([x y & z] z))

(defn plus-gen
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]}
  [x]
  (dec x))

(comment
  @mi/instrumented-vars
  ((get @mi/instrumented-vars `sum) 1 "2")
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


(defn plus-many2
  ([a] (inc a))
  ([a b c]
   (apply + [a b c])))

(m/=> plus-many2
  [:function
   [:=> [:cat :int] :int]

   [:=> [:cat :int :int :int] :int]])

(comment
  (plus-many 5)
  (plus-many 5 8 1 0 20)
  (plus-many "hi")
  (var x)

  (macroexpand '(mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument-test)]})))

(def pow-gen
  (m/-instrument
    {:schema [:function
              [:=> [:cat :int] [:int {:max 6}]]
              [:=> [:cat :int :int] [:int {:max 6}]]]
     :gen    mg/generate}))


(comment
  (mi/unstrument! nil)
  (mi/instrument! {:gen mg/generate})
  (mi/collect!)
  (minus 8)
  ;; should always be generated
  (pow-gen 10)
  ;; should not be generated
  (plus-many 10)

  (mi/instrument! {:report  (pretty/reporter)
                   :filters [
                             ;(mi/filter-var #{#'sum})

                             (mi/-filter-var (fn [x]
                                               (println "Checking var: " x)
                                               (println "meta: " (:validate? (meta x)))
                                               (:validate? (meta x))
                                               ))
                             ;(mi/-filter-ns 'malli.instrument-app)

                             ]})
  (mi/collect!)
  (mi/collect! {:ns ['malli.instrument-app]})
  (minus 5)
  (minus "5")
  (m/function-schemas))

(defn minus2
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope  #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus2)
(defn minus-test [x] (dec x))

(defn plus-it [x] (inc x))
(m/=> plus-it [:=> [:cat :int] [:int {:max 6}]])

(defn sum3 [a b] (+ a b))
(m/=> sum3 [:=> [:cat :int :int] :int])

(def small-int [:int {:max 6}])

(def MyInt (m/-simple-schema {:type 'MyInt, :pred #(and (int? %) (< 100 %))}))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] small-int])

(defn plusX [x] (inc x))
(m/=> plusX [:=> [:cat :int] MyInt])

(defn try-it []

  (println "in try it")
  ;(my-function-bad 1)
  ;(println "minus2")

  ; (plus-many 10)
  (plus-many2 10)


  ;(minus2 1 )
  )

(defn ^:dev/after-load x []
  (println "AFTER LOAD - malli.dev.cljs/start!")
  (md/start!)
  (js/setTimeout try-it 100)

  ;(mi/unstrument!)
  ;; register all function schemas and instrument them based on the options
  ;(md/collect-all!)
  #_(mi/instrument! {:report (pretty/thrower)
                     :filters
                     [(mi/-filter-ns 'malli.helpers2 'malli.instrument-app)]})

  ;(println "f3: " (h2/f3 "500"))

  ;(println "f5 " (h2/f5 [:a 1, :b 2] :c 3, :d 4 ))

  )

(comment
  ((->minus) 5)
  (mi/check)
  (macroexpand '(mi/check))
  (mi/instrument! {:report (pretty/reporter) :filters [(mi/-filter-var #{#'sum})]})
  (m/type (m/schema [:=> [:cat :int] small-int]))
  (m/type (m/function-schema [:=> [:cat :int] small-int]))
  (plus 8)
  (m/function-schemas)
  (mi/instrument!)
  (mi/instrument! {:report  (pretty/reporter)
                   :filters [(mi/-filter-var #{#'plusX})]})
  (plusX 10))
