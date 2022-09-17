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
  {:malli/schema [:=> [:cat :int [:* :int]] :any]}
  [x & args]
  (prn "X is " x " args are " args)
  123)

;(defn pure-vary
;  {:malli/schema [:=> [:cat [:* :string]] some?]}
;  [& x] x)
;
;(defn multi-and-vary
;  {:malli/schema [:function
;                  [:=> [:cat :int] :int]
;                  [:=> [:cat :string :string] :string]
;                  [:=> [:cat :int :string [:* int?]] [:sequential :int]]]}
;  ([x] x)
;  ([x y] y)
;  ([x y & z] z))
;
;
;(defn multi-and-vary2
;  {:malli/schema [:function
;                  [:=> [:cat :int] :int]
;                  [:=> [:cat :string :string :string :string] :string]
;                  [:=> [:cat :int :string [:* int?]] [:sequential :int]]]}
;  ([x] x)
;  ([x y z a] y)
;  ([x y & z] z))
;
;(defn func2
;  [x y & args]
;  (println "in func2"))
;
;(defn solo-arity [x] (println "x"))
;
;(defn two-arity
;  ([x] (println "just x"))
;  ([x y] (println "x and y"))
;  )

(defn init []
  (js/console.log "INIT!"))

;(comment
;  (meta #'h2/f3))

(defn refresh {:dev/after-load true} []
  ;(.log js/console "hot reload")
  )


;(defn x+y
;  {:malli/schema [:=> [:cat float? float?] :double]}
;  [x y]
;  (+ x y))

;(comment
;  (x+y 5 "10"))

;(defn sum [a b] (+ a b))

;(def sum2
;  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
;                  :report (pretty/reporter)}
;    sum))
;
;(m/=> sum [:=> [:cat :int :int] :int])
;
;(comment
;  (sum 1 2)
;  (sum "1" 2))
;
;(set! sum
;  (m/-instrument {:schema (m/schema [:=> [:cat :int :int] :int])
;                  :report (pretty/reporter)}
;    sum))

;(defn minus
;  "a normal clojure function, no dependencies to malli"
;  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
;   :malli/gen    true
;   :malli/scope  #{:input :output}}
;  [x]
;  (dec x))
;
;(defn works?
;  ([x] x)
;  ([x & y] y)
;  ([x y & z] z))
;
;(defn plus-gen
;  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]}
;  [x]
;  (dec x))
;
;(comment
;  @mi/instrumented-vars
;  ((get @mi/instrumented-vars `sum) 1 "2")
;  (sum 1 "2")
;  (sum 1 2)
;  (sum 2)
;  )
;
;(defn plus1 [a] (inc a))
;(m/=> plus1 [:=> [:cat :int] :int])
;
;
;(defn plus2
;  {:validate? true}
;  [a b]
;  (+ a b))
;(m/=> plus2 [:=> [:cat :string :int] :int])


;; multi-arity function
;(defn plus-many
;  ([a] (inc a))
;  ([a b & others]
;   (apply + a b others)))
;
;(m/=> plus-many
;  [:function
;   [:=> [:cat :int] :int]
;   [:=> [:cat :int :int [:* :int]] :int]])
;
;
;(defn plus-many2
;  ([a] (inc a))
;  ([a b c]
;   (apply + [a b c])))
;
;(m/=> plus-many2
;  [:function
;   [:=> [:cat :int] :int]
;   [:=> [:cat :int :int :int] :int]])

;(def pow-gen
;  (m/-instrument
;    {:schema [:function
;              [:=> [:cat :int] [:int {:max 6}]]
;              [:=> [:cat :int :int] [:int {:max 6}]]]
;     :gen    mg/generate}))
;

;(comment
;  (mi/unstrument! nil)
;  (mi/instrument! {:gen mg/generate})
;  (mi/collect!)
;  (minus 8)
;  ;; should always be generated
;  (pow-gen 10)
;  ;; should not be generated
;  (plus-many 10)
;
;  (mi/instrument! {:report  (pretty/reporter)
;                   :filters [
;                             ;(mi/filter-var #{#'sum})
;
;                             (mi/-filter-var (fn [x]
;                                               (println "Checking var: " x)
;                                               (println "meta: " (:validate? (meta x)))
;                                               (:validate? (meta x))
;                                               ))
;                             ;(mi/-filter-ns 'malli.instrument-app)
;
;                             ]})
;  (mi/collect!)
;  (mi/collect! {:ns ['malli.instrument-app]})
;  (minus 5)
;  (minus "5")
;  (m/function-schemas))
;
;(defn minus2
;  "kukka"
;  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
;   :malli/scope  #{:input :output}}
;  [x] (dec x))
;
;(defn ->minus [] minus2)
;(defn minus-test [x] (dec x))
;
;(defn plus-it [x] (inc x))
;(m/=> plus-it [:=> [:cat :int] [:int {:max 6}]])

;(defn sum3 [a b] (+ a b))
;(m/=> sum3 [:=> [:cat :int :int] :int])

;(def small-int [:int {:max 6}])

;(def MyInt (m/-simple-schema {:type 'MyInt, :pred #(and (int? %) (< 100 %))}))

;(defn plus [x] (inc x))
;(m/=> plus [:=> [:cat :int] small-int])

;(defn plusX [x] (inc x))
;(m/=> plusX [:=> [:cat :int] MyInt])
;
;(defn plus-many3
;  {:malli/schema [:=> [:cat [:* :int]] :int]}
;  ([& others] (apply + others)))

;(defn multi-arity-fn
;  {:malli/schema
;   [:function
;    [:=> [:cat] [:int]]
;    [:=> [:cat :int] [:int]]
;    [:=> [:cat :string :string] [:string]]]}
;  ([] 500)
;  ([a] (inc a))
;  ([a b] (str a b)))

(defn multi-arity-variadic-fn
  #_{:malli/schema
   [:function
    ;[:=> [:cat] [:int]]
    [:=> [:cat :int] [:int]]
    ;[:=> [:cat :string :string] [:string]]
    [:=> [:cat :string :string :string [:* :string]] [:string]]]}
  ([] 500)
  ([a] (inc a))
  ([a b] (str a b))
  ([a b c & more] (str a b c more)))

;(defn variadic-fn1
;  {:malli/schema [:=> [:cat [:* :int]] [:int]]}
;  [& vs] (apply + vs))

;(defn variadic-fn2
;  {:malli/schema [:=> [:cat :int [:* :int]] [:int]]}
;  [a & vs] (apply + a vs))

(defn try-it []
  (println "in try it")
  ;(my-function-bad 1 "2")
  (println "TRY IT")




   ;(variadic-fn1 1 "2")
   ;(variadic-fn2 1 "2")
   ;(= 3 (variadic-fn1 1 2))
   ;(= 3 (variadic-fn2 1 2))
   ;(= 500 (multi-arity-fn))
   ;(= 2 (multi-arity-fn 1))
   ;(= "ab" (multi-arity-fn "a" "b"))

   ;(multi-arity-fn "a")
   ;(multi-arity-fn 1 2)
   (= 500 (multi-arity-variadic-fn))
   (= 2 (multi-arity-variadic-fn 1))
   (= "ab" (multi-arity-variadic-fn "a" "b"))
   (= "ab(\"c\")" (multi-arity-variadic-fn "a" "b" "c" :x))
  (println (multi-arity-variadic-fn "a" "b" "c" :x))
  ;(multi-arity-variadic-fn "a")

   ;(multi-arity-variadic-fn 1 2)

  ;(println (multi-arity-variadic-fn "1" "2" "3" :c))
  (println (multi-arity-variadic-fn 1))

  (.log js/console "HELLO?")

  ;(my-function-bad 1 5)
  ;(my-function-bad 1)
  ;(plus-many3 "nil")


  ;(pure-vary "hi" "abc")
  (println "minus2")


  ;(plus-many 10 10)
  ;(multi-fnXX 1 3)
  ;(plus-many2 10)
  ;(minus2 1 )
  )

(defn ^:dev/after-load x []

  (println "AFTER LOAD - malli.dev.cljs/start!")
  (println "AFTER LAOAD")

  (md/start!)

  (js/setTimeout try-it 100))
