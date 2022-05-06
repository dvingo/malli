(ns malli.helpers
  (:require
    [malli.core :as m]))

(def defs-small-int
  [:int {:max 6}])

(def int-schema :int)

(defn my-fn1 [a b]
  (+ 10 a b))

(m/=> my-fn1 [:=> [:cat :int :string] :int])

(comment

  (my-fn1 2 "4"))

(defn my-fn2
  ;{:malli/schema [:=> [:cat :int :int] :string]}
  [a b]
  (+ 10 (int a) b))


(m/=> my-fn2 [:=> [:cat :int :int] :int])

(comment
  (my-fn2 2 4))

