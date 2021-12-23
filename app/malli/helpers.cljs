(ns malli.helpers
  (:require
    [malli.core :as m]))

(defn helper1 [a b]
  (str a b))

(m/=> helper1 [:=> [:cat [:string {:min 1}] [:string {:min 1}] ] [:string {:min 2}]])
