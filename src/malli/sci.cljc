(ns malli.sci
  (:require [borkdude.dynaload :as dynaload]))

(defn get-sci []
  (let [eval-string* (dynaload/dynaload 'sci.core/eval-string* {:default nil})
        init (dynaload/dynaload 'sci.core/init {:default nil})
        fork (dynaload/dynaload 'sci.core/fork {:default nil})]
    [@eval-string* @init @fork]) )

(defn eval-available? [] (every? some? (get-sci)))

(defn evaluator [options fail!]
  #?(:bb      (fn []
                (fn [form]
                  (load-string (str "(ns user (:require [malli.core :as m]))\n" form))))
     :default (let [[eval-string* init fork] (get-sci)]
                (fn [] (if (and eval-string* init fork)
                         (let [ctx (init options)]
                           (eval-string* ctx "(alias 'm 'malli.core)")
                           (fn eval [s]
                             (eval-string* (fork ctx) (str s))))
                         fail!)))))
