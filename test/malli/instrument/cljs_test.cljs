(ns malli.instrument.cljs-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [clojure.string :as str]
    [malli.instrument.cljs :as mi]
    [malli.core :as m]))

(defn plus [x] (inc x))
(m/=> plus [:=> [:cat :int] [:int {:max 6}]])

(defn ->plus [] plus)

(defn unstrument! [] (with-out-str (mi/unstrument! {:filters [(mi/-filter-ns 'malli.instrument-test)]})))
(defn instrument! [] (with-out-str (mi/instrument! {:filters [(mi/-filter-ns 'malli.instrument-test)]})))
(defn collect! [] (mi/collect! {:ns *ns*}))

(deftest instrument!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (= "21" ((->plus) "2")))
    (is (= 7 ((->plus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (try
      ((->plus) "2")
      (catch js/Error e
        (is (re-find #":malli.core/invalid-input" (.-message e)))))

    (try
      ((->plus) 6)
      (catch js/Error e
        (is (re-find #":malli.core/invalid-output" (.-message e)))))))

(defn minus
  "kukka"
  {:malli/schema [:=> [:cat :int] [:int {:min 6}]]
   :malli/scope #{:input :output}}
  [x] (dec x))

(defn ->minus [] minus)

(collect!)

(deftest collect!-test

  (testing "without instrumentation"
    (unstrument!)
    (is (= 1 ((->minus) "2")))
    (is (= 5 ((->minus) 6))))

  (testing "with instrumentation"
    (instrument!)
    (try
      ((->minus) "2")
      (catch js/Error e
        (is (re-find #":malli.core/invalid-input" (.-message e)))))

    (try
      ((->minus) 6)
      (catch js/Error e
        (is (re-find #":malli.core/invalid-output" (.-message e)))))))
