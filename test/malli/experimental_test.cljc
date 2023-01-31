(ns malli.experimental-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            #?(:cljs [goog.object :as g])
            [malli.experimental :as mx]
            [malli.instrument :as mi])
  #?(:clj (:require [malli.dev])))

;; normal, no-args
(mx/defn f1 [] 1)

;; normal, args
(mx/defn f2 [x] x)

;; schematized, arg
(mx/defn f3 [x :- :int] x)

;; schematized, many args
(mx/defn f4 :- [:int {:min 0}]
  "int int -> int functions"
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(def AB [:map [:a [:int {:min 0}]] [:b :int]])
(def CD [:map [:c [:int {:min 0}]] [:d :int]])

;; schematized, nested keywords args
(mx/defn f5 :- [:cat :int :int :int :int AB CD]
  "Nested Keyword argument"
  [[& {:keys [a b] :as m1} :- AB]
   & {:keys [c d] :as m2} :- CD]
  [a b c d m1 m2])

;; multi-arity
(mx/defn f6 :- [:int {:min 0}]
  "docstring"
  {:some "meta"}
  ([x :- [:int {:min 0}]] (inc x))
  ([x :- [:int {:min 0}], y :- :int] (+ x y))
  ([x :- [:int {:min 0}], y :- :int & zs :- [:* :int]] (apply + x y zs))
  {:more "meta"})

(def expectations
  [{:var          #'f1
    :var-meta     (meta #'f1)
    :calls        [[nil 1]
                   [[1] #?(:clj ::throws :cljs 1)]]
    :instrumented [[nil 1]
                   [[1] ::throws]]}
   {:var          #'f2
    :var-meta     (meta #'f2)
    :calls        [[[1] 1]
                   [["kikka"] "kikka"]
                   [[] #?(:clj ::throws :cljs nil)]
                   [[1 2] #?(:clj ::throws :cljs 1)]]
    :instrumented [[[1] 1]
                   [["kikka"] "kikka"]
                   [[] ::throws]
                   [[1 2] ::throws]]}
   {:var          #'f3
    :var-meta     (meta #'f3)
    :meta         {:arglists     '([x])
                   :raw-arglists '[[x :- :int]]
                   :schema       [:=> [:cat :int] :any]}
    :calls        [[[1] 1]
                   [["kikka"] "kikka"]
                   [[1 2] #?(:clj ::throws :cljs 1)]]
    :instrumented [[[1] 1]
                   [["kikka"] ::throws]
                   [[1 2] ::throws]]}
   {:var          #'f4
    :var-meta     (meta #'f4)
    :meta         {:doc          "int int -> int functions"
                   :arglists     '([x y])
                   :raw-arglists '([x :- [:int {:min 0}] y :- :int])
                   :schema       [:=> [:cat [:int {:min 0}] :int] [:int {:min 0}]]}
    :calls        [[[1 2] 3]
                   [[-2 1] -1]
                   [[-1 -1] -2]
                   [[1 "2"] #?(:clj ::throws :cljs "12")]]
    :instrumented [[[1 2] 3]
                   [[-2 1] ::throws] ;; input
                   [[2 -3] ::throws] ;; ret
                   [[1 "2"] ::throws]]}
   {:var          #'f5
    :var-meta     (meta #'f5)
    :meta         {:arglists     '([[& {:keys [a b], :as m1}] & {:keys [c d], :as m2}])
                   :raw-arglists '([[& {:keys [a b] :as m1} :- AB]
                                    & {:keys [c d] :as m2} :- CD])
                   :schema       #?(:clj [:=>
                                          [:cat [:maybe [:cat AB]] CD]
                                          [:cat :int :int :int :int AB CD]]
                                    :cljs
                                    '[:=>
                                      [:cat [:maybe [:cat AB]] CD]
                                      [:cat :int :int :int :int AB CD]])}
    :calls        [[[[{:a 1, :b 2}] {:c 3, :d 4}]
                    [1 2 3 4 {:a 1, :b 2} {:c 3, :d 4}]]
                   [[[{:a -1, :b 2}] {:c 3, :d 4}]
                    [-1 2 3 4 {:a -1, :b 2} {:c 3, :d 4}]]]
    :instrumented [[[[{:a 1, :b 2}] {:c 3, :d 4}]
                    [1 2 3 4 {:a 1, :b 2} {:c 3, :d 4}]]
                   [[[{:a -1, :b 2}] {:c 3, :d 4}]
                    ::throws]]}
   {:var          #'f6
    :var-meta     (meta #'f6)
    :meta         {:arglists     '([x] [x y] [x y & zs])
                   :raw-arglists '([x :- [:int {:min 0}]]
                                   [x :- [:int {:min 0}] y :- :int]
                                   [x :- [:int {:min 0}] y :- :int & zs :- [:* :int]])
                   :schema       [:function
                                  [:=> [:cat [:int {:min 0}]] [:int {:min 0}]]
                                  [:=> [:cat [:int {:min 0}] :int] [:int {:min 0}]]
                                  [:=> [:cat [:int {:min 0}] :int [:* :int]] [:int {:min 0}]]]}
    :calls        [[[1] 2]
                   [[-1] 0]
                   [[1 2] 3]
                   [[1 -2] -1]
                   [[-1 2] 1]
                   [[1 2 3 4] 10]
                   [[-1 2 3 4] 8]]
    :instrumented [[[1] 2]
                   [[-1] #?(:clj ::throws :cljs 0)]
                   [[1 2] 3]
                   [[1 -2] #?(:clj ::throws :cljs -1)]
                   [[-1 2] #?(:clj ::throws :cljs 1)]
                   [[1 2 3 4] 10]
                   [[-1 2 3 4] ::throws]]}])

(defn -strument! [mode v]
  (with-out-str
    (mi/-strument!
     {:mode mode
      :filters [(mi/-filter-var #(= % v))]})))

#?(:cljs (defn -prop-js-path [ns prop] (into-array (map munge (conj (str/split (str ns) #"\.") prop)))))
#?(:cljs (defn -get-prop [ns prop] (g/getValueByKeys goog/global (-prop-js-path ns prop))))
#?(:cljs (defn -find-var
           ([v] (let [[ns s] (str/split (subs (str v) 2) #"/")] (-get-prop ns s)))
           ([n s] (-get-prop n s))))

(deftest ^:simple defn-test
  (doseq [{:keys [var var-meta calls instrumented] :as e} expectations]

    (testing "plain calls"
      (doseq [[arg ret] calls]
        (testing (pr-str var arg)
          (if #?(:cljs false :clj (= ::throws ret))

            (is (thrown? #?(:clj Exception :cljs js/Error)
                  (apply var arg)))
            (let [actual (apply var arg)]
              (is (= ret actual)))))))

    (when-let [m (:meta e)]
      (testing "meta"
        (doseq [[k v] m]
          (testing k
            (let [m (k (meta var))]
              (is (= #?(:clj v :cljs v )
                    #?(:clj m :cljs (if (and (list? m) (= 'quote (first m))) (vec (second m)) m))
                    )))))))
    (when instrumented
      (testing "instrumented calls"
        (-strument! :instrument var)
        (try
          (doseq [[arg ret] instrumented]
            (testing (pr-str var arg)
              (if (= ::throws ret)
                (is (thrown? #?(:clj Exception :cljs js/Error)
                      #?(:clj (apply var arg)
                         :cljs
                         (let [accessor (str "cljs$core$IFn$_invoke$arity$" (count arg)), arity-fn (g/get (-find-var var) accessor)]
                           (.apply arity-fn arity-fn arg)))))
                (is (= ret
                      #?(:clj (apply var arg)
                         :cljs (apply (-find-var var) arg)))))))
          (finally
            (-strument! :unstrument var)))))))

(mx/defn ^:malli/always f4-checked :- [:int {:min 0}]
  "int int -> int functions"
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(mx/defn f4-checked-2 :- [:int {:min 0}]
  "int int -> int functions"
  {:malli/always true}
  [x :- [:int {:min 0}], y :- :int]
  (+ x y))

(mx/defn f4-checked-multiarity :- [:int {:min 0}]
  {:malli/always true}
  ([x :- [:int {:min 0}], y :- :int]
   (+ x y))
  ([x :- [:int {:min 2}]]
   x))


(defn always-assertions []
  (doseq [[f description] [[f4-checked ":malli/always meta on var"]
                           [f4-checked-2 ":malli/always meta inside defn"]
                           [f4-checked-multiarity "multiple arities"]]]
    (testing description
      (is (= 3 (f 1 2))
          "valid input works")
      (is (= :malli.core/invalid-input
             (try (f -2 1)
                  (catch #?(:clj Exception :cljs js/Error) e
                    (:type (ex-data e)))))
          "invalid input throws")
      (is (= :malli.core/invalid-output
             (try (f 2 -3)
                  (catch #?(:clj Exception :cljs js/Error) e
                    (:type (ex-data e)))))
          "invalid output throws")))
  (testing "other arity of multiple arity function"
    (is (= 3 (f4-checked-multiarity 3))
        "valid input works")
    (is (= :malli.core/invalid-input
           (try (f4-checked-multiarity 1)
                (catch #?(:clj Exception :cljs js/Error) e
                  (:type (ex-data e)))))
        "invalid input throws")))

(deftest always-test
  (testing "without malli.dev"
    (always-assertions))
  #?(:clj
     (do
       (testing "with malli.dev/start!"
         (malli.dev/start!)
         (try
           (always-assertions)
           (finally
             (malli.dev/stop!))))
       (testing "after malli.dev/stop!"
         (always-assertions)))))
