(ns malli.dev.dom-reporter
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.dev.pretty :as pretty]
            [cljs.pprint :refer [pprint]]
            [goog.dom :as gdom]
            [goog.style :as style]))

(def modal-container-dom-node "malli-instrument-modal-container")
(def modal-container-class-name "malli-instrument-modal")
(def modal-content-class-name "malli-instrument-modal-content")

(defn named->str [k] (if (implements? INamed k)
                       (if (namespace k)
                         (str (namespace k) "/" (name k))
                         (name k))
                       k))

(defn hiccup-to-dom
  "Takes valid hiccup vector, returns DOM object tree."
  [hiccup]
  (let [tag (named->str (first hiccup))
        attrs (if (map? (second hiccup)) (second hiccup) {})
        children (if (map? (second hiccup)) (drop 2 hiccup) (drop 1 hiccup))
        style-map (:style attrs)
        on-click (:on-click attrs)
        elem (gdom/createDom tag (clj->js (dissoc attrs :style :on-click)))]
    (when style-map
      (style/setStyle elem (clj->js style-map)))
    (when on-click
      (.addEventListener elem "click" (:on-click attrs)))
    (doseq [child children]
      (if (vector? child)
        (gdom/appendChild elem (hiccup-to-dom child))
        (gdom/appendChild elem (gdom/createTextNode (named->str child)))))
    elem))

(defn render-hiccup!
  "Convert hiccup data to DOM object and append to container element with the given ID."
  [container-id-or-node hiccup-data]
  (let [container (if (string? container-id-or-node) (gdom/getElement container-id-or-node) container-id-or-node)
        dom-tree (hiccup-to-dom hiccup-data)]
    (gdom/removeChildren container)
    (gdom/appendChild container dom-tree)))

(comment
  (gdom/append js/dan (gdom/createDom "h1" #js{:className "hi" :style #js{:color "Blue"}} "hellow"))
  (hiccup-to-dom [:div
                  [:h1 {:style {:color "RED"}} "ERROR"]
                  [:p "The error is: "]
                  [:pre "hello"]])
  (render-hiccup! "dan"
    [:div {:style {:margin "20px"}}
     [:h3 {:style {:padding "10px 20px"}} "CANGED"]
     [:h1 {:style {:color "grey"}} "ERROR"]
     [:p {:style {:border "1px solid yellow"}} "The error is: "]
     [:pre "hello"]]))

(defonce shadow-root_ (atom nil))

(defn find-existing-modal []
  (when-let [root @shadow-root_]
    (.-firstChild root)))

(defn hide-modal [modal]
  (let [modal-style (.-style modal)]
    (set! (.-display modal-style) "none")))

(defn hide-current-modal []
  (hide-modal (find-existing-modal)))

(defn modal-content-style []
  #js{:borderRadius "4px"
      :padding "10px 20px"
      :color "#eee"
      ;:margin-right "110px"
      })

(defn modal-container-style []
  #js {:position "fixed"
       :top "10%"
       :left "50%"
       :transform "translate(-50%, 0)"
       :max-width "50rem"
       ;:box-shadow "4px 4px 4px grey"
       :box-shadow "rgb(78 78 78) 4px 4px 20px 20px"
       ;:box-shadow "#47474c 4px 4px 20px 20px"
       :padding "20px 40px 0"
       :border-radius "4px"
       :background-color "rgb(51 51 54 / 94%)"
       ;"rgb(60 56 56 / 94%)"
       })

(defn modal-hiccup [message]
  [:div ;; shadow-root containing element
   [:div {:className modal-container-class-name
          :style {:position "fixed"
                  :inset "0"
                  :background "rgba(10, 10, 10, 0.74)"}}
    [:div {:className "malli-instrument-modal-container" :style (modal-container-style)}
     [:span {:className "malli-instrument-modal-close"
             :on-click (fn [e] (.log js/console "CLICKED CLOSE" e)
                         (hide-current-modal))
             :style {:position "absolute"
                     :top "0"
                     :right "10px"
                     :font-size "30px"
                     :line-height "30px"
                     :cursor "pointer"}}
      "×"]
     [:div {:style {:display "flex" :align-items "flex-end"}}
      [(if (string? message) :pre :div) {:className modal-content-class-name :style (modal-content-style)}
       message]
      [:img {:style {:width "130px" :height "264px" :opacity "0.5"} :src "https://raw.githubusercontent.com/metosin/malli/master/docs/img/malli.png"}]]]]])

(defn create-or-update-modal! [content]
  (if-let [existing-modal (find-existing-modal)]
    (do
      (render-hiccup! existing-modal (modal-hiccup content))
      existing-modal)

    (hiccup-to-dom (modal-hiccup content))))

(defn show-modal [modal]
  (let [modal-style (.-style modal)]
    (set! (.-display modal-style) "block")))

(defn get-shadow-root-host []
  (if-let [parent (gdom/getElement modal-container-dom-node)]
    parent
    (let [parent (gdom/createDom "div" #js{:id modal-container-dom-node})]
      (.appendChild (.-body js/document) parent)
      parent)))

(defn show-dom-message! [dom-message]
  (let [modal (create-or-update-modal! dom-message)]
    (when-not (find-existing-modal)
      (let [parent (get-shadow-root-host)
            shadow-root (.attachShadow parent #js{:mode "open"})]
        (.appendChild shadow-root modal)
        (reset! shadow-root_ shadow-root)))
    (show-modal modal)))

(defn ^:dev/before-load before-load []
  (when-let [modal (find-existing-modal)]
    (hide-modal modal)))

(def -dark-colors
  {:title 45
   :title-dark 32
   :text 253
   :link 255
   :string 180
   :constant 149
   :type 123
   :error 196})

(def colors -dark-colors)

(defn -color [color body]
  (let [color (get colors color (:error colors))]
    [:span {:style {:color color}} body]))

(defn -visit [args]
  (cond
    (nil? args)
    [:span (-color :text "nil")]

    :else
    [:span (-color :text (pr-str args))]))

;(defrecord EdnPrinter [symbols print-meta print-length print-level unknown]
;
;  fipp.visit/IVisitor
;
;  (visit-unknown [this x]
;    (or (and unknown (try (some->> (unknown x) (fipp.visit/visit this))
;                          (catch #?(:clj Exception, :cljs js/Error) _)))
;      (fipp.visit/visit this (fipp.ednize/edn x))))
;
;  (visit-nil [this]
;    (-color :text "nil" this))
;
;  (visit-boolean [this x]
;    (-color :text (str x) this))
;
;  (visit-string [this x]
;    (-color :string (pr-str x) this))
;
;  (visit-character [this x]
;    (-color :text (pr-str x) this))
;
;  (visit-symbol [this x]
;    (-color :text (str x) this))
;
;  (visit-keyword [this x]
;    (-color :constant (pr-str x) this))
;
;  (visit-number [this x]
;    (-color :text (pr-str x) this))
;
;  (visit-seq [this x]
;    (if-let [pretty (symbols (first x))]
;      (pretty this x)
;      (fipp.edn/pretty-coll this (-color :text "(" this) x :line (-color :text ")" this) fipp.visit/visit)))
;
;  (visit-vector [this x]
;    (fipp.edn/pretty-coll this (-color :text "[" this) x :line (-color :text "]" this) fipp.visit/visit))
;
;  (visit-map [this x]
;    (let [xs (sort-by identity (fn [a b]
;                                 (try
;                                   (arrangement.core/rank (first a) (first b))
;                                   (catch #?(:clj Exception :cljs :default) _ -1))) x)]
;      (fipp.edn/pretty-coll this (-color :text "{" this) xs [:span (-color :text "," this) :line] (-color :text "}" this)
;        (fn [printer [k v]]
;          [:span (fipp.visit/visit printer k) " " (fipp.visit/visit printer v)]))))
;
;  (visit-set [this x]
;    (let [xs (sort-by identity (fn [a b] (arrangement.core/rank a b)) (seq x))]
;      (fipp.edn/pretty-coll this "#{" xs :line "}" fipp.visit/visit)))
;
;  (visit-tagged [this {:keys [tag form]}]
;    (let [object? (= 'object tag)
;          tag-f (if (map? form) #(-color :type % this) identity)]
;      [:group "#" (tag-f (pr-str tag))
;       (when (or (and print-meta (meta form)) (not (coll? form))) " ")
;       (if object?
;         [:group [:align "[" (fipp.visit/visit this (last form))] "]"]
;         (fipp.visit/visit this form))]))
;
;  (visit-meta [this m x]
;    (if print-meta
;      [:align [:span "^" (fipp.visit/visit this m)] :line (fipp.visit/visit* this x)]
;      (fipp.visit/visit* this x)))
;
;  (visit-var [_ x]
;    [:text (str x)])
;
;  (visit-pattern [_ x]
;    [:text (pr-str x)])
;
;  (visit-record [this x]
;    (fipp.visit/visit this (fipp.ednize/record->tagged x))))



(defmulti -format (fn [err-type data printer] err-type) :default ::default)

(defmethod -format ::default [e data printer]
  [:div {:className "malli-error"}
   [:h1 "Unknown Error"]
   [:div
    [:p "Type:" (type e)]
    [:p "Message:" (ex-message e)]
    (when-let [data (ex-data e)]
      [:p "Ex-data:" (-visit data)])
    ]])

(def label-styles
  #js{:padding "10px 0px"
      :font-size "1.2rem"
      :font-weight "700"})

(def label-attrs {:style label-styles})

(defmethod -format ::m/invalid-input [_ {:keys [args input fn-name]} printer]
  [:div {:className "malli-error"}
   [:h1 "Invalid Function Input"]
   [:div
    [:p label-attrs "Invalid function arguments"]
    (-visit args)
    (when fn-name
      [:div
       [:p label-attrs "Function Var "] [:pre {:className "malli-error-fn-name"} fn-name]])

    [:p label-attrs "Input Schema"]
    [:pre (pr-str (m/form input))]

    [:p label-attrs "Errors"]
    [:pre
     (with-out-str
       (pprint
         (me/with-error-messages (m/explain input args))
         ;(pretty/-explain input args printer)
         ))]
    [:p {:style {:padding "20px 0"}} "More information "
     [:a {:href "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas"
          :style {:color "#59a1df"}
          :ref "nooper" :target "_blank"} "function schema docs"]]]])

(defmethod -format ::m/invalid-output [_ {:keys [value args output fn-name]} printer]
  [:div {:className "malli-error"}
   [:h1 "Invalid Function Output"]
   [:div
    [:p label-attrs "Invalid function return value"]
    (-visit value)
    (when fn-name
      [:div
       [:p label-attrs "Function Var "] [:pre {:className "malli-error-fn-name"} fn-name]])

    [:p label-attrs "Function arguments"]
    [:pre (-visit args)]

    [:p label-attrs "Output Schema"]
    [:pre (pr-str (m/form output))]

    [:p label-attrs "Errors"]
    [:pre
     (with-out-str
       (pprint (me/with-error-messages (m/explain output value))))]
    [:p {:style {:padding "20px 0"}} "More information "
     [:a {:href "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas"
          :style {:color "#59a1df"}
          :ref "nooper" :target "_blank"} "function schema docs"]]]])

(defn dom-reporter
  []
  (let [printer (pretty/-printer)
        report (pretty/reporter printer)]
    (fn [type data]
      (let [message (with-out-str (report type data))
            dom-message (-format type data printer)]
        (def err-type type)
        (def data' data)
        (def dom' dom-message)
        (.log js/console "ERROR type: " (pr-str type) ", data: " (pr-str data))
        (.log js/console "message: " message)
        (show-dom-message! dom-message)))))

(defn dom-reporter-with-thrower
  []
  (let [thrower (malli.dev.pretty/thrower)
        dom-reporter' (dom-reporter)]
    (fn [type data]
      (dom-reporter' type data)
      (thrower type data))))
