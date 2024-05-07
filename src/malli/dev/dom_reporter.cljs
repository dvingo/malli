(ns malli.dev.dom-reporter
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.dev.pretty :as pretty]
            [cljs.pprint :refer [pprint]]
            [goog.dom :as gdom]
            [goog.style :as style]))

(def shadow-host-dom-node-id "malli-instrument-modal-host")
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

(defonce shadow-root_ (atom nil))

(defn find-existing-modal []
  (when-let [root @shadow-root_]
    (.-firstChild root)))

(defn hide-modal [modal]
  (let [modal-style (.-style modal)]
    (set! (.-display modal-style) "none")))

(defn hide-current-modal []
  (hide-modal (find-existing-modal)))

(defn modal-hiccup [message]
  [:div ;; shadow-root containing element
   [:div {:className modal-container-class-name
          :style {:position "fixed"
                  :inset "0"
                  :background "var(--modal-backdrop-background)"}}
    [:div {:className "malli-instrument-modal-container"}
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
      [(if (string? message) :pre :div) {:className modal-content-class-name}
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
  (if-let [parent (gdom/getElement shadow-host-dom-node-id)]
    parent
    (let [parent (gdom/createDom "div" #js{:id shadow-host-dom-node-id})]
      (.appendChild (.-body js/document) parent)
      parent)))

(defn css-map->str [m]
  (str
    (reduce-kv (fn [acc k v]
                 (str acc (named->str k) ":"
                   (if (number? v) (str v "px") v) ";"))
      "{" m)
    "}"))

(defn css->str [css]
  (if (every? map? (vals css))
    (reduce-kv
      (fn [acc k v]
        (str acc (named->str k) (css-map->str v)))
      "" css)
    (css-map->str css)))

(def colors
  {:title "var(--title-color)"
   :title-dark "var(--title-dark-color)"
   :text "var(--text-color)"
   :link "var(--link-color)"
   :string "var(--string-color)"
   :constant "var(--constant-color)"
   :type "var(--type-color)"
   :error "var(--error-color)"})

(def dark-theme
  {(str "." modal-container-class-name)
   {:--title-color "rgb(100 100 100)"
    :--title-dark-color "rgb(100 100 100)"
    :--text-color "rgb(226 220 205)"
    :--link-color "rgb(100 100 100)"
    :--string-color "rgb(100 100 100)"
    :--constant-color "rgb(100 100 100)"
    :--type-color "rgb(100 100 100)"
    :--error-color "rgb(226 220 205)"
    :--modal-title-color "#d0d0d0"
    :--modal-close-icon-color "white"
    :--modal-content-color "rgb(235 238 245)"
    :--modal-container-shadow "rgb(27 27 30 / 94%) 4px 4px 20px 20px"
    :--modal-container-background "rgba(51, 51, 54, 0.94)"
    :--modal-link-color "#59a1df"
    :--modal-backdrop-background "rgba(10, 10, 10, 0.74)"}})

(def light-theme
  {(str "." modal-container-class-name)
   {:--title-color "rgb(100 100 100)"
    :--title-dark-color "rgb(100 100 100)"
    :--text-color "rgb(100 100 100)"
    :--link-color "rgb(100 100 100)"
    :--string-color "rgb(100 100 100)"
    :--constant-color "rgb(100 100 100)"
    :--type-color "rgb(100 100 100)"
    :--error-color "rgb(72 68 68)"
    :--modal-title-color "black"
    :--modal-close-icon-color "black"
    :--modal-content-color "hsl(0 61% 6% / 1)"
    :--modal-container-shadow "rgba(51, 51, 54, 0.94) 4px 4px 20px 20px"
    :--modal-container-background "white"
    :--modal-link-color "#59a1df"
    :--modal-backdrop-background "rgb(58 58 67 / 77%)"}})

(def modal-styles
  {".malli-error > h1"
   {:color "var(--modal-title-color)"}

   ".malli-instrument-modal-container"
   {:position "fixed"
    :top "10%"
    :left "50%"
    :transform "translate(-50%, 0)"
    :max-width "50rem"
    :box-shadow "var(--modal-container-shadow)"
    :padding "20px 40px 0"
    :border-radius "4px"
    :background-color "var(--modal-container-background)"}

   (str "." modal-content-class-name)
   {:border-radius "4px"
    :padding "10px 20px"
    :color "var(--modal-content-color)"}

   ".malli-instrument-modal-close"
   {:color "var(--modal-close-icon-color)"}})

(def themes {:light light-theme :dark dark-theme})

(defn make-css-str [theme]
  (str
    (css->str (if (map? theme) theme (get themes theme dark-theme)))
    (css->str modal-styles)))

(make-css-str :light)

(defn -color [color body]
  (let [color (get colors color (:error colors))]
    [:span {:style {:color color}} body]))

(defn make-stylesheet [theme]
  (doto (js/CSSStyleSheet.)
    (.replaceSync (make-css-str theme))))

(defn show-dom-message! [dom-message theme]
  (let [modal (create-or-update-modal! dom-message)]
    (when-not (find-existing-modal)
      (let [parent (get-shadow-root-host)
            sheet (make-stylesheet theme)
            shadow-root (.attachShadow parent #js{:mode "open"})]
        (set! (.-adoptedStyleSheets shadow-root) #js [sheet])
        (.appendChild shadow-root modal)
        (reset! shadow-root_ shadow-root)))
    (show-modal modal)))

(defn ^:dev/before-load before-load []
  (when-let [modal (find-existing-modal)]
    (hide-modal modal)))

(defn -visit [x]
  (cond
    (nil? x) [:span (-color :text "nil")]
    (boolean? x) [:span (-color :text (str x))]
    (string? x) [:span (-color :text (pr-str x))]
    (char? x) [:span (-color :text (pr-str x))]
    ;(symbol? x) (visit-symbol visitor x)
    ;(keyword? x) (visit-keyword visitor x)
    ;(number? x) (visit-number visitor x)
    ;(seq? x) (visit-seq visitor x)
    ;(vector? x) (visit-vector visitor x)
    ;(record? x) (visit-record visitor x)
    ;(map? x) (visit-map visitor x)
    ;(set? x) (visit-set visitor x)
    ;(tagged-literal? x) (visit-tagged visitor x)
    (var? x) [:span (-color :text (str x))]
    (regexp? x) [:span (-color :text (pr-str x))]
    :else (-color :text (pr-str x))))

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
      [:p "Ex-data:" (-visit data)])]])

(def label-styles
  #js{:padding "10px 0px"
      :font-size "1.2rem"
      :font-weight "700"})

(def label-attrs {:style label-styles})

(def docs-link
  [:a {:href "https://cljdoc.org/d/metosin/malli/CURRENT/doc/function-schemas"
       :style {:color "var(--modal-link-color)"}
       :ref "nooper" :target "_blank"} "function schema docs"])

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
         (update
           (me/with-error-messages (m/explain input args))
           :schema m/form)
         ;(pretty/-explain input args printer)
         ))]
    [:p {:style {:padding "20px 0"}} "More information " docs-link]]])

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
       (pprint
         (update
          (me/with-error-messages (m/explain output value))
           :schema m/form)))]
    [:p {:style {:padding "20px 0"}} "More information " docs-link]]])

(defn get-preferred-color-scheme []
  (if (.-matches (js/window.matchMedia "(prefers-color-scheme:light)"))
    :light :dark))

(defn dom-reporter
  "Takes an optional theme keyword (:light or :dark) or hashmap of css to override the theme."
  ([] (dom-reporter (get-preferred-color-scheme)))
  ([theme]
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
         (show-dom-message! dom-message theme))))))

(defn dom-reporter-with-thrower
  "Takes an optional theme keyword (:light or :dark) or hashmap of css to override the theme."
  ([] (dom-reporter-with-thrower (get-preferred-color-scheme)))
  ([theme]
   (let [thrower (malli.dev.pretty/thrower)
         dom-reporter' (dom-reporter theme)]
     (fn [type data]
       (dom-reporter' type data)
       (thrower type data)))))
