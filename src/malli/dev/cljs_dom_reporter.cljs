(ns malli.dev.cljs-dom-reporter
  (:require [malli.dev.pretty :as pretty]
            [goog.dom :as gdom]
            [goog.style :as style]))

(defn find-existing-modal []
  (gdom/getElementByClass "modal"))

(defn set-modal-style! [modal-node]
  (style/setStyle modal-node #js{:border "1px solid black"
                                 :top "10%"
                                 :left "20%"
                                 :boxShadow "4px 4px 4px grey"
                                 :borderRadius "4px"
                                 :padding "10px"
                                 :position "fixed"
                                 :color "#eee"
                                 :max-width "50rem"
                                 :background "rgb(79 55 86)"}))

(defn create-modal [content]
  (if-let [existing-modal (find-existing-modal)]
    (do
      (let [modal-content (gdom/getElementByClass "modal-content")]
        (set-modal-style! modal-content)
        (set! (.-innerHTML modal-content) content)
        existing-modal))
    (let [modal (gdom/createDom "div" #js {:class "modal"})
          modal-content (gdom/createDom "pre" #js {:class "modal-content"})
          modalClose (gdom/createDom "span" #js {:class "close"} "×CLOLSE")]
      (set-modal-style! modal-content)
      (set! (.-innerHTML modal-content) content)
      (gdom/appendChild modal modalClose)
      (gdom/appendChild modal modal-content)
      modal)))

(defn show-modal [modal]
  (let [modal-style (.-style modal)]
    (set! (.-display modal-style) "block")))

(defn hide-modal [modal]
  (let [modal-style (.-style modal)]
    (set! (.-display modal-style) "none")))

(defn show-message! [message]
  (let [body (gdom/getElementByTagNameAndClass "body")
        modal (create-modal message)]
    (when-not (find-existing-modal)
      (println "Appending modal: " modal)
      (gdom/appendChild body modal))
    (show-modal modal)))

(defn ^:dev/before-load before-load []
  (println "IN DOM REPORTER BEFORE LOAD")
  (if-let [modal (find-existing-modal)]
    (hide-modal modal)))

(defn dom-reporter
  []
  (let [report (pretty/reporter (pretty/-printer))]
    (fn [type data]
      (let [message (with-out-str (report type data))]
        (.log js/console "ERROR type: " (pr-str type) ", data: " (pr-str data))
        (.log js/console "message: " message)
        (show-message! message)))))
