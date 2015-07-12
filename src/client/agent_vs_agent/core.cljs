(ns ^:figwheel-always agent-vs-agent.core
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [secretary.core :as secretary :refer-macros [defroute]]
             [goog.events :as events]
             [goog.history.EventType :as EventType])
    (:import goog.History))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))

(om/root
 (fn [data owner]
   (reify om/IRender
     (render [_]
       (dom/div nil
                (dom/h1 nil (:text data))
                (dom/h2 nil "Hi333")))))
 app-state
 {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )

(secretary/set-config! :prefix "#")
(let [h (History.)]
  (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))
