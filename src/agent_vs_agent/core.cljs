(ns ^:figwheel-always agent-vs-agent.core
    (:require [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [secretary.core :as secretary :refer-macros [defroute]]
              [goog.events :as events]
              [goog.history.EventType :as EventType])
    (:import goog.History))

(enable-console-print!)

(print "HHHHH?")
(defonce app-state (atom {:text "Hello world!"}))

(defroute home-path "/" []
  (om/root
   (fn [data owner]
     (reify om/IRender
       (render [_]
         (dom/h1 nil "HEYYYY222"))))
   app-state
   {:target (. js/document (getElementById "app"))}))

(defn main []
  (secretary/set-config! :prefix "#")
  (let [h (History.)]
    (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
    (doto h (.setEnabled true))))

(main)
(print "HI")
