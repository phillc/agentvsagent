(ns ava.core
  (:require [om.core :as om]
            [om.dom :as dom])
)
(enable-console-print!)

(println "HI")
(println (+ 5 3 3))

(defonce app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

(om/root
 (fn [data owner]
   (om/component
    (apply dom/ul #js {:className "animals"}
           (map (fn [text] (dom/li nil text)) (:list data)))))
 app-state
 {:target (. js/document (getElementById "hearts-app"))})
