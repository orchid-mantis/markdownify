(ns markdownify.main
  (:require [reagent.core :as reagent]))

(defn app []
  [:div
   [:h1 "Hello world!"]])
   

(defn mount! []
  (reagent/render [app]
                  (.getElementById js/document "app")))

(defn main! []
  (mount!))

(defn reload! []
  (mount!))
