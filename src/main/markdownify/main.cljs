(ns markdownify.main
  (:require [reagent.core :as reagent]
            ["showdown" :as showdown]
            ["mousetrap" :as mousetrap]
            [markdownify.edit :as edit]))

(defonce flash-message (reagent/atom nil))
(defonce flash-timeout (reagent/atom nil))

(defn flash
  ([text]
   (flash text 3000))
  ([text ms]
   (js/clearTimeout @flash-timeout)
   (reset! flash-message text)
   (reset! flash-timeout
           (js/setTimeout #(reset! flash-message nil) ms))))

(defonce text-state (reagent/atom {:format :md
                                   :value ""}))

(defonce showdown-converter (showdown/Converter.))

(defn md->html [md]
  (.makeHtml showdown-converter md))

(defn html->md [html]
  (.makeMarkdown showdown-converter html))

(defn ->md [{:keys [format value]}]
  (case format
    :md value
    :html (html->md value)))

(defn ->html [{:keys [format value]}]
  (case format
    :md (md->html value)
    :html value))

; https://hackernoon.com/copying-text-to-clipboard-with-javascript-df4d4988697f
(defn copy-to-clipboard [s]
  (let [el (.createElement js/document "textarea")
        selected (when (pos? (-> js/document .getSelection .-rangeCount))
                   (-> js/document .getSelection (.getRangeAt 0)))]
    (set! (.-value el) s)
    (.setAttribute el "readonly" "")
    (set! (-> el .-style .-position) "absolute")
    (set! (-> el .-style .-left) "-9999px")
    (-> js/document .-body (.appendChild el))
    (.select el)
    (.execCommand js/document "copy")
    (-> js/document .-body (.removeChild el))
    (when selected
      (-> js/document .getSelection .removeAllRanges)
      (-> js/document .getSelection (.addRange selected)))))

(defn app []
  [:div {:style {:position :relative}}
   [:div.flash-message 
    {:style {:transform (if @flash-message
                          "scaleY(1)"
                          "scaleY(0)")
             :transition "transform 0.2s ease-out"}}
    @flash-message]
   
   [:h1 "Markdownify"]
   [:div
    {:style {:display :flex}}

    [:div
     {:style {:flex "1"}}
     [:div.header
      [:h2 "Markdown"]
      [:div.toolbar
       [:button
        {:on-click #(edit/selected->header text-state)
         :title "Heading - Alt + H"}
        "T"]
       [:button
        {:on-click #(edit/selected->bold text-state)
         :title "Bold - Alt + B"
         :style {:font-weight "bold"}}
        "B"]
       [:button
        {:on-click #(edit/selected->italic text-state)
         :title "Italic - Alt + I"
         :style {:font-style "italic"}}
        "I"]]]
     [:textarea#markdown-textarea.mousetrap
      {:on-change (fn [e]
                    (reset! text-state {:format :md
                                        :value (-> e .-target .-value)}))
       :value (->md @text-state)}]
     [:button.copy-button
      {:on-click (fn []
                   (copy-to-clipboard (->md @text-state))
                   (flash "Markdown copied to clipboard"))}
      "Copy Markdown"]]

    [:div.content
     [:div.header
      [:h2 "HTML"]]
     [:textarea
      {:on-change (fn [e]
                    (reset! text-state {:format :html
                                        :value (-> e .-target .-value)}))
       :value (->html @text-state)}]
     [:button.copy-button
      {:on-click (fn []
                   (copy-to-clipboard (->html @text-state))
                   (flash "HTML copied to clipboard"))
       }
      "Copy HTML"]]

    [:div.content
     [:div.header
      [:h2 "HTML Preview"]]
     [:div {:style {:height "500px"}
            :dangerouslySetInnerHTML {:__html (->html @text-state)}}]]]])

(defn init-key-bindings [text-state]
  (mousetrap/bind "alt+i" #(edit/selected->italic text-state))
  (mousetrap/bind "alt+b" #(edit/selected->bold text-state))
  (mousetrap/bind "alt+h" #(edit/selected->header text-state)))

(defn mount! []
  (reagent/render [app]
                  (.getElementById js/document "app"))
  (init-key-bindings text-state))

(defn main! []
  (mount!))

(defn reload! []
  (mount!))
