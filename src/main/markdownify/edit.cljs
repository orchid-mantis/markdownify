(ns markdownify.edit)

(defn selected-text [el]
  (let [start (.-selectionStart el)
        end (.-selectionEnd el)]
    (. (.-value el) (slice start end))))

; https://stackoverflow.com/questions/3964710/replacing-selected-text-in-the-textarea
(defn replace-selected-text [el text]
  (let [start (.-selectionStart el)
        end (.-selectionEnd el)]
    (set! (.-value el) (str (. (.-value el) (slice 0 start))
                            text
                            (. (.-value el) (slice end))))))

(defn md-edit [text-state edit-fn]
  (let [el (.getElementById js/document "markdown-textarea")
        value (selected-text el)
        text (when (seq value) (. value trim))]
    (when (seq text)
      (replace-selected-text el (edit-fn text))
      (reset! text-state {:format :md
                          :value (.-value el)}))))

(defn selected->italic [text-state]
  (md-edit text-state #(str "*" % "*")))

(defn selected->bold [text-state]
  (md-edit text-state #(str "**" % "**")))
