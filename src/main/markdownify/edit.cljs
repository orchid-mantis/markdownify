(ns markdownify.edit)

(defn selected-range [el]
  {:start (.-selectionStart el)
   :end (.-selectionEnd el)})

(defn selected-text [el]
  (let [{:keys [start end]} (selected-range el)]
    (. (.-value el) (slice start end))))

; https://stackoverflow.com/questions/3964710/replacing-selected-text-in-the-textarea
(defn replace-selected-text [el text]
  (let [{:keys [start end]} (selected-range el)]
    (set! (.-value el) (str (. (.-value el) (slice 0 start))
                            text
                            (. (.-value el) (slice end))))))

(defn re-select [el text start]
  (let [end (+ start (count text))]
    (-> el .focus)
    (-> el (.setSelectionRange start end))))

(defn md-edit [text-state edit-fn]
  (let [el (.getElementById js/document "markdown-textarea")
        {:keys [start]} (selected-range el)
        value (selected-text el)
        text (when (seq value) (. value trim))]
    (when (seq text)
      (let [new-text (edit-fn text)]
        (replace-selected-text el new-text)
        (re-select el new-text start)
        (reset! text-state {:format :md
                            :value (.-value el)})))))

(defn selected->italic [text-state]
  (md-edit text-state #(str "*" % "*")))

(defn selected->bold [text-state]
  (md-edit text-state #(str "**" % "**")))
