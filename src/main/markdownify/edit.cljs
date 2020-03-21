(ns markdownify.edit
  (:require [clojure.set :as s]))

(defn selected-range [el]
  {:start (.-selectionStart el)
   :end (.-selectionEnd el)})

(defn slice [str start end]
  (. str (slice start end)))

; https://stackoverflow.com/questions/3964710/replacing-selected-text-in-the-textarea
(defn replace-text [el text start end]
  (set! (.-value el) (str (. (.-value el) (slice 0 start))
                          text
                          (. (.-value el) (slice end)))))

(defn re-select [el start end]
  (-> el .focus)
  (-> el (.setSelectionRange start end)))

(def markdown {:italic      {:gen-md #(str "*" % "*")     :offset 1}
               :bold        {:gen-md #(str "**" % "**")   :offset 2}
               :bold-italic {:gen-md #(str "***" % "***") :offset 3}
               :heading     {:gen-md #(str "#" %)         :offset 1}})

(defn map-values [f map]
  (into {} (for [[k v] map]
             [k (f v)])))

(defn current-md [val start end]
  (let [selected (slice val start end)
        slice-fn (fn [{:keys [gen-md offset]}]
                   (let [slice (slice val (- start offset) (+ end offset))
                         new-text (gen-md selected)]
                     [offset (= slice new-text)]))]
    (->> (map-values slice-fn markdown)
         (filter (fn [[_ [_ bool]]] (true? bool)))
         (map-values first)
         (sort-by second)
         (last)
         (first))))

(defn md-edit [text-state req-md]
  (let [el (.getElementById js/document "markdown-textarea")
        {:keys [start end]} (selected-range el)
        selected (slice (.-value el) start end)
        val (when (seq selected) (. selected trim))
        current-md (current-md (.-value el) start end)]
    (when (seq val)
      (let [{gen-curr :gen-md curr-offset :offset} (get markdown current-md)
            {:keys [gen-md offset]} (get markdown req-md)
            select-offset #(/ (- (count %1) (count %2)) 2)]
        (cond
          (nil? current-md)
          (do (replace-text el (gen-md val) start end)
              (re-select    el              (+ start offset) (+ end offset)))

          (= current-md req-md)
          (do (replace-text el selected  (- start offset) (+ end offset))
              (re-select    el           (- start offset) (- end offset)))

          (= #{:italic :bold} (set [current-md req-md]))
          (let [{gen-bi :gen-md} (get markdown :bold-italic)
                offset (select-offset (gen-bi val) (gen-curr val))]
            (replace-text el (gen-bi val) (- start curr-offset) (+ end curr-offset))
            (re-select    el              (+ start offset)      (+ end offset)))

          (= :bold-italic current-md)
          (let [other (first (s/difference #{:italic :bold} #{req-md}))
                {gen-other :gen-md} (get markdown other)
                offset (select-offset (gen-curr val) (gen-other val))]
            (replace-text el (gen-other val) (- start curr-offset) (+ end curr-offset))
            (re-select    el                 (- start offset)      (- end offset))))
        (reset! text-state {:format :md
                            :value (.-value el)})))))

(defn selected->italic [text-state]
  (md-edit text-state :italic))

(defn selected->bold [text-state]
  (md-edit text-state :bold))

(defn selected->header [text-state]
  (md-edit text-state :heading))
