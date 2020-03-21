(ns markdownify.edit
  (:require [clojure.set :as s]))

(defn selected-range [el]
  {:start (.-selectionStart el)
   :end (.-selectionEnd el)})

(defn slice
  ([str start end]
   (. str (slice start end)))
  ([str end]
   (. str (slice end))))

; https://stackoverflow.com/questions/3964710/replacing-selected-text-in-the-textarea
(defn replace-text [el text start end]
  (set! (.-value el) (str (. (.-value el) (slice 0 start))
                          text
                          (. (.-value el) (slice end)))))

(defn re-select [el start end]
  (-> el .focus)
  (-> el (.setSelectionRange start end)))

(def markdown {; heading
               :heading     {:gen-md #(str "#" % " ")     :offset 1}
               ; bold-italic
               :italic      {:gen-md #(str "*" % "*")     :offset 1}
               :bold        {:gen-md #(str "**" % "**")   :offset 2}
               :bold-italic {:gen-md #(str "***" % "***") :offset 3}})

(defn heading-transitions [current-md req-md]
  (case [current-md req-md]
    [nil :heading] :->markdown
    [:heading :heading] :->plain-text
    nil))

(defn b-i-transitions [current-md req-md]
  (case [current-md req-md]
    [nil :italic] :->markdown
    [nil :bold] :->markdown
    [:italic :italic] :->plain-text
    [:bold :bold] :->plain-text
    [:italic :bold] :->bold-italic
    [:bold :italic] :->bold-italic
    [:bold-italic :italic] :->keep-one-remove-other
    [:bold-italic :bold] :->keep-one-remove-other
    nil))

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

(defn heading-state-args [[current-md req-md] val [start end]]
  (let [{:keys [gen-md offset]} (get markdown req-md)]
    (case (heading-transitions current-md req-md)
      :->markdown
      {:replace [(gen-md val) start end] :select [(+ start offset) (+ end offset)]}

      :->plain-text
      {:replace [val (- start offset) (+ end offset)] :select [(- start offset) (- end offset)]}

      nil)))

(defn b-i-state-args [[current-md req-md] val [start end]]
  (let [{gen-curr :gen-md offset-curr :offset} (get markdown current-md)
        {:keys [gen-md offset]} (get markdown req-md)
        select-offset #(/ (- (count %1) (count %2)) 2)]
    (case (b-i-transitions current-md req-md)
      :->markdown
      {:replace [(gen-md val) start end] :select [(+ start offset) (+ end offset)]}

      :->plain-text
      {:replace [val (- start offset) (+ end offset)] :select [(- start offset) (- end offset)]}

      :->bold-italic
      (let [{gen-md :gen-md} (get markdown :bold-italic)
            offset (select-offset (gen-md val) (gen-curr val))]
        {:replace [(gen-md val) (- start offset-curr) (+ end offset-curr)] :select [(+ start offset) (+ end offset)]})

      :->keep-one-remove-other
      (let [other (first (s/difference #{:italic :bold} #{req-md}))
            {gen-other :gen-md} (get markdown other)
            offset (select-offset (gen-curr val) (gen-other val))]
        {:replace [(gen-other val) (- start offset-curr) (+ end offset-curr)] :select [(- start offset) (- end offset)]})

      nil)))

(defn md-edit [text-state req-md]
  (let [el (.getElementById js/document "markdown-textarea")
        {:keys [start end]} (selected-range el)
        selected (slice (.-value el) start end)
        selected (when (seq selected) (. selected trim))
        current-md (current-md (.-value el) start end)]
    (when (seq selected)
      (let [result (->> [heading-state-args b-i-state-args]
                        (map (fn [f] (f [current-md req-md] selected [start end])))
                        (filter some?)
                        (first))
            {:keys [replace select]} result]
        (when result
          (let [[value start end] replace]
            (replace-text el value start end))

          (let [[start end] select]
            (re-select el start end))
          (reset! text-state {:format :md
                              :value (.-value el)}))))))

(defn selected->italic [text-state]
  (md-edit text-state :italic))

(defn selected->bold [text-state]
  (md-edit text-state :bold))

(defn selected->header [text-state]
  (md-edit text-state :heading))
