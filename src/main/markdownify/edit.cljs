(ns markdownify.edit
  (:require [clojure.set :as s]))

(def markdown {; heading
               :h1          {:gen-md #(str "#" % " ")     :offset 1}
               :h2          {:gen-md #(str "##" % "  ")   :offset 2}
               ; bold-italic
               :italic      {:gen-md #(str "*" % "*")     :offset 1}
               :bold        {:gen-md #(str "**" % "**")   :offset 2}
               :bold-italic {:gen-md #(str "***" % "***") :offset 3}})

(defn state-transitions [current-md req-md]
  (case [current-md req-md]
    ; heading
    [nil :h1] :->h1
    [:h1 :h1] :->h2
    [:h2 :h1] :->h2-remove
    ; bold-italic
    [nil :italic] :->italic
    [nil :bold] :->bold
    [:italic :italic] :->italic-remove
    [:bold :bold] :->bold-remove
    [:italic :bold] :->bold-italic
    [:bold :italic] :->bold-italic
    [:bold-italic :italic] :->keep-bold-remove-italic
    [:bold-italic :bold] :->keep-italic-remove-bold
    nil))

(defn slice
  ([str start end]
   (. str (slice start end)))
  ([str end]
   (. str (slice end))))

(defn shift [direction start end offset]
  (case direction
    :left  [(- start offset) (- end offset)]
    :right [(+ start offset) (+ end offset)]))

(defn next-state-args [[current-md req-md next-state] [val start end]]
  (let [{curr-off :offset} (get markdown current-md)
        {:keys [gen-md offset]} (get markdown req-md)]
    (case next-state
      (:->h1 :->italic :->bold)
      {:replace [(gen-md val) start end] :select (shift :right start end offset)}

      :->h2
      (let [{gen-md :gen-md} (get markdown :h2)]
        {:replace [(gen-md val) (- start 1) (+ end 1)] :select (shift :right start end 1)})

      (:->h2-remove :->italic-remove :->bold-remove)
      {:replace [val (- start curr-off) (+ end curr-off)] :select (shift :left start end curr-off)}

      :->bold-italic
      (let [{:keys [gen-md offset]} (get markdown :bold-italic)]
        {:replace [(gen-md val) (- start curr-off) (+ end curr-off)] :select (shift :right start end (- offset curr-off))})

      (:->keep-bold-remove-italic :->keep-italic-remove-bold)
      (let [other (first (s/difference #{:italic :bold} #{req-md}))
            {:keys [gen-md offset]} (get markdown other)]
        {:replace [(gen-md val) (- start curr-off) (+ end curr-off)] :select (shift :left start end (- curr-off offset))}))))

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

(defn selection-range [el]
  {:start (.-selectionStart el)
   :end   (.-selectionEnd   el)})

; https://stackoverflow.com/questions/3964710/replacing-selected-text-in-the-textarea
(defn replace-text [el val start end]
  (set! (.-value el) (str (slice (.-value el) 0 start)
                          val
                          (slice (.-value el) end))))

(defn select-text [el start end]
  (-> el .focus)
  (-> el (.setSelectionRange start end)))

(defn count-space-indent [str]
  (count (take-while #{\space} str)))

(defn space-indent-counts [str]
  [(count-space-indent str) (count-space-indent (reverse str))])

(defn trim-range [str start end]
  (let [[c1 c2] (space-indent-counts str)]
    [(+ start c1) (- end c2)]))

(defn md-edit [text-state req-md]
  (let [el (.getElementById js/document "markdown-textarea")
        {:keys [start end]} (selection-range el)
        user-selection (slice (.-value el) start end)
        [start end] (trim-range user-selection start end)
        selected (when (seq user-selection) (. user-selection trim))
        current-md (current-md (.-value el) start end)
        next-state (state-transitions current-md req-md)]
    (if (and (seq selected) next-state)
      (let [{:keys [replace select]} (next-state-args [current-md req-md next-state] [selected start end])]
        (let [[value start end] replace]
          (replace-text el value start end))
        (let [[start end] select]
          (select-text el start end))
        (reset! text-state {:format :md
                            :value (.-value el)}))

      (select-text el start end))))

(defn selected->italic [text-state]
  (md-edit text-state :italic))

(defn selected->bold [text-state]
  (md-edit text-state :bold))

(defn selected->header [text-state]
  (md-edit text-state :h1))
