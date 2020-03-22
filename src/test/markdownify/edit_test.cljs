(ns markdownify.edit-test
  (:require [cljs.test :refer (deftest testing is)]
            [markdownify.edit :refer (current-md slice next-state-args)]))

(deftest current-md-test
  (is (= nil
         (current-md "this is some text" 5 7)))
  (is (= :italic
         (current-md "this *is* some text" 6 8)))
  (is (= :bold
         (current-md "this **is** some text" 7 9)))
  (is (= :bold-italic
         (current-md "this ***is*** some text" 8 10)))
  (is (= :bold-italic
         (current-md "this ****is**** some text" 9 11))))

(defn replace-text [input-text value start end]
  (str (slice input-text 0 start)
       value
       (slice input-text end)))

(deftest next-state-args-test
  (testing "state transitions for heading"
    (let [{:keys [replace]} (next-state-args [nil :h1 :->h1] ["heading" 0 7])
          [value start end] replace]
      (is (= "#heading " ; additional space is required (it helps with markdown remove)
             (replace-text "heading" value start end))))

    (let [{:keys [replace]} (next-state-args [:h1 :h1 :->h2] ["heading" 1 8])
          [value start end] replace]
      (is (= "##heading  " ; additional spaces
             (replace-text "#heading" value start end))))

    (let [{:keys [replace]} (next-state-args [:h2 :h1 :->h2-remove] ["h1" 2 4])
          [value start end] replace]
      (is (= "h1"
             (replace-text "##h1" value start end))))

    (let [{:keys [replace]} (next-state-args [:h2 :h1 :->h2-remove] ["h1" 2 4])
          [value start end] replace]
      (is (= "h1" ; additional spaces are removed
             (replace-text "##h1  " value start end)))))

  (testing "state transitions for bold & italic"
    (let [{:keys [replace]} (next-state-args [nil :italic :->italic] ["italic" 0 6])
          [value start end] replace]
      (is (= "*italic*"
             (replace-text "italic" value start end))))

    (let [{:keys [replace]} (next-state-args [nil :bold :->bold] ["bold" 0 4])
          [value start end] replace]
      (is (= "**bold**"
             (replace-text "bold" value start end))))

    (let [{:keys [replace]} (next-state-args [:italic :bold :->bold-italic] ["bold-italic" 1 12])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "*bold-italic*" value start end))))

    (let [{:keys [replace]} (next-state-args [:bold :italic :->bold-italic] ["bold-italic" 2 13])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "**bold-italic**" value start end))))

    (let [{:keys [replace]} (next-state-args [:italic :italic :->italic-remove] ["italic" 1 7])
          [value start end] replace]
      (is (= "italic"
             (replace-text "*italic*" value start end))))

    (let [{:keys [replace]} (next-state-args [:bold :bold :->bold-remove] ["bold" 2 6])
          [value start end] replace]
      (is (= "bold"
             (replace-text "**bold**" value start end))))

    (let [{:keys [replace]} (next-state-args [:bold-italic :italic :->keep-bold-remove-italic] ["bold-italic" 3 14])
          [value start end] replace]
      (is (= "**bold-italic**"
             (replace-text "***bold-italic***" value start end))))

    (let [{:keys [replace]} (next-state-args [:bold-italic :bold :->keep-italic-remove-bold] ["bold-italic" 3 14])
          [value start end] replace]
      (is (= "*bold-italic*"
             (replace-text "***bold-italic***" value start end))))))
