(ns markdownify.edit-test
  (:require [cljs.test :refer (deftest testing is)]
            [markdownify.edit :refer (current-md slice b-i-next-state-args)]))

(deftest current-md-test
  (is (= nil
         (current-md "this is some text" 6 8)))
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

(deftest b-i-next-state-args-test
  (testing "can generate markdown"
    (let [{:keys [replace]} (b-i-next-state-args [nil :italic] "italic" [0 6])
          [value start end] replace]
      (is (= "*italic*"
             (replace-text "italic" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [nil :bold] "bold" [0 4])
          [value start end] replace]
      (is (= "**bold**"
             (replace-text "bold" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [:italic :bold] "bold-italic" [1 12])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "*bold-italic*" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [:bold :italic] "bold-italic" [2 13])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "**bold-italic**" value start end)))))

  (testing "can remove markdown"
    (let [{:keys [replace]} (b-i-next-state-args [:italic :italic] "italic" [1 7])
          [value start end] replace]
      (is (= "italic"
             (replace-text "*italic*" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [:bold :bold] "bold" [2 6])
          [value start end] replace]
      (is (= "bold"
             (replace-text "**bold**" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [:bold-italic :italic] "bold-italic" [3 14])
          [value start end] replace]
      (is (= "**bold-italic**"
             (replace-text "***bold-italic***" value start end))))

    (let [{:keys [replace]} (b-i-next-state-args [:bold-italic :bold] "bold-italic" [3 14])
          [value start end] replace]
      (is (= "*bold-italic*"
             (replace-text "***bold-italic***" value start end))))))
