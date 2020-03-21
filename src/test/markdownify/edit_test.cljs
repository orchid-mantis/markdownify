(ns markdownify.edit-test
  (:require [cljs.test :refer (deftest testing is)]
            [markdownify.edit :refer (current-md slice
                                                 heading-state-args
                                                 b-i-state-args)]))

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

(deftest heading-state-args-test
  (testing "can generate markdown"
    (let [{:keys [replace]} (heading-state-args [nil :heading] "heading" [0 7])
          [value start end] replace]
      (is (= "#heading " ; additional space is required (it helps with markdown remove)
             (replace-text "heading" value start end)))))

  (testing "can remove markdown"
    (let [{:keys [replace]} (heading-state-args [:heading :heading] "heading" [1 8])
          [value start end] replace]
      (is (= "heading"
             (replace-text "#heading" value start end))))
    
    (let [{:keys [replace]} (heading-state-args [:heading :heading] "h1" [1 3])
          [value start end] replace]
      (is (= "h1" ; missing space is expected 
             (replace-text "#h1 " value start end))))))

(deftest b-i-state-args-test
  (testing "can generate markdown"
    (let [{:keys [replace]} (b-i-state-args [nil :italic] "italic" [0 6])
          [value start end] replace]
      (is (= "*italic*"
             (replace-text "italic" value start end))))

    (let [{:keys [replace]} (b-i-state-args [nil :bold] "bold" [0 4])
          [value start end] replace]
      (is (= "**bold**"
             (replace-text "bold" value start end))))

    (let [{:keys [replace]} (b-i-state-args [:italic :bold] "bold-italic" [1 12])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "*bold-italic*" value start end))))

    (let [{:keys [replace]} (b-i-state-args [:bold :italic] "bold-italic" [2 13])
          [value start end] replace]
      (is (= "***bold-italic***"
             (replace-text "**bold-italic**" value start end)))))

  (testing "can remove markdown"
    (let [{:keys [replace]} (b-i-state-args [:italic :italic] "italic" [1 7])
          [value start end] replace]
      (is (= "italic"
             (replace-text "*italic*" value start end))))

    (let [{:keys [replace]} (b-i-state-args [:bold :bold] "bold" [2 6])
          [value start end] replace]
      (is (= "bold"
             (replace-text "**bold**" value start end))))

    (let [{:keys [replace]} (b-i-state-args [:bold-italic :italic] "bold-italic" [3 14])
          [value start end] replace]
      (is (= "**bold-italic**"
             (replace-text "***bold-italic***" value start end))))

    (let [{:keys [replace]} (b-i-state-args [:bold-italic :bold] "bold-italic" [3 14])
          [value start end] replace]
      (is (= "*bold-italic*"
             (replace-text "***bold-italic***" value start end))))))
