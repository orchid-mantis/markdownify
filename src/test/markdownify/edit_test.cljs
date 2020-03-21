(ns markdownify.edit-test
  (:require [cljs.test :refer (deftest is)]
            [markdownify.edit :refer (current-md)]))

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
