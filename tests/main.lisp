(defpackage html-to-md/tests/main
  (:use :cl
        :html-to-md
        :rove))
(in-package :html-to-md/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :html-to-md)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
