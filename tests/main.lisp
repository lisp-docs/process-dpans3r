(defpackage process-dpans3r/tests/main
  (:use :cl
        :process-dpans3r
        :rove))
(in-package :process-dpans3r/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :process-dpans3r)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
