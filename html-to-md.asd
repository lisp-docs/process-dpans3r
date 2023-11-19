(defsystem "html-to-md"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("uiop" "cl-ppcre" "alexandria")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "html-to-md/tests"))))

(defsystem "html-to-md/tests"
  :author ""
  :license ""
  :depends-on ("html-to-md"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for html-to-md"
  :perform (test-op (op c) (symbol-call :rove :run c)))
