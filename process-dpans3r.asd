(defsystem "process-dpans3r"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("uiop" "cl-ppcre" "alexandria" "str")
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "code-blocks"))))
  :description ""
  :in-order-to ((test-op (test-op "process-dpans3r/tests"))))

(defsystem "process-dpans3r/tests"
  :author ""
  :license ""
  :depends-on ("process-dpans3r"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for process-dpans3r"
  :perform (test-op (op c) (symbol-call :rove :run c)))
