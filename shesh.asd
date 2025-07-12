;;;; shesh.asd
;;;; ASDF system definition for shesh

(defsystem "shesh"
           :description "A Common Lisp interface for interacting with shell processes using UIOP"
           :version "0.1.0"
           :author "Your Name"
           :license "MIT"
           :depends-on () ; UIOP is included with most modern CL implementations
           :serial t
           :components ((:file "shesh")))

;;; Optional: Define a test system
(defsystem "shesh/test"
           :description "Tests for shesh"
           :depends-on ("shesh")
           :serial t
           :components ((:file "test-shell")))
