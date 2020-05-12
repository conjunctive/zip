(defsystem zip-test
  :author "Conjunctive"
  :maintainer "Conjunctive"
  :license "AGPL-3.0"
  :version "0.1.1"
  :homepage "https://github.com/conjunctive/zip"
  :source-control (:git "git@github.com:conjunctive/zip.git")
  :description "Test system for Zip"
  :depends-on ("alexandria"
               "arrows"
               "fset"
               "prove"
               "zip")
  :components ((:module "t"
                :serial t
                :components
                ((:file "zip"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
