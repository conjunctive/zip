(defsystem zip
  :author "Conjunctive"
  :maintainer "Conjunctive"
  :license "AGPL-3.0"
  :version "0.1.1"
  :homepage "https://github.com/conjunctive/zip"
  :source-control (:git "git@github.com:conjunctive/zip.git")
  :description "Zippers for Common Lisp"
  :depends-on ("alexandria"
               "arrows"
               "fset")
  :components ((:module "src"
                :serial t
                :components ((:file "zip"))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op zip-test))))
