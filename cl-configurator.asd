;;;;package definition
(in-package #:asdf-user)
(asdf:defsystem "cl-configurator"
  :description "sexp configuration manager"
  :name "cl-configurator"
  :author "k1d77a"
  :version "1"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "classes")
	       (:file "conditions")
	       (:file "main")))
     
