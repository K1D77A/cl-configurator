;;;;package definition
(in-package #:asdf-user)
(asdf:defsystem "cl-configurator"
  :description "xml configuration manager"
  :name "cl-configurator"
  :author "k1d77a"
  :version "0"
  :license "MIT"
  :depends-on (#:cxml)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "main")))
     
