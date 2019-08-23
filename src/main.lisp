(in-package #:cl-configurator)
(defparameter *example* (cxml:parse-file "test/configuration.xml" (cxml-dom:make-dom-builder)))
