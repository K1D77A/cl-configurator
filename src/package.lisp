(defpackage #:cl-configurator
  (:use #:CL)
  (:export "config-to-hash"
	   "access"
	   "set-access"
	   "read-configuration-file")
  (:nicknames "cl-con"))
(in-package #:cl-configurator)
