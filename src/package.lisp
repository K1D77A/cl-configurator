(defpackage #:cl-configurator
  (:use #:CL)
  (:export "config-to-hash"
	   "access"
	   "set-access"
	   "import-configuration"
	   "*default-from-start*"
	   "*default-depth*"
	   "*default-ignore*"
	   "*default-all-parents*")
  (:nicknames "cl-con"))
(in-package #:cl-configurator)
