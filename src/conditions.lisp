(in-package #:cl-configurator)
;;;;file containing the conditions that have been defined and are in use
(define-condition unexpected-type (error)
  ((type
    :initarg :type
    :accessor unexpected-type-type
    :initform nil
    :documentation "The value of (type-of val) that caused the error")
   (value
    :initarg :value
    :accessor unexpected-type-value
    :initform nil
    :documentation "The value of val")))

(defmethod print-object ((object unexpected-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Value: ~A~%Type of value: ~S"
	    (unexpected-type-value object)
	    (unexpected-type-type object))))
(defun unexpected-type-error (value)
  (error 'unexpected-type
	 :type (type-of value)
	 :value value))

(define-condition malformed-entry (error)
  ((entry
    :initarg :entry
    :accessor malformed-entry-entry
    :initform nil
    :documentation "The entry that is malformed")))

(defmethod print-object ((object malformed-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Object ~S is malformed.~%It be (keyword <value or children>) in list form"
	    (malformed-entry-entry object))))

(defun malformed-entry-error (value)
  (error 'malformed-entry
	 :entry value))
