(in-package #:cl-configurator)
;;;;file containing class definitions and some related methods
(defclass name ()
  ((name
    :accessor node-name
    :initarg :name
    :initform nil)))
(defclass parents ()
  ((parents
    :accessor node-parents
    :initarg :parents
    :initform nil)))
(defclass children ()
  ((children
    :accessor node-children
    :initarg :children
    :initform nil)))
(defclass value()
  ((value
    :accessor node-value
    :initarg :value
    :initform nil)))

(defclass root (name children)
  ())
(defclass branch (name parents children)
  ())
(defclass leaf (name parents value)
  ())

(defun make-root (name children)
  (make-instance 'root :name name :children children)) 

(defmethod print-object ((object root) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~S Children: ~S"
	    (node-name object)
	    (length (node-children object)))))


(defun make-branch (name parents children)
  (make-instance 'branch :name name :children children :parents parents))

(defmethod print-object ((object branch) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~S Parents: ~S Children: ~S"
	    (node-name object)
	    (node-parents object)
	    (length (node-children object)))))

(defun make-leaf (name parents value)
  (make-instance 'leaf :name name :value value :parents parents))

(defmethod print-object ((object leaf) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Name: ~S Parents: ~S Value: ~S"
	    (node-name object)
	    (node-parents object)
	    (node-value object))))
