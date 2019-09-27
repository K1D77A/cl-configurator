(in-package #:cl-configurator)
(defparameter *configuration-function-names* ())
(defun read-configuration-file (path)
  (with-open-file (file path)
    (read file)))


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

(defun contains-children-p (list)
  "Checks if the list contains children"
  (some (lambda (x);;this is also slow, no need to keep going
	  (not (null x)))
	(mapcar #'listp list)))

(defun collect-children (list)
  "Collects the children of list into a new list"
  (let ((children ()))
    (mapcar (lambda (x)
	      (when (listp x)
		(push x children)))
	    list)
    (reverse children)))
(defun branch-p (list)
  "Checks if a list is a branch, meaning it has children"
  (if (and (any-node-name list)
	   (collect-children list));;this is slow
      t
      nil))
(defun leaf-p (list)
  "Checks if list is a leaf, meaning it has no children"
  (and (any-node-name list)
       (not (contains-children-p list))))
(defun n-name (list)
  (first list))

(defun config-to-objects (config)
  "Converts the config file to objects of types branch and leaf and then returns a tree starting with
root, then a list of all the branches with their children as lists not objects and then a list of the leaves"
  (let ((branches ())
	(leaves ()))
    (labels ((rec (list parent)
	      ; (print (reverse parent))
	       (cond ((null list)
		      nil)
		     ((branch-p (first list))
		      (let* ((branch (first list))
			     (branch-obj
			       (make-branch (n-name branch)(reverse  parent) (rest branch))))
			(push branch-obj branches)
			(cons (make-branch (n-name branch) (reverse parent)
					   (rec (rest branch)
						(append (list (first branch))  parent)))
			      (rec (rest list) parent))))
		     ((leaf-p (first list))
		      (let* ((leaf (first list))
			    (leaf-obj
			      (make-leaf (n-name leaf) (reverse parent) (first (rest leaf)))))
			(push leaf-obj leaves)
			(cons (make-leaf (n-name leaf) (reverse parent) (first (rest leaf)))
			      (rec (rest list)  parent))))
		     (t nil))))
		     
      (values (make-root (first config)
			 (rec (rest config) (list (first config))))
	      branches
	      leaves))))

(defun combine-parents (node depth-of-combination)
  "Combines the name of the node with the parent names upto depth-of-combination, eg 
the node-name 'charles' with the node-parents being (sex height race name) and depth 2 would
would become ':race-name-charles', but 4 would be ':sex-height-race-name-charles', deptch is measured from right to left, so most recent parent first"
  ())
  
  


(defun combine-group-and-name (group name)
  (format nil "~A-~A" group name))

(defparameter *val-hash* (make-hash-table))
(defparameter *allow-all* nil)
(defun entry-name (entry)
  (first entry))
(defun entry-value (entry)
  (first (rest entry)))

(defun config-to-hash (config)
  (let ((lst (rest config)));;remove :configuration
    (mapcar (lambda (list)
	      (let ((group (first list)))
		(mapcar (lambda (entry)
			  (let ((name (intern
				       (combine-group-and-name group (entry-name entry))
				       "KEYWORD")))
			    (make-function name (entry-value entry))))
			(rest list))))
	    lst)))

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

(defgeneric make-function (name value)
  (:documentation "Takes in a name and a value and creates a variable by that name with that data"))

(defmethod make-function :before (name value)
  (format *standard-output* "Name: ~S~%Value: ~S~%"
	  name value))
(defmethod make-function (name value)
  (if *allow-all*
      (setf (gethash name *val-hash*) value)
      (unexpected-type-error value)))

(defmethod make-function (name (value string))
  (setf (gethash name *val-hash*) value))
(defmethod make-function (name (value integer))
  (setf (gethash name *val-hash*) value))
(defmethod make-function (name (value float))
  (setf (gethash name *val-hash*) value))

(defgeneric access (key)
  (:documentation "gets the value of key"))
(defmethod access (key)
  (gethash key *val-hash*))

(defgeneric set-access (key value)
  (:documentation "Sets the value of key to value"))
(defmethod set-access (key value)
  (setf (gethash key *val-hash*) value))
