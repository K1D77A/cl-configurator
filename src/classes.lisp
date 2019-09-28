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
    :type list
    :initarg :parents
    :initform nil)))
(defclass children ()
  ((children
    :accessor node-children
    :type list
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


(defgeneric is-valid-node (object)
  (:documentation "makes sure that the objects in use are valid, ie lists have a keyword at the
start and objects have a name"))
(defmethod is-valid-node :before (object)
  (if *debug*
      (format *standard-output* "~S~%" object)))
(defmethod is-valid-node ((object list))
  (if (keywordp (first object))
      t
      (malformed-entry-error object)))
(defmethod is-valid-node ((object name))
  (if (node-name object)
      t
      (malformed-entry-error object)))
(defmethod is-valid-node (object)
  (malformed-entry-error object))

(defgeneric n-children (object)
  (:documentation "returns the children of object"))
(defmethod n-children :before (object)
  (is-valid-node object))
(defmethod n-children ((object list))
  (rest object)) 
(defmethod n-children ((object children))
  (node-children object))

(defgeneric (setf n-children) (children object)
  (:documentation "Adds a new child to a branch"))
(defmethod (setf n-children) :before (children object)
  (is-valid-node object))
(defmethod (setf n-children) (children (object children))
  (setf (node-children object) children))
(defmethod (setf n-children) (children (object list))
  (setf (rest object) children))



(defgeneric contains-children-p (object)
  (:documentation "Checks if object contains children"))
(defmethod contains-children-p :before (object)
  (is-valid-node object))
(defmethod contains-children-p ((object list))
  "Checks if the list contains children"
  (some (lambda (x);;this is also slow, o need to keep going
	  (not (null x)))
	(mapcar #'listp object)))
(defmethod contains-children-p ((object children))
  (if (node-children object)
      t
      nil))
(defgeneric root-p (object)
  (:documentation "Checks if an object is a root node"))
(defmethod root-p :before (object)
  (is-valid-node object))
(defmethod root-p ((object list))
  "You can't know if a list has parents so it is not possible to tell with a list"
  (contains-children-p object))
(defmethod root-p ((object parents))
  nil)
(defmethod root-p ((object root))
  t)
(defmethod root-p (object)
  nil)
    
(defgeneric branch-p (object)
  (:documentation "Checks if object is a branch"))
(defmethod branch-p :before (object)
  (is-valid-node object))
(defmethod branch-p ((object list))
  "Checks if a list is a branch, meaning it has children"
  (if (and (n-name object)
	   (contains-children-p object));;this is slow
      t
      nil))
(defmethod branch-p ((object branch))
  t)
(defmethod branch-p (object)
  nil)

(defgeneric leaf-p (object)
  (:documentation "Checks if object is a leaf, meaning it has no children"))
(defmethod leaf-p :before (object)
  (is-valid-node object))
(defmethod leaf-p ((object list))
  "Checks if list is a leaf, meaning it has no children"
  (and (n-name object)
       (not (contains-children-p object))))
(defmethod leaf-p ((object leaf))
  t)
(defmethod leaf-p (object)
  nil)

(defgeneric n-name (object)
  (:documentation "Returns the name of object"))
(defmethod n-name :before (object)
  (is-valid-node object))
(defmethod n-name ((object list))
  (first object))
(defmethod n-name ((object name))
  (node-name object))


(defgeneric (setf n-name) (new object))
(defmethod (setf n-name) :before (new object)
  (is-valid-node object))
(defmethod (setf n-name) (new (object list))
  (setf (first object) new))
(defmethod (setf n-name) (new (object value))
  (setf (node-name object) new))


(defgeneric n-value (object)
  (:documentation "Returns the name of object"))
(defmethod n-value :before (object)
  (is-valid-node object))
(defmethod n-value ((object list))
  (second object))
(defmethod n-value ((object name))
  (node-value object))

(defgeneric (setf n-value) (new object))
(defmethod (setf n-value) :before (new object)
  (is-valid-node object))
(defmethod (setf n-value) (new (object list))
  (setf (second object) new))
(defmethod (setf n-value) (new (object value))
  (setf (node-value object) new))

(defgeneric n-parents (object)
  (:documentation "Returns the parents of a node"))
(defmethod n-parents :before (object)
  (is-valid-node object))
(defmethod n-parents ((object name))
  (node-parents object))

(defgeneric (setf n-parents) (new object))
(defmethod (setf n-parents) :before (new object)
  (is-valid-node object))
(defmethod (setf n-parents) (new (object value))
  (setf (node-value object) new))

  
(defgeneric delete-child (object child)
  (:documentation "Deletes child from children"))
(defmethod delete-child ((object children) child)
  (let ((new-children (remove-if (lambda (entry)
				   (equal (n-name entry) child))
				 (n-children object))))
    (setf (n-children object) new-children)))
(defgeneric contains-child (object child)
  (:documentation "Checks if child is part of objects children"))
(defmethod contains-child ((object children) child)
  (let ((bool nil))
    (dolist (entry (n-children object) bool)
      (when (equal (n-name entry) child)
	(setf bool t)))))
  
				    
  
