(in-package #:cl-configurator)
(defun read-configuration-file (path)
  (with-open-file (file path)
    (read file)))


(defparameter *default-depth* 2)
(defparameter *default-ignore* '(0));;ignore :configuration
(defparameter *default-all-parents* nil)
(defparameter *default-from-start* nil)
(defparameter *val-hash* (make-hash-table))
(defparameter *allow-all* nil)


(defparameter *debug* nil)

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


(defun remove-list-items-from-list (items list)
  "removes the contents of items from list"
  (let ((new-list list))
    (dotimes (l (length items) new-list)
      (setf new-list (remove (nth l items) new-list :test #'equal)))))
(defun collect-positions-in-list (list list-of-positions)
  "Collects the values from list that are at the positions listed in list-of-positions. starts from 0. Will ignore values that are higher than (length list)"
  (let ((positions))
    (dolist (item list-of-positions positions)
      (if (numberp item)
	  (when (< item (length list))
	    (push (elt list item) positions))
	  (error "item is not a number")))
    (reverse positions)))

(defun combine-list-into-keyword (parents depth-of-combination &key (from-start nil)
								 (ignore-positions '())
								 (all-parents nil))
  "Combines the name of the parents with the parent names upto depth-of-combination, eg 
the node-name 'charles' with the node-parents being (sex height race name) and depth 2 would
would become ':race-name-charles', but 4 would be ':sex-height-race-name-charles',
 depth is measured from right to left, so most recent parent first. If all-parents is set to t, 
then all the parents with the exception of the positions listed in ignore-positions will be used"
  (when (listp ignore-positions)
    (let ((items-to-remove (collect-positions-in-list parents ignore-positions)))
      (setf parents (remove-list-items-from-list items-to-remove parents))))
  (when all-parents
    (setf depth-of-combination (length parents)))
  (when (< depth-of-combination 1)
    (setf depth-of-combination 1))
  (when (< (length parents) depth-of-combination)
    (setf depth-of-combination (length parents)))
  (if from-start
      (intern (format nil "~{~a~^-~}" (subseq parents 0 depth-of-combination))
	      "KEYWORD")
      (intern (format nil "~{~a~^-~}" (reverse (subseq (reverse parents) 0 depth-of-combination)))
	      "KEYWORD")))

 







(defgeneric make-entry (name value)
  (:documentation "Takes in a name and a value and creates a variable by that name with that data"))
 
(defmethod make-entry :before (name value)
  (format *standard-output* "Name: ~S~%Value: ~S~%"
	  name value))

(defmethod make-entry (name value)
  (if *allow-all*
      (setf (gethash name *val-hash*) value)
      (unexpected-type-error value)))

(defmethod make-entry (name (value character))
  (setf (gethash name *val-hash*) value))
(defmethod make-entry (name (value number))
  (setf (gethash name *val-hash*) value))
(defmethod make-entry (name (value sequence))
  (setf (gethash name *val-hash*) value))
(defmethod make-entry (name (value pathname))
  (setf (gethash name *val-hash*) value))

(defun leaf-to-entry (leaf depth-of-combination  &key (from-start nil)
						   (ignore-positions '())
						   (all-parents nil))
  (with-accessors ((name node-name)
		   (parents node-parents)
		   (value node-value))
      leaf
    (let* ((parent-and-name (append parents (list name)))
	   (new-name (combine-list-into-keyword parent-and-name depth-of-combination
						:from-start from-start
						:ignore-positions ignore-positions
						:all-parents all-parents)))
      (make-entry new-name value))))



(defgeneric access (key)
  (:documentation "gets the value of key"))
(defmethod access (key)
  (gethash key *val-hash*))

(defgeneric set-access (key value)
  (:documentation "Sets the value of key to value"))
(defmethod set-access (key value)
  (setf (gethash key *val-hash*) value))

(defun leaves-to-hashes (list-of-leaves depth-of-combination &key (from-start nil)
							       (ignore-positions '())
							       (all-parents nil))
  "Takes in a list of leaves which is normally generated by config-to-objects, and then generates 
the keywords used to access their values with (access <keyword>)"
  (mapcar (lambda (leaf)
	    (leaf-to-entry leaf depth-of-combination :from-start from-start
						     :ignore-positions ignore-positions
						     :all-parents all-parents))
	  list-of-leaves))

(defun import-configuration (path &key (from-start *default-from-start*)
				    (depth-of-combination *default-depth*)
				    (ignore-positions *default-ignore*)
				    (all-parents *default-all-parents*))
  "Imports the configuration, converts it to objects and then puts the leaves into a hashtable,
generating the keyword accessors based primarily on the value of depth-of-combination and 
ignore-positions which is a list, but if you change all-parents to t it'll add all parents
except those listed in ignore-positions"
  (multiple-value-bind (tree branches leaves)
      (config-to-objects (read-configuration-file path))
    (declare (ignore branches));;currently ignores tree and branches
    (leaves-to-hashes leaves  depth-of-combination :from-start from-start
						   :ignore-positions ignore-positions
						   :all-parents all-parents)
    tree))
(defparameter *root-node* ())
(defun modify-value-in-root (root-node leaf-name value)
  "Modifies the value of leaf in root-node and returns the modified tree structure"
  (labels ((rec (node)
	     ;(print node)
	     (cond ((null node)
		    nil)
		   ((listp node)
		    (rec (first node))
		    (rec (rest node)))
		   ((branch-p node)
		    (rec (n-children node)))
		   ((leaf-p node)
		    (when (equal leaf-name (n-name node))
		      (setf (n-value node) value)
		      t))
		   (t nil))))
    (rec (n-children root-node))
    root-node))
(defun append-to-list (list item)
  "appends anything to a list, unlike append which only appends other lists"
  (if (listp item)
      (append list item)
      (append list (list item))))
(defun add-leaf-in-root (root-node branch-name leaf-name leaf-value)
  "Creates a new leaf and adds it to the children named by branch-name. This will only work
when root-node is made of objects not lists, as lists do not keep track of their parents"
  (unless (keywordp leaf-name)
    (unexpected-type-error leaf-name))
  (labels ((rec (node)
	     (cond ((null node)
		    nil)
		   ((listp node)
		    (rec (first node))
		    (rec (rest node)))
		   ((branch-p node)
		    (when (equal (n-name node) branch-name)
		      (let* ((children (n-children node))
			     (parents (n-parents node))
			     (leaf (make-leaf leaf-name parents leaf-value)))
			(setf (n-children node) 
			      (append-to-list children leaf))))
		    (rec (n-children node)))
		   ((leaf-p node)
		    nil)
		   (t nil))))
    (rec (n-children root-node))
    root-node))

(defun add-new-branch-to-root (root-node parent branch-name)
  "Creates a new branch by name branch-name and adds it to the parent branch"
  (unless (keywordp branch-name)
    (unexpected-type-error branch-name))
  (labels ((rec (node)
	     (cond ((null node)
		    nil)
		   ((listp node)
		    (rec (first node))
		    (rec (rest node)))
		   ((branch-p node)
		    (when (equal (n-name node) parent)
		      (let* ((children (n-children node))
			     (parents (n-parents node))
			     (branch (make-branch branch-name parents nil)))
			(setf (n-children node) 
			      (append-to-list children branch))))
		    (rec (n-children node)))
		   ((leaf-p node)
		    nil)
		   (t nil))))
    (rec (n-children root-node))
    root-node)))
