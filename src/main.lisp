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

  (let* ((nparents (if (listp ignore-positions)
		       (let ((items-to-remove (collect-positions-in-list parents ignore-positions)))
			 (remove-list-items-from-list items-to-remove parents))
		       parents))
	(len-par (length nparents))
	(d-o-p (cond (all-parents
		      len-par)
		     ((< depth-of-combination 1)
		      1)
		     ((< len-par depth-of-combination)
		      len-par)
		     (t depth-of-combination))))
    (intern (format nil "~{~a~^-~}" (if from-start
					(subseq nparents 0 d-o-p)
					(reverse (subseq (reverse nparents) 0 d-o-p))))
	    "KEYWORD")))

;; (defun combine-list-into-keyword (parents depth-of-combination &key (from-start nil)
;; 								 (ignore-positions '())
;; 								 (all-parents nil))
;;   "Combines the name of the parents with the parent names upto depth-of-combination, eg 
;; the node-name 'charles' with the node-parents being (sex height race name) and depth 2 would
;; would become ':race-name-charles', but 4 would be ':sex-height-race-name-charles',
;;  depth is measured from right to left, so most recent parent first. If all-parents is set to t, 
;; then all the parents with the exception of the positions listed in ignore-positions will be used"
;;   (when (listp ignore-positions)
;;     (let ((items-to-remove (collect-positions-in-list parents ignore-positions)))
;;       (setf parents (remove-list-items-from-list items-to-remove parents))))
;;   (when all-parents
;;     (setf depth-of-combination (length parents)))
;;   (when (< depth-of-combination 1)
;;     (setf depth-of-combination 1))
;;   (when (< (length parents) depth-of-combination)
;;     (setf depth-of-combination (length parents)))
;;   (if from-start
;;       (intern (format nil "~{~a~^-~}" (subseq parents 0 depth-of-combination))
;; 	      "KEYWORD")
;;       (intern (format nil "~{~a~^-~}" (reverse (subseq (reverse parents) 0 depth-of-combination)))
;; 	      "KEYWORD")))

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

(defmethod (setf access) (key value)
  (setf (gethash key *val-hash*) value))
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


