(in-package #:cl-configurator)

(defparameter *configuration-function-names* ())
(defun read-configuration-file (path)
  (with-open-file (file path)
    (read file)))


;;^ is
;; (:CONFIGURATION (:DIRECTORY (:ROOT "/home/bob/documents/website/bob/"))
;;  (:MISC (:USERNAME-LENGTH 4) (:TRY-AGAIN-TIME 3))
;;  (:DATABASE (:NAME "bobs site") (:USER "bob") (:ADDRESS "localhost")
;;   (:PASSWORD ""))
;;  (:HTML (:TITLE "im a title") (:STANDARD-IMAGE "/images/bobbing.gif"))
;;  (:DATA (:FILENAME "data"))
;;  (:PASSWORDS (:LOGIN "bigyeetus3") (:TOTALS "yeetusmcbeetus2")))

(defun combine-group-and-name (group name)
  (format nil "~A-~A" group name))

(defparameter *val-hash* (make-hash-table))
(defparameter *allow-all* nil)

(defun config-to-functions (config)
  (let ((lst (rest config)));;remove :configuration
    (mapcar (lambda (list)
	      (let ((group (first list)))
		(mapcar (lambda (entry)
			  (let ((name (intern (combine-group-and-name group (first entry))
					      "KEYWORD")))
			    (make-function name (first (rest entry)))))
			    
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
(defmethod (setf access) (key newvalue)
  (setf (gethash key *val-hash*) nil))

(defgeneric set-access (key value)
  (:documentation "Sets the value of key to value"))
(defmethod set-access (key value)
  (setf (gethash key *val-hash*) value))
