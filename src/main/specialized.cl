;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; specialized.cl --- Definition of specialized element nodes
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: specialized.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defclass specialized-element (element)
  ()
  (:documentation "Base class of all specialized element node classes."))

(defgeneric parse-attribute (element key value)
  (:method-combination or)
  (:documentation
   "Internal implementation of the attribute parsing process for
specialized node classes."))

(defgeneric unparse-attributes (element)
  (:method-combination nconc)
  (:documentation
   "Internal implementation of the attribute unparsing process for
specialized node classes."))

(defun process-attributes (element attributes)
  (loop for (key unparsed-value) on attributes by #'cddr
	do
	(multiple-value-bind (slot value)
	    (parse-attribute element key unparsed-value)
	  (if slot
	      (setf (slot-value element slot) value)
	      (cerror "Ignore" "Unparsed attribute ~A='~A'"
		      key unparsed-value)))))

(defmethod update-instance-for-different-class
    ((previous generic-element) (current specialized-element) &rest args)
  (declare (ignore args))
  (process-attributes current (slot-value previous 'attributes)))

(defmethod initialize-instance :after
    ((element specialized-element) &rest args &key gi attributes)
  (declare (ignore args gi))
  (process-attributes element attributes))

(defmethod reinitialize-instance :after
    ((element specialized-element) &rest args &key gi attributes)
  (declare (ignore args gi))
  (process-attributes element attributes))

(defmethod element-p ((element specialized-element) (gi symbol))
  (typep element gi))

(defmethod element-attributes ((element specialized-element))
  (unparse-attributes element))

(defun xml-class-namespace (symbol)
  (etypecase symbol
    (hash-table symbol)
    (symbol (or (get symbol 'xml-class-namespace)
		(setf (get symbol 'xml-class-namespace)
		      (make-hash-table :test #'equal))))))

(defun symbol-xml-class (symbol name)
  (gethash name (xml-class-namespace symbol) 'generic-element))

(defun (setf symbol-xml-class) (class symbol name)
  (setf (gethash name (xml-class-namespace symbol)) class))

(defmacro define-xml-element
    ((name gi &optional (ns 'xml-class-namespace)) supers slots &rest options)
  "Define a specialized XML element node class."
  (loop for (slot-name . slot-options) in slots
	for xml-gi = (getf slot-options :xml-gi (symbol-name name))
	for xml-reader = (getf slot-options :xml-reader 'identity)
	for xml-writer = (getf slot-options :xml-writer 'identity)
	do
	(remf slot-options :xml-gi)
	(remf slot-options :xml-reader)
	(remf slot-options :xml-writer)
	collect (list slot-name xml-gi xml-reader xml-writer)
	into xml-slots
	collect (list* slot-name slot-options) into normal-slots
	finally
	(return
	  `(progn
	    (defclass ,name ,(or supers '(specialized-element))
	      ,normal-slots ,@options)
	    (defmethod element-gi ((element ,name)) ,gi)
	    (defmethod parse-attribute or ((element ,name) key value)
	      (declare (ignorable key value))
	      (cond
		,@(loop for (slot-name xml-gi xml-reader xml-writer)
			in xml-slots
			collect
			`((string= key ,xml-gi)
			  (values ',slot-name
			          (funcall (function ,xml-reader) value))))))
	    (defmethod unparse-attributes nconc ((element ,name))
	      (nconc
	       ,@(loop for (slot-name xml-gi xml-reader xml-writer)
		       in xml-slots
		       collect
		       `(when (slot-boundp element ',slot-name)
			  (list
			    ,xml-gi
			    (funcall (function ,xml-writer)
			             (slot-value element ',slot-name)))))))
	    (setf (symbol-xml-class ',ns ',gi) ',name)))))
