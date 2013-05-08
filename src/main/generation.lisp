;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; generation.cl --- XML node generation code
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: generation.cl,v 1.3 2003/01/16 02:41:12 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defun flattening-list (&rest elements)
  (do* ((result nil)
	(rest elements (cdr rest))
	(elem (car rest) (car rest)))
       ((null rest) (nreverse result))
    (if (listp elem)
	(dolist (sub-elem elem)
	  (push sub-elem result))
	(push elem result))))

(defmacro with-element ((gi &rest attributes) &body body)
  (if (stringp gi)
      `(make-instance 'generic-element :gi ,gi
	              :attributes (list ,@attributes)
	              :children (flattening-list ,@body))
      `(make-instance ',gi
	              ,@attributes
	              :children (flattening-list ,@body))))

(defmacro with-pcdata (&body body)
  `(make-instance 'pcdata :text (concatenate 'string ,@body)))

(defun make-entity-reference (name)
  (make-instance 'entity-reference :name name))

(defun make-char-reference (name)
  (make-instance 'char-reference :name name))

(defun from-list-template (template)
  (if (stringp template)
      template
      (destructuring-bind ((gi &rest args) &rest children) template
	(if (stringp gi)
	    (make-instance 'generic-element :gi gi :attributes args
			   :children (mapcar #'from-list-template children))
	    (apply #'make-instance gi
		   :children (mapcar #'from-list-template children)
		   args)))))
