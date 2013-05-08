;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; wuja.cl --- Implementation of Wuja-style matching
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: wuja.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX-Transform)

;;;; %File Description:
;;;; 
;;;; This file implements matching that is equivalent to the Wuja
;;;; style of matching.
;;;; 

;;; In Wuja a transformation is just a triple of pattern, condition
;;; and body:

(defclass trafo ()
  ((pattern :initarg :pattern :reader trafo-pattern)
   (condition :initarg :condition :reader trafo-condition)
   (body :initarg :body :reader trafo-body)))

;;; Defining Trafos:

(defmacro define-trafo (name pattern-spec condition &body body)
  (let* ((pattern (parse-pattern pattern-spec))
	 (variables (pattern-variables pattern))
	 (lambda-list `(&key ,@(mapcar #'(lambda (x) `((,x ,x))) variables))))
    `(setf (get ',name 'trafo)
	   (make-instance 'trafo :pattern ',pattern
			  :condition #'(lambda ,lambda-list ,condition)
			  :body #'(lambda ,lambda-list ,@body)))))

;;; Applying trafos

(defgeneric apply-trafo (trafo tree)
  (:documentation "Apply a Wuja-style trafo to an XML-tree, returning
the result of applying the body to the first match."))

(defmethod apply-trafo ((trafo symbol) tree)
  (let ((trafo-object (get trafo 'trafo nil)))
    (unless trafo-object (error "Trafo ~S does not exist." trafo))
    (apply-trafo trafo-object tree)))

(defmethod apply-trafo ((trafo trafo) tree)
  (let ((pattern (trafo-pattern trafo))
	(condition (trafo-condition trafo))
	(body (trafo-body trafo)))
    (labels ((apply-children (children)
	       (loop for child in children
		     do
		     (multiple-value-bind (match result bindings)
			 (pattern-match pattern child)
		       (declare (ignore result))
		       (when (and match (apply condition bindings))
			 (return-from apply-trafo (apply body bindings)))
		       (apply-children (node-children child))))))
      (apply-children (list tree)))))

(defgeneric apply-all-trafo (trafo tree)
  (:documentation "Apply a Wuja-style trafo to an XML-tree, returning
the results of all matches in a list."))

(defmethod apply-all-trafo ((trafo symbol) tree)
  (let ((trafo-object (get trafo 'trafo nil)))
    (unless trafo-object (error "Trafo ~S does not exist." trafo))
    (apply-all-trafo trafo-object tree)))

(defmethod apply-all-trafo ((trafo trafo) tree)
  (let ((pattern (trafo-pattern trafo))
	(condition (trafo-condition trafo))
	(body (trafo-body trafo)))
    (labels ((apply-children (children)
	       (loop for child in children
		     nconcing
		     (multiple-value-bind (match result bindings)
			 (pattern-match pattern child)
		       (declare (ignore result))
		       (if (and match (apply condition bindings))
			   (cons (apply body bindings)
				 (apply-children (node-children child)))
			   (apply-children (node-children child)))))))
      (apply-children (list tree)))))

