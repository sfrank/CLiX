;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; template.cl --- XML template-based rendering
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: template.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; This renders an internal XML representation with special
;;;; interpretation of processing instructions along the way.
;;;; 

(defvar *xml-template-handlers* (make-hash-table :test #'equal))

(defun set-xml-template-handler (name fun)
  (setf (gethash name *xml-template-handlers*) fun))

(defun get-xml-template-handler (name)
  (gethash name *xml-template-handlers* nil))

(defun render-xml-template (template stream &rest bindings)
  (progv (mapcar #'first bindings) (mapcar #'second bindings)
    (render-template-node template stream)))

(defmacro with-xml-template-output ((stream &rest bindings) &body body)
  (let ((stream-var (gensym)))
    `(let ((,stream-var ,stream))
       (write-line "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>"
	           ,stream-var)
       (render-xml-template (progn ,@body) ,stream-var ,@bindings))))

(defgeneric render-template-node (node stream))

(defmethod render-template-node ((node element) stream)
  (write-char #\< stream)
  (write-line (element-gi node) stream)
  (loop for (key value) on (element-attributes node) by #'cddr
	do
	(write-char #\Space stream)
	(write-string key stream)
	(write-char #\= stream)
	(write-char #\' stream)
	(write-string value stream)
	(write-char #\' stream))
  (write-char #\> stream)
  (dolist (sub-node (node-children node))
    (render-template-node sub-node stream))
  (write-char #\< stream)
  (write-char #\/ stream)
  (write-line (element-gi node) stream)
  (write-char #\> stream))

(defmethod render-template-node ((node pcdata) stream)
  (write-string (pcdata-text node) stream))

(defmethod render-template-node ((node processing-instruction) stream)
  (let* ((target (processing-instruction-target node))
	 (handler (get-xml-template-handler target)))
    (if (null handler)
	(render-node node stream)
	(let ((result (funcall handler target
			       (processing-instruction-data node))))
	  (if (listp result)
	      (dolist (sub-node result)
		(render-template-node sub-node stream))
	      (render-template-node result stream))))))

;;;; Sample handlers

(defvar *cl-eval-handler-package* (find-package "CLIX-USER")
  "Package that cl-eval code is read and evaluated in.")

(defun cl-eval-handler (target data)
  (declare (ignore target))
  (handler-case (let ((*package* *cl-eval-handler-package*))
		  (eval (read-from-string data)))
    (error (condition)
      (format nil "~A" condition))))

(set-xml-template-handler "cl-eval" #'cl-eval-handler)
