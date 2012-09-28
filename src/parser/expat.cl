;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; expat.cl --- Expat based external parser
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: expat.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; This file implements a parser backend based on an external
;;;; expat-based preprocessor that does the low-level XML parsing and
;;;; returns a Lisp-friendly nested list representation which we can
;;;; easily convert to the internal representation in CL.
;;;; 

(defvar *xml-expat-parser-path*
  (probe-file (make-pathname :name "elements" :type :unspecific
			     :defaults *load-pathname*)))

(defun parse-xml-from-list (list namespace)
  (cond
    ((stringp list)
     (make-instance 'pcdata :text list))
    ((not (consp list))
     (error "Strange input from external XML pre-processor ~S." list))
    ((and (stringp (car list)) (stringp (cdr list)))
     (make-instance 'processing-instruction
		    :target (car list)
		    :data (cdr list)))
    (t
     (destructuring-bind ((gi &rest args) &rest children) list
       (make-instance (symbol-xml-class namespace gi)
		      :gi gi :attributes args
		      :children
		      (mapcar #'(lambda (x) (parse-xml-from-list x namespace))
			      children))))))

(defun parse-xml-from-file (file &optional (ns 'xml-default-class-namespace))
  (let* ((process
	  #+cmu
	   (ext:run-program *xml-expat-parser-path* nil
			    :input file :output :stream :wait nil))
	 (output (ext:process-output process)))
    (let ((list (ignore-errors (read output))))
      (ext:process-wait process)
      (ext:process-close process)
      (unless (and (zerop (ext:process-exit-code process)) list)
	(error "Error parsing XML file ~A." file))
      (parse-xml-from-list list ns))))
