;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; rendering.cl --- Rendering of XML nodes
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: rendering.cl,v 1.4 2003/01/16 02:41:12 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defgeneric render-node (node stream)
  (:documentation
   "Render the given XML node onto the given stream."))

(defmethod render-node ((node processing-instruction) stream)
  (write-char #\< stream)
  (write-char #\? stream)
  (write-string (processing-instruction-target node) stream)
  (write-char #\Space stream)
  (write-string (processing-instruction-data node) stream)
  (write-char #\? stream)
  (write-char #\> stream))

(defmethod render-node ((node element) stream)
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
    (render-node sub-node stream))
  (write-char #\< stream)
  (write-char #\/ stream)
  (write-line (element-gi node) stream)
  (write-char #\> stream))

(defmethod render-node ((node pcdata) stream)
  (declare (optimize (speed 3)) (type stream stream))
  (let ((text (pcdata-text node)))
    (declare (string text))
    (loop for char of-type character across text
	  do
	  (case char
	    (#\< (write-string "&lt;" stream))
	    (#\> (write-string "&gt;" stream))
	    (#\& (write-string "&amp;" stream))
	    (#\" (write-string "&quot;" stream))
	    (t (write-char char stream))))))

(defmethod render-node ((node entity-reference) stream)
  (write-char #\& stream)
  (princ (entity-reference-name node) stream)
  (write-char #\; stream))

(defmethod render-node ((node char-reference) stream)
  (write-char #\& stream)
  (write-char #\# stream)
  (princ (char-reference-name node) stream)
  (write-char #\; stream))

(defmacro with-xml-output ((stream &key (xml-decl t)) &body body)
  (let ((stream-var (gensym)))
    `(let ((,stream-var ,stream))
       (when ,xml-decl
         (write-line "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" ?>"
	             ,stream-var))
       (render-node (progn ,@body) ,stream-var))))
