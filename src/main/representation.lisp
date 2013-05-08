;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; representation.cl --- Internal representation for XML
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: representation.cl,v 1.3 2003/01/16 02:41:12 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; This file defines the basic internal representation of XML in CLiX,
;;;; which basically breaks down into two sections:
;;;;
;;;; a) The basic representation of XML, which maps 1:1 to the raw XML
;;;; constructs, and
;;;;
;;;; b) any advanced application-specific representations, which can
;;;; be defined by the tools provided in CLiX.
;;;;

(defclass node ()
  ((parent :initform nil :reader node-parent))
  (:documentation
   "Base class of all nodes in the internal representation of XML."))

(defgeneric node-children (node)
  (:documentation
   "Get the list of all child nodes of the given node."))

(defclass processing-instruction (node)
  ((target :initarg :target :accessor processing-instruction-target)
   (data :initarg :data :accessor processing-instruction-data))
  (:documentation
   "Internal representation of processing instructions."))

(defmethod node-children ((node processing-instruction))
  nil)

(defclass element (node)
  ((children :initarg :children :initform nil :accessor node-children))
  (:documentation "Base class of all element nodes."))

(defmethod initialize-instance :after
    ((instance element) &rest initargs &key children)
  (declare (ignore initargs))
  (dolist (child children)
    (setf (slot-value child 'parent) instance)))

(defmethod (setf node-children) :before (newvalue (instance element))
  (declare (ignore newvalue))
  (when (slot-boundp instance 'children)
    (dolist (child (slot-value instance 'children))
      (setf (slot-value child 'parent) nil))))

(defmethod (setf node-children) :after (newvalue (instance element))
  (dolist (child newvalue)
    (setf (slot-value child 'parent) instance)))

(defgeneric element-p (node gi)
  (:documentation "Check whether is of the given type or GI."))

(defmethod element-p (node gi)
  nil)

(defclass generic-element (element)
  ((gi :initarg :gi :accessor element-gi)
   (attributes :initarg :attributes :initform nil
	       :accessor element-attributes))
  (:documentation
   "Class of all generic element nodes, i.e. part of the basic
internal representation."))

(defmethod element-p ((node generic-element) gi)
  (string= (element-gi node) gi))

(defgeneric element-attribute (element attribute &optional default)
  (:documentation
   "Return the value of the given attribute if it exists, or default
otherwise."))

(defmethod element-attribute
    ((element element) attribute &optional default)
  (loop for (key value) on (element-attributes element) by #'cddr
	do (when (string= key attribute) (return value))
	finally (return default)))

(defclass pcdata (node)
  ((text :initarg :text :accessor pcdata-text)))

(defmethod node-children ((node pcdata))
  nil)

(defclass entity-reference (node)
  ((name :initarg :name :accessor entity-reference-name)))

(defmethod node-children ((node entity-reference))
  nil)

(defclass char-reference (node)
  ((name :initarg :name :accessor char-reference-name)))

(defmethod node-children ((node char-reference))
  nil)
