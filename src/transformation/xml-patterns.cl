;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; xml-patterns.cl --- XML-specifc patterns
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: xml-patterns.cl,v 1.3 2000/10/27 00:11:11 dent Exp $

(in-package :CLiX-Transform)

;;;; %File Description:
;;;; 
;;;; This file defines a couple of XML-specific patterns, based on the
;;;; internal representation for XML of CLiX.
;;;; 

;;; Descendancy-related patterns

;;; The following encapsulating patterns are used to check for the
;;; existence of descendants of nodes that fullfil a certain pattern:
;;;
;;; - `child-pattern' checks for the existence of a direct child.
;;;
;;; - `parent-pattern' checks for the existence of a direct parent.
;;;
;;; - `descendant-pattern' checks for the existence of a direct or
;;;   transitive child.
;;;
;;; - `ancestor-pattern' checks for the existence of a direct or
;;; transitive parent.
;;;
;;; - `every-child-pattern' checks that every direct child fulfills
;;;   the given pattern.  The result is the set of all children.
;;;
;;; - `every-descendant-pattern' checks that every descendant fulfills
;;;   the given pattern.  The result is the set of all descendants.
;;;
;;; - `some-child-pattern' collects every direct child that fulfills
;;;   the given pattern.
;;;
;;; - `some-descendant-pattern' collects every descendant that
;;;   fulfills the given pattern.
;;;
;;; - `element-pcdata-pattern' descends into the pcdata content of an
;;;   element.

(defclass child-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'child)) args)
  (destructuring-bind (pattern) args
    (make-instance 'child-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern child-pattern))
  `(child ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern child-pattern) tree)
  (dolist (child (node-children tree))
    (multiple-value-bind (match result bindings)
	(pattern-match (encapsulating-pattern-sub-pattern pattern) child)
      (when match
	(return (values match result bindings))))))

(defclass parent-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'parent)) args)
  (destructuring-bind (pattern) args
    (make-instance 'parent-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern parent-pattern))
  `(parent ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern parent-pattern) tree)
  (when (node-parent tree)
    (pattern-match (encapsulating-pattern-sub-pattern pattern)
		   (node-parent tree))))

(defclass descendant-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'descendant)) args)
  (destructuring-bind (pattern) args
    (make-instance 'descendant-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern descendant-pattern))
  `(descendant ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern descendant-pattern) tree)
  (let ((test-pattern (encapsulating-pattern-sub-pattern pattern)))
    (labels ((test-pattern (node)
	       (multiple-value-bind (match result bindings)
		   (pattern-match test-pattern node)
		 (if match
		     (values match result bindings)
		     (dolist (child (node-children node))
		       (multiple-value-bind (match result bindings)
			   (test-pattern child)
			 (when match
			   (return (values match result bindings)))))))))
      (test-pattern tree))))

(defclass ancestor-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'ancestor)) args)
  (destructuring-bind (pattern) args
    (make-instance 'ancestor-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern ancestor-pattern))
  `(descendant ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern ancestor-pattern) tree)
  (let ((test-pattern (encapsulating-pattern-sub-pattern pattern)))
    (labels ((test-pattern (node)
	       (multiple-value-bind (match result bindings)
		   (pattern-match test-pattern node)
		 (if match
		     (values match result bindings)
		     (when (node-parent node)
		       (test-pattern (node-parent node)))))))
      (test-pattern tree))))

(defclass every-child-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'every-child)) args)
  (destructuring-bind (pattern) args
    (make-instance 'every-child-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern every-child-pattern))
  `(every-child
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern every-child-pattern) tree)
  (when (typep tree 'node)
    (loop with sub-pattern = (encapsulating-pattern-sub-pattern pattern)
	  for child in (node-children tree)
	  collect
	  (multiple-value-bind (match result bindings)
	      (pattern-match sub-pattern child)
	    (declare (ignore bindings))
	    (unless match
	      (return nil))
	    result)
	  into results
	  finally
	  (return (values t results nil)))))

(defclass some-child-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'some-child)) args)
  (destructuring-bind (pattern) args
    (make-instance 'some-child-pattern :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern some-child-pattern))
  `(some-child
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern some-child-pattern) tree)
  (when (typep tree 'node)
    (loop with sub-pattern = (encapsulating-pattern-sub-pattern pattern)
	  for child in (node-children tree)
	  nconcing
	  (multiple-value-bind (match result bindings)
	      (pattern-match sub-pattern child)
	    (declare (ignore bindings))
	    (when match
	      (list result)))
	  into results
	  finally
	  (return (values t results nil)))))

(defclass element-pcdata-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'element-pcdata)) args)
  (destructuring-bind (pattern) args
    (make-instance 'element-pcdata-pattern
		   :sub-pattern (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern element-pcdata-pattern))
  `(element-pcdata
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern element-pcdata-pattern) tree)
  (when (typep tree 'element)
    (pattern-match (encapsulating-pattern-sub-pattern pattern)
		   (element-pcdata tree))))

;;; Node-specific patterns

;;; The following patterns test for certain node sub-types and their
;;; properties:
;;;
;;; - `element-pattern' tests whether the given node is an element
;;;   with the given GI.
;;;
;;; - `attribute-pattern' tests whether the given node is an element
;;;   and has an attribute of the given name.
;;;
;;; - `substring-pattern' tests whether the given string contains the
;;;   given substring.

(defclass element-pattern (pattern)
  ((gi :initarg :gi :accessor element-pattern-gi)))

(defmethod parse-pattern-clause ((keyword (eql 'element)) args)
  (destructuring-bind (gi) args
    (make-instance 'element-pattern :gi gi)))

(defmethod unparse-pattern ((pattern element-pattern))
  `(element ,(element-pattern-gi pattern)))

(defmethod pattern-match ((pattern element-pattern) tree)
  (when (element-p tree (element-pattern-gi pattern))
    (values t tree nil)))

(defclass attribute-pattern (encapsulating-pattern)
  ((name :initarg :name :accessor attribute-pattern-name)))

(defmethod parse-pattern-clause ((keyword (eql 'attribute)) args)
  (destructuring-bind (name sub-pattern) args
    (make-instance 'attribute-pattern :name name
		   :sub-pattern (parse-pattern sub-pattern))))

(defmethod unparse-pattern ((pattern attribute-pattern))
  `(attribute ,(attribute-pattern-name pattern)
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern attribute-pattern) tree)
  (when (typep tree 'element)
    (let ((attr (element-attribute tree (attribute-pattern-name pattern) nil)))
      (when attr
	(pattern-match (encapsulating-pattern-sub-pattern pattern) attr)))))

(defclass substring-pattern (pattern)
  ((substring :initarg :substring :accessor substring-pattern-substring)))

(defmethod parse-pattern-clause ((keyword (eql 'substring)) args)
  (destructuring-bind (substring) args
    (make-instance 'substring-pattern :substring substring)))

(defmethod unparse-pattern ((pattern substring-pattern))
  `(substring ,(substring-pattern-substring pattern)))

(defmethod pattern-precedence ((pattern substring-pattern))
  1)

(defmethod pattern-variables ((pattern substring-pattern))
  nil)

(defmethod pattern-match ((pattern substring-pattern) tree)
  (when (and (typep tree 'string)
	     (search (substring-pattern-substring pattern) tree))
    (values t tree nil)))

;;; Syntactic-sugar pattern macros

(define-pattern-macro text (pattern)
  `(with ,pattern (element-pcdata t)))

(define-pattern-macro child-element (gi &rest patterns)
  `(child (and1 (element ,gi) ,@patterns)))

(define-pattern-macro parent-element (gi &rest patterns)
  `(parent (and1 (element ,gi) ,@patterns)))

(define-pattern-macro cpath (&rest specs)
  (labels ((convert (specs)
	     (when specs
	       (let ((spec (first specs)))
		 (list
		  (if (consp spec)
		      `(child (and1 (bind ,(first spec)
				     (element ,(second spec)))
			       ,@(convert (rest specs))))
		      `(child (and1 (element ,spec)
			       ,@(convert (rest specs))))))))))
    (second (first (convert specs)))))

(define-pattern-macro path (&rest specs)
  (flet ((compute-pattern (spec sub-pattern)
	   (if (consp spec)
	       `(and1 (bind ,(first spec) (element ,(second spec)))
		 ,sub-pattern)
	       `(and1 (element ,spec) ,sub-pattern))))
    (do* ((pattern t `(parent ,(compute-pattern (first list) pattern)))
	  (list specs (rest list)))
	 ((null (rest list))
	  (if list
	      (compute-pattern (first list) pattern)
	      pattern)))))
