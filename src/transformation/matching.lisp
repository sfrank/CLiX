;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; matching.cl --- Matching constructs
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: matching.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX-Transform)

;;;; %File Description:
;;;; 
;;;; This file contains constructs that allow a user to match patterns
;;;; against XML trees.
;;;; 

;;; Utility macros for macro-writing

(defmacro with-unique-names (vars &body body)
  `(let ,(loop for var in vars
               collect `(,var (make-symbol ,(symbol-name var))))
     ,@body))

(defmacro rebinding (vars &body body)
  (loop for var in vars
        for name = (make-symbol (symbol-name var))
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                           (with-unique-names ,vars
                             `(let (,,@temps)
                                ,,@body))))))

;;; Simple manual pattern-matching constructs

;;; Applying patterns

(defmacro with-pattern-match (pattern-spec tree &body body)
  "Try to match the pattern specified by `pattern-spec' against `tree'
which is evaluated.  If this succeeds executes the body in a lexical
environment where all the bindings of `pattern-spec' are in effect and
returns the result of this.  Otherwise the body is not executed, and
nil is returned."
  (with-unique-names (match result bindings)
    (let* ((pattern (parse-pattern pattern-spec))
	   (variables (pattern-variables pattern)))
      `(multiple-value-bind (,match ,result ,bindings)
	   (pattern-match ',pattern ,tree)
	 (declare (ignore ,result))
	 (when ,match
	   (destructuring-bind 
		 (&key ,@(loop for var in variables collect `((,var ,var))))
	       ,bindings
	     ,@body))))))

(defmacro pattern-case (tree &rest clauses)
  "The body of the first clause whose pattern-spec matches the result
of evaluating `tree' is executed in a lexical environment where all
the bindings of the pattern specified by the clause's `pattern-spec'
are in effect and return the result of this.  Otherwise processing
proceeds with the next clause, until there is no further clause to
process, in which case nil is returned."
  (with-unique-names (block tree-var match result bindings)
    `(block ,block
       (let ((,tree-var ,tree))
	 ,@(loop for (pattern-spec . body) in clauses
		 for pattern = (parse-pattern pattern-spec)
		 for variables = (pattern-variables pattern)
		 for lambda-list = `(&key
				     ,@(loop for var in variables
					    collect `((,var ,var))))
		 collect
		 `(multiple-value-bind (,match ,result ,bindings)
		      (pattern-match ',pattern ,tree-var)
		    (declare (ignore ,result))
		    (when ,match
		      (destructuring-bind ,lambda-list ,bindings
			(return-from ,block
			  (progn ,@body))))))))))

;;; Predicate Dispatching transformations

;;; A transformation is a specialized kind of generic function that
;;; takes one argument and dispatches on this argument based on the
;;; patterns of its rules.  Each rule consists of a pattern and a
;;; body.  Of all rules of a transformation the most specific rule
;;; that matches will be executed, i.e. its body will be executed in a
;;; a lexical environment where all the bindings of the pattern are in
;;; effect.
;;;
;;; Pattern specificity is judged according to rule precedence,
;;; i.e. the rule with the highest precedence is considered to be the
;;; most specific.  If no rule is applicable to a given argument, then
;;; an error of type `transformation-failure' will be raised.
;;; 
;;; The body of a rule can decline to handle an argument by invoking
;;; `fail-rule', which will cause the dispatching to resume as if the
;;; current rule hadn't matched in the first place.

(define-condition rule-failure ()
  ())

(define-condition transformation-failure (error)
  ((transformation :initarg :transformation
		   :reader transformation-failure-transformation)
   (tree :initarg :tree :reader transformation-failure-tree)))

(defclass transformation ()
  ((rules :initarg :rules :initform nil :accessor transformation-rules))
  (:metaclass pcl:funcallable-standard-class))

(defclass rule ()
  ((pattern :initarg :pattern :type pattern :accessor rule-pattern)
   (body :initarg :body :type function :accessor rule-body)))

(defmethod transformation-function ((transformation transformation))
  #'(kernel:instance-lambda (tree)
      (loop for rule in (transformation-rules transformation)
	    for pattern = (rule-pattern rule)
	    do
	    (multiple-value-bind (match result bindings)
		(pattern-match pattern tree)
	      (declare (ignore result))
	      (when match
		(handler-case 
		    (return (apply (rule-body rule) bindings))
		  (rule-failure () nil))))
	    finally
	    (error 'transformation-failure
		   :transformation transformation
		   :tree tree))))

(defmethod initialize-instance :after ((instance transformation) &rest args)
  (declare (ignore args))
  (pcl:set-funcallable-instance-function
   instance
   (transformation-function instance)))

(defmethod transformation-add-rule ((transformation transformation) rule)
  (setf (transformation-rules transformation)
	(sort (cons rule
		    (delete rule (transformation-rules transformation)
			    :test
			    #'(lambda (a b)
				(pattern-equal (rule-pattern a)
					       (rule-pattern b)))))
	      #'> :key #'(lambda (rule)
			   (pattern-precedence (rule-pattern rule))))))

(defmacro define-transformation (name &rest options)
  `(progn
    (pcl::proclaim-defgeneric ',name '(tree))
    (setf (fdefinition ',name)
          (make-instance 'transformation ,@options))))

(defmacro define-rule (name pattern-spec &body body)
  (let* ((pattern (parse-pattern pattern-spec))
	 (variables (pattern-variables pattern)))
    `(transformation-add-rule (fdefinition ',name)
      (make-instance 'rule :pattern ',pattern
       :body
       #'(lambda (&key ,@(mapcar #'(lambda (var) `((,var ,var))) variables))
	   (macrolet ((fail-rule () '(signal 'rule-failure)))
	     ,@body))))))
