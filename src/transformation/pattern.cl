;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; pattern.cl --- Pattern-Matching Engine
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: pattern.cl,v 1.3 2000/10/27 00:11:11 dent Exp $

(in-package :CLiX-Transform)

;;;; %File Description:
;;;; 
;;;; The contents of this file defines the internal and external
;;;; representation of patterns, conversions between the two, equality
;;;; and ordering relations on patterns, interpretative and compiled
;;;; matching of patterns, and binding of variables in patterns, as
;;;; well as pattern macros and some utility code for defining
;;;; specific pattern classes.
;;;; 


;;; Abstract base class of all patterns.

;;; The GFs defined on this are part of the external and internal
;;; interface of the pattern-matching "engine":
;;;
;;; - `parse-pattern-clause' is used by `parse-pattern' to parse the
;;;   external representation of patterns into the internal
;;;   object-based representation.
;;; 
;;; - `unparse-pattern' is the reverse operation, which regenerates
;;;   the external representation from the internal representation.
;;;
;;; - `pattern-equal' tests the equality of patterns based on their
;;;   structural identity.
;;;
;;; - `pattern-precedence' returns the precedence level of a pattern
;;;   which is used when ordering patterns based on their specificity.
;;;   A higher precedence indicates a higher specificity.
;;;
;;; - `pattern-variables' returns a list of all variable names that
;;;   a pattern might possibly bind on success.
;;;
;;; - `pattern-match' tries to match a given pattern with a given
;;;   XML tree.  On failure it will return nil, on success it will
;;;   return three values: T to indicate success, the "result" of the
;;;   match, which should be ignored, and a keyword-list of the form
;;;   (var1 val1 var2 val2 ...) that contains all bindings produced by
;;;   the pattern.
;;;
;;; - `pattern-code' will return a Lisp form that will at runtime
;;;   produce the equivalent effect of calling `pattern-match' on the
;;;   given pattern and tree-variable.

(defclass pattern ()
  ())

(defmethod make-load-form ((pattern pattern) &optional environment)
  (make-load-form-saving-slots pattern :environment environment))

(defun parse-pattern (pattern)
  "Parse an external representation of a pattern into its internal
representation object."
  (cond
    ((eq pattern t)
     (parse-pattern-clause 'true nil))
    ((eq pattern nil)
     (parse-pattern-clause 'false nil))
    ((consp pattern)
     (destructuring-bind (keyword &rest args) pattern
       (parse-pattern-clause keyword args)))
    (t
     (error "Illegal pattern spec: ~S" pattern))))

(defun pattern-equal (pattern1 pattern2)
  "Test the equality of two patterns."
  (equal (unparse-pattern pattern1) (unparse-pattern pattern2)))

(defgeneric parse-pattern-clause (keyword args)
  (:documentation "Parse a pattern object from the given keyword and args."))

(defgeneric unparse-pattern (pattern)
  (:documentation "Unparse a pattern object into its surface list syntax."))

(defgeneric pattern-precedence (pattern)
  (:documentation "Return the precedence level of a pattern."))

(defgeneric pattern-variables (pattern)
  (:documentation
   "Return a list of variables that the pattern can bind on success."))

(defgeneric pattern-match (pattern tree)
  (:documentation
   "Try to match the tree with the given pattern.  On failure this
returns NIL, on success it returns T, the result of the match, and a
list of bound variables with their values."))

(defgeneric pattern-code (pattern tree-var)
  (:documentation
   "Produce a Lisp form that has at runtime the same effect as if
calling `pattern-match' on the given pattern and tree-variable."))

;;; Some default methods

(defmethod pattern-precedence ((pattern pattern))
  1)

(defmethod pattern-variables ((pattern pattern))
  nil)

(defmethod pattern-code ((pattern pattern) tree-var)
  `(pattern-match ',pattern ,tree-var))


;;; Abstract base classes for certain classes of patterns

;;; The following base classes introduce certain state and behaviour
;;; that a large number of pattern classes utilize, and which has
;;; therefore been factored out to general base classes:
;;;
;;; - `encapsulating-pattern' provides for patterns that encapsulate
;;;   exactly one sub-pattern.
;;;
;;; - `combining-pattern' provides for patterns that combine any
;;;   number of sub-patterns.

(defclass encapsulating-pattern (pattern)
  ((sub-pattern :initarg :sub-pattern
		:accessor encapsulating-pattern-sub-pattern)))

(defmethod pattern-precedence ((pattern encapsulating-pattern))
  (pattern-precedence (encapsulating-pattern-sub-pattern pattern)))

(defmethod pattern-variables ((pattern encapsulating-pattern))
  (pattern-variables (encapsulating-pattern-sub-pattern pattern)))

(defclass combining-pattern (pattern)
  ((sub-patterns :initarg :sub-patterns
		 :accessor combining-pattern-sub-patterns)))

(defmethod pattern-variables ((pattern combining-pattern))
  (loop for sub-pattern in (combining-pattern-sub-patterns pattern)
	appending (pattern-variables sub-pattern)))


;;; General patterns

;;; The following classes define patterns that are generic to all uses
;;; of pattern matching, i.e. logical operators, variable binding,
;;; constants and precedence manipulation:
;;;
;;; - `binding-pattern' binds a variable to the result of its
;;;   sub-pattern, if it matches.
;;;
;;; - `with-pattern' tries to match its first sub-pattern.  If it
;;;   matches then it tries to match the result of the first
;;;   sub-pattern with its second sub-pattern.  The result is the
;;;   result of the second sub-pattern, and the bindings are the
;;;   bindings of both sub-patterns on success.
;;;
;;; - `precedence-pattern' modifies the precedence of the pattern it
;;;   encapsulates to the one provided by the user.
;;;
;;; - `true-pattern' and `false-pattern' are constant patterns that
;;;   always/never match.  Note that t and nil are treated specially
;;;   by the parse-pattern code, so that they can be used as
;;;   short-hands for `(true)' and `(false)' respectively.
;;;
;;; - `and-pattern', `and1-pattern', `or-pattern' and `not-pattern'
;;;   are the usual (n-nary, short-circuiting) logical operators,
;;;   where `and-pattern' and `or-pattern' return the first result
;;;   that doesn't/does match, whereas `and1-pattern' always returns
;;;   the first result.
;;;
;;; - `typep-pattern' checks that the current node is of the given
;;;   type.
;;;
;;; - `funcall-pattern' applies the given function object to the
;;;   current tree, and matches if the function returns non-nil.

(defclass binding-pattern (encapsulating-pattern)
  ((variable :initarg :variable :reader binding-pattern-variable)))

(defmethod parse-pattern-clause ((keyword (eql 'bind)) args)
  (destructuring-bind (variable sub-pattern) args
    (make-instance 'binding-pattern
		   :variable variable
		   :sub-pattern (parse-pattern sub-pattern))))

(defmethod unparse-pattern ((pattern binding-pattern))
  `(bind
    ,(binding-pattern-variable pattern)
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-variables ((pattern binding-pattern))
  (cons (binding-pattern-variable pattern)
	(pattern-variables (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern binding-pattern) tree)
  (multiple-value-bind (match result bindings)
      (pattern-match (encapsulating-pattern-sub-pattern pattern) tree)
    (when match
      (values match result
	      (list* (binding-pattern-variable pattern) result bindings)))))

(defclass with-pattern (encapsulating-pattern)
  ((value-pattern :initarg :value-pattern :reader with-pattern-value-pattern)))

(defmethod parse-pattern-clause ((keyword (eql 'with)) args)
  (destructuring-bind (value-pattern sub-pattern) args
    (make-instance 'with-pattern
		   :value-pattern (parse-pattern value-pattern)
		   :sub-pattern (parse-pattern sub-pattern))))

(defmethod unparse-pattern ((pattern with-pattern))
  `(with
    ,(unparse-pattern (with-pattern-value-pattern pattern))
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-variables ((pattern with-pattern))
  (append (pattern-variables (with-pattern-value-pattern pattern))
	  (pattern-variables (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-precedence ((pattern with-pattern))
  (+ (pattern-precedence (with-pattern-value-pattern pattern))
     (pattern-precedence (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-match ((pattern with-pattern) tree)
  (multiple-value-bind (value-match value-result value-bindings)
      (pattern-match (with-pattern-value-pattern pattern) tree)
    (when value-match
      (multiple-value-bind (match result bindings)
	  (pattern-match (encapsulating-pattern-sub-pattern pattern)
			 value-result)
	(when match
	  (values match result (append value-bindings bindings)))))))

(defclass precedence-pattern (encapsulating-pattern)
  ((precedence :initarg :precedence :reader precedence-pattern-precedence)))

(defmethod parse-pattern-clause ((keyword (eql 'precedence)) args)
  (destructuring-bind (precedence sub-pattern) args
    (make-instance 'precedence-pattern
		   :precedence precedence
		   :sub-pattern (parse-pattern sub-pattern))))

(defmethod unparse-pattern ((pattern precedence-pattern))
  `(precedence
    ,(precedence-pattern-precedence pattern)
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-precedence ((pattern precedence-pattern))
  (precedence-pattern-precedence pattern))

(defmethod pattern-match ((pattern precedence-pattern) tree)
  (pattern-match (encapsulating-pattern-sub-pattern pattern) tree))

(defclass true-pattern (pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'true)) args)
  (assert (null args))
  (make-instance 'true-pattern))

(defmethod unparse-pattern ((pattern true-pattern))
  `(true))

(defmethod pattern-precedence ((pattern true-pattern))
  0)

(defmethod pattern-match ((pattern true-pattern) tree)
  (values t tree nil))

(defclass false-pattern (pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'false)) args)
  (assert (null args))
  (make-instance 'false-pattern))

(defmethod unparse-pattern ((pattern false-pattern))
  `(false))

(defmethod pattern-precedence ((pattern false-pattern))
  0)

(defmethod pattern-match ((pattern false-pattern) tree)
  (declare (ignore tree))
  nil)

(defclass and-pattern (combining-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'and)) args)
  (destructuring-bind (&rest patterns) args
    (make-instance 'and-pattern
		   :sub-patterns (mapcar #'parse-pattern patterns))))

(defmethod unparse-pattern ((pattern and-pattern))
  `(and ,@(mapcar #'unparse-pattern (combining-pattern-sub-patterns pattern))))

(defmethod pattern-precedence ((pattern and-pattern))
  (reduce #'+ (combining-pattern-sub-patterns pattern)
	  :key #'pattern-precedence))

(defmethod pattern-match ((pattern and-pattern) tree)
  (let (and-match and-result and-bindings)
    (dolist (sub-pattern (combining-pattern-sub-patterns pattern)
	     (values and-match and-result and-bindings))
      (multiple-value-bind (match result bindings)
	  (pattern-match sub-pattern tree)
	(unless match
	  (return nil))
	(setq and-match match and-result result
	      and-bindings (nconc bindings and-bindings))))))

(defclass and1-pattern (and-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'and1)) args)
  (destructuring-bind (&rest patterns) args
    (make-instance 'and1-pattern
		   :sub-patterns (mapcar #'parse-pattern patterns))))

(defmethod unparse-pattern ((pattern and1-pattern))
  `(and1 ,@(mapcar #'unparse-pattern
		   (combining-pattern-sub-patterns pattern))))

(defmethod pattern-match ((pattern and1-pattern) tree)
  (let ((patterns (combining-pattern-sub-patterns pattern)))
    (multiple-value-bind (and-match and-result and-bindings)
	(pattern-match (first patterns) tree)
      (when and-match
	(dolist (sub-pattern (rest patterns)
		 (values and-match and-result and-bindings))
	  (multiple-value-bind (match result bindings)
	      (pattern-match sub-pattern tree)
	    (declare (ignore result))
	    (unless match
	      (return nil))
	    (setq and-match match
		  and-bindings (nconc bindings and-bindings))))))))

(defclass or-pattern (combining-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'or)) args)
  (destructuring-bind (&rest patterns) args
    (make-instance 'or-pattern
		   :sub-patterns (mapcar #'parse-pattern patterns))))

(defmethod unparse-pattern ((pattern or-pattern))
  `(or ,@(mapcar #'unparse-pattern (combining-pattern-sub-patterns pattern))))

(defmethod pattern-precedence ((pattern or-pattern))
  (reduce #'min (combining-pattern-sub-patterns pattern)
	  :key #'pattern-precedence
	  :initial-value most-positive-fixnum))

(defmethod pattern-match ((pattern or-pattern) tree)
  (dolist (sub-pattern (combining-pattern-sub-patterns pattern))
    (multiple-value-bind (match result bindings)
	(pattern-match sub-pattern tree)
      (when match
	(return (values match result bindings))))))

(defclass not-pattern (encapsulating-pattern)
  ())

(defmethod parse-pattern-clause ((keyword (eql 'not)) args)
  (destructuring-bind (sub-pattern) args
    (make-instance 'not-pattern
		   :sub-pattern (parse-pattern sub-pattern))))

(defmethod unparse-pattern ((pattern not-pattern))
  `(not
    ,(unparse-pattern (encapsulating-pattern-sub-pattern pattern))))

(defmethod pattern-variables ((pattern not-pattern))
  nil)

(defmethod pattern-match ((pattern not-pattern) tree)
  (multiple-value-bind (match result bindings)
      (pattern-match (encapsulating-pattern-sub-pattern pattern) tree)
    (if match
	nil
	(values t nil nil))))

(defclass typep-pattern (pattern)
  ((type :initarg :type :reader typep-pattern-type)))

(defmethod parse-pattern-clause ((keyword (eql 'typep)) args)
  (destructuring-bind (type) args
    (make-instance 'typep-pattern :type type)))

(defmethod unparse-pattern ((pattern typep-pattern))
  `(typep ,(typep-pattern-type pattern)))

(defmethod pattern-match ((pattern typep-pattern) tree)
  (when (typep tree (typep-pattern-type pattern))
    (values t tree nil)))

(defclass funcall-pattern (pattern)
  ((function :initarg :function :reader funcall-pattern-function)))

(defmethod parse-pattern-clause ((keyword (eql 'funcall)) args)
  (destructuring-bind (function) args
    (make-instance 'funcall-pattern :function function)))

(defmethod unparse-pattern ((pattern funcall-pattern))
  `(funcall ,(funcall-pattern-function pattern)))

(defmethod pattern-match ((pattern funcall-pattern) tree)
  (let ((result (funcall (funcall-pattern-function pattern) tree)))
    (when result
      (values t result nil))))


;;; Pattern Macros

;;; Pattern macros are patterns that are extensions of the external
;;; syntax, that are mapped into existing patterns, i.e. they are only
;;; present in the external syntax, and lose their existance and
;;; identity once they are parsed into the internal syntax.

(defmacro define-pattern-macro (keyword arglist &body body)
  "Define a pattern macro for the given keyword and argument list.
The body should return the new external syntax that the macro mapped
into."
  (let ((args (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defmethod parse-pattern-clause ((keyword (eql ',keyword)) ,args)
       (parse-pattern 
	(destructuring-bind ,arglist ,args ,@body))))))
