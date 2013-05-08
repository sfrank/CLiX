;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; access.cl --- Advanced node access and querying functions
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: access.cl,v 1.2 2000/10/26 17:27:37 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defun node-element-children (node)
  (remove-if-not #'(lambda (x) (typep x 'element)) (node-children node)))

(defun find-element (top gi)
  (find gi (node-element-children top) :key #'element-gi :test #'string=))

#+NIL
(defun find-elements (top gi)
  (delete gi (node-element-children top) :key #'element-gi :test #'string/=))

(defun find-elements (top gi)
  (loop for node in (node-children top)
	when (and (typep node 'element)
		  (string= gi (element-gi node)))
	collect node))

(defun element-pcdata (top)
  (let ((children (node-children top)))
    (cond
      ((and children (null (cdr children)))
       (if (typep (car children) 'pcdata)
	   (pcdata-text (car children))
	   ""))
      (t
       (let ((pcdata-children (remove-if-not #'(lambda (x) (typep x 'pcdata))
					     children)))
	 (cond
	   ((null pcdata-children)
	    "")
	   ((null (cdr pcdata-children))
	    (pcdata-text (car pcdata-children)))
	   (t
	    (reduce #'(lambda (x y) (concatenate 'string x y))
		    pcdata-children
		    :key #'pcdata-text
		    :initial-value ""))))))))

;;;; Node Paths

(defun access-path (set path)
  (cond
    ((null path)
     set)
    ((eq (first path) :wild)
     (access-path (mapcan #'node-element-children set) (rest path)))
    ((eq (first path) :text)
     (access-path (mapcar #'element-pcdata set) (rest path)))
    ((eq (first path) :first)
     (if (null (rest path))
	 (first set)
	 (access-path (list (first set)) (rest path))))
    ((eq (first path) :last)
     (if (null (rest path))
	 (first (last set))
	 (access-path (last set) (rest path))))
    (t
     (access-path
      (loop for elem in set nconcing (find-elements elem (first path)))
      (rest path)))))

(defmacro select-path (top &rest path)
  `(access-path (list ,top) ',path))

(defmacro bind-xml-paths (root (&rest bindings) &body body)
  (let ((root-var (gensym)))
    `(let ((,root-var ,root))
      (unless (listp ,root-var) (setq ,root-var (list ,root-var)))
      (let (,@(loop for (var . path) in bindings
		    collect `(,var (access-path ,root-var ',path))))
	,@body))))


;;;; Pattern Matching
(defun element-matches-p (element pattern)
  "Return true if the element matches the given pattern."
  (cond
    ((atom pattern)
     (ecase pattern
       ((t) t)))
    (t
     (case (car pattern)
       (not (not (element-matches-p element (second pattern))))
       (and (every #'(lambda (p) (element-matches-p element p))
		   (rest pattern)))
       (or (some #'(lambda (p) (element-matches-p element p)) (rest pattern)))
       (child
	(let ((new-pattern (cons 'and (cddr pattern))))
	  (some #'(lambda (e) (element-matches-p e new-pattern))
		(find-elements element (cadr pattern)))))
       (children
	(let ((new-pattern (cons 'and (cddr pattern))))
	  (every #'(lambda (e) (element-matches-p e new-pattern))
		 (find-elements element (cadr pattern)))))
       (attribute
	(let ((attr (element-attribute element (second pattern))))
	  (and attr (search (third pattern) attr))))
       (text
	(search (second pattern) (element-pcdata element)))
       (t
	(apply (car pattern) element (cdr pattern)))))))

(defun search-elements (elements pattern)
  (remove-if-not #'(lambda (elem) (element-matches-p elem pattern)) elements))
