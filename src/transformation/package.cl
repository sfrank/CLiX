;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; package.cl --- CLiX-Transform package
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: package.cl,v 1.2 2000/10/27 00:11:11 dent Exp $

(in-package :CL)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defpackage :CLiX-Transform
    (:nicknames :CLiX-T :XML-T)
    (:use :CL :CLiX)
    (:export
     ;; Pattern
     #:pattern
     #:parse-pattern #:pattern-equal
     #:parse-pattern-clause #:unparse-pattern
     #:pattern-precedence
     #:pattern-variables
     #:pattern-match #:pattern-code
     #:encapsulating-pattern
     #:combining-pattern
     #:binding-pattern #:bind
     #:with-pattern #:with
     #:precedence-pattern #:precedence
     #:true-pattern #:true
     #:false-pattern #:false
     #:and-pattern #:and
     #:and1-pattern #:and1
     #:or-pattern #:or
     #:not-pattern #:not
     #:typep-pattern #:typep
     #:funcall-pattern #:funcall
     #:define-pattern-macro
     ;; XML-Patterns
     #:child-pattern #:child
     #:parent-pattern #:parent
     #:descendant-pattern #:descendant
     #:ancestor-pattern #:ancestor
     #:every-child-pattern #:every-child
     #:some-child-pattern #:some-child
     #:element-pcdata-pattern #:element-pcdata
     #:element-pattern ;element
     #:attribute-pattern #:attribute
     #:substring-pattern #:substring
     #:text
     #:child-element #:parent-element
     #:cpath #:path
     ;; Matching
     #:with-pattern-match
     #:pattern-case
     #:rule-failure
     #:transformation-failure
     #:transformation-failure-transformation
     #:transformation-failure-tree
     #:define-transformation
     #:define-rule
     ))
