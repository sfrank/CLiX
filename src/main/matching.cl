;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; matching.cl --- Simple Pattern-Matching and Transformation Facility
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: matching.cl,v 1.1.1.1 2000/10/23 12:19:57 dent Exp $

(in-package :CLiX)

;;;; %File Description:
;;;; 
;;;; 
;;;; 

(defclass transformation ()
  ())

(defgeneric transformation-matches-p (transformation node))

(defgeneric transformation-transform (transformation node match))

(defun apply-transformation (transformation root)
  (let ((match (transformation-matches-p transformation root)))
    (unless match
      (error "Transformation failed"))
    (transformation-rule-transform rule root match)))

(defclass identity-transformation (transformation)
  ())

(defmethod transformation-matches-p
    ((transformation identity-transformation) node)
  t)

(defmethod transformation-transform
    ((transformation identity-transformation) node match)
  (assert (eq match t))
  node)

(defclass 