;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; teitrafo.cl --- Demonstration of Transformations on TEI XML
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: teitrafo.cl,v 1.2 2000/10/27 00:11:11 dent Exp $

(in-package :CL)

;;;; %File Description:
;;;; 
;;;; This file demonstrates how one would transform XML (TEI Play)
;;;; into XML (HTML).
;;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "CLiX-Transform"))

;;; Set up specialized representations for the HTML subset we use.

(defpackage :html
    (:use :CL :CLiX)
  (:EXPORT #:html #:head #:title #:body #:h1 #:h2 #:ul #:li #:p))

(in-package :html)

(define-xml-element (html "html" html) ()
  ())

(define-xml-element (head "head" html) ()
  ())

(define-xml-element (title "title" html) ()
  ())

(define-xml-element (body "body" html) ()
  ())

(define-xml-element (h1 "h1" html) ()
  ())

(define-xml-element (h2 "h2" html) ()
  ())

(define-xml-element (ul "ul" html) ()
  ())

(define-xml-element (li "li" html) ()
  ())

(define-xml-element (p "p" html) ()
  ())

;;; Set up specialized representations for the TEI Play subset we use.

(defpackage :tei (:use :CL :CLiX)
  (:export #:tei #:play #:title #:personae #:persona #:pgroup #:grpdescr))

(in-package :tei)

(define-xml-element (play "PLAY" tei) ()
  ())

(define-xml-element (title "TITLE" tei) ()
  ())

(define-xml-element (personae "PERSONAE" tei) ()
  ())

(define-xml-element (persona "PERSONA" tei) ()
  ())

(define-xml-element (pgroup "PGROUP" tei) ()
  ())

(define-xml-element (grpdescr "GRPDESCR" tei) ()
  ())

;;; The transformation code itself

(defpackage :CLiX-Demo (:use :CL :CLiX :CLiX-Transform)
  (:export #:transform-file))

(in-package :CLiX-Demo)

;;; Support utility functions

(defun map-children (fun node)
  (loop for child in (node-children node)
	for value = (funcall fun child)
        append (if (listp value) value (list value))))

;;; The transformation proper

(define-transformation transform-play)

(define-rule transform-play (and (bind node (element tei:play))
				 (bind title (text (child-element tei:title))))
  (with-element (html:html)
    (with-element (html:head)
      (with-element (html:title)
	(make-instance 'pcdata :text title)))
    (with-element (html:body)
      (map-children #'transform-play node))))

(define-rule transform-play (bind node (path tei:play tei:title))
  (with-element (html:h1)
    (make-instance 'pcdata :text (element-pcdata node))))

(define-rule transform-play (path tei:personae tei:title)
  nil)

(define-rule transform-play (and (bind node (element tei:personae))
				 (bind title (text (child-element tei:title))))
  (list
   (with-element (html:h2)
     (make-instance 'pcdata :text title))
   (with-element (html:ul)
     (map-children #'transform-play node))))

(define-rule transform-play (bind person (text (element tei:persona)))
  (with-element (html:li)
    (with-element (html:p)
      (make-instance 'pcdata :text person))))

(define-rule transform-play (and (bind node (element tei:pgroup))
				 (bind descr
				       (text (child-element tei:grpdescr))))
  (with-element (html:li)
    (with-element (html:ul)
      (map-children #'transform-play node))
    (with-element (html:p)
      (make-instance 'pcdata :text descr))))
      
(define-rule transform-play (path tei:pgroup tei:grpdescr)
  nil)

(define-rule transform-play t
  nil)

;;; Driver code

(defun transform-file (from to)
  (let ((play-xml (parse-xml-from-file from 'tei:tei)))
    (with-open-file (output to :direction :output)
      (with-xml-output (output :xml-decl nil)
	(transform-play play-xml)))))
