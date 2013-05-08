;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; package.cl --- Package definitions for CLiX
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: package.cl,v 1.4 2003/01/16 02:41:12 dent Exp $

(cl:in-package :CL)

;;;; %File Description:
;;;; 
;;;; This defines the CLiX main package, and a sample user package for
;;;; interactive exploratory work with CLiX.
;;;; 

(defpackage "CLIX"
    (:use "CL")
  (:nicknames "XML")
  (:export #:node #:node-parent #:node-children #:render-node
	   #:processing-instruction #:processing-instruction-target
	   #:processing-instruction-data
	   #:entity-reference #:entity-reference-name
	   #:char-reference #:char-reference-name
	   #:element #:element-gi #:element-p #:element-attributes
	   #:element-attribute
	   #:generic-element #:specialized-element
	   #:pcdata #:pcdata-text
	   #:with-element #:with-pcdata #:from-list-template #:with-xml-output
	   #:make-char-reference #:make-entity-reference
	   #:define-xml-element #:parse-xml-from-file
	   #:node-element-children #:find-element
	   #:find-elements #:element-pcdata
	   #:access-path #:select-path #:bind-xml-paths
	   #:element-matches-p #:child #:children #:attribute #:text
	   #:search-elements
	   #:set-xml-template-handler #:get-xml-template-handler
	   #:render-xml-template #:with-xml-template-output))

(defpackage "CLIX-USER"
    (:use "CL" "CLIX")
  (:nicknames "XML-USER"))
