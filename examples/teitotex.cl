;;;; CLiX --- Common Lisp (improves|incorporates|infiltrates|is) XML
;;;; This is copyrighted software.  See documentation for terms.
;;;; 
;;;; teitotex.cl --- Further Demonstration of Transformations on TEI XML
;;;; 
;;;; Checkout Tag: $Name:  $
;;;; $Id: teitotex.cl,v 1.1 2000/11/09 02:13:59 dent Exp $

(in-package :CL)

;;;; %File Description:
;;;; 
;;;; This file demonstrates how one would transform XML (TEI Play)
;;;; into LaTeX.
;;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "CLiX-Transform"))

;;; Set up specialized representations for the TEI Play subset we use.

(defpackage :tei (:use :CL :CLiX)
  (:export #:tei #:play #:title #:personae #:persona #:pgroup #:grpdescr
	   #:fm #:p #:scndescr #:playsubt
	   #:act #:scene #:stagedir #:speech #:speaker #:line))

(in-package :tei)

(defmacro define-simple-xml-elements (namespace supers &rest names)
  `(progn
     ,@(loop for name in names
	     collect
	     `(define-xml-element (,name ,(symbol-name name) ,namespace)
	          ,supers
	        ()))))

(define-simple-xml-elements tei ()
  play title personae persona pgroup grpdescr
  fm p
  scndescr playsubt
  act scene stagedir speech speaker line)

;;; The transformation code itself

(defpackage :CLiX-Demo (:use :CL :CLiX :CLiX-Transform)
  (:export #:transform-file))

(in-package :CLiX-Demo)

;;; Support utility functions

(defun map-children (fun node)
  (mapc fun (node-children node)))

(defun write-tex-escaped-string (string &optional (stream *standard-output*))
  "This behaves like `write-string', but escapes all TeX relevant characters."
  (declare (type string string) (type stream stream))
  (loop for char of-type character across string
	do
	(case char
	  ((#\$ #\&) (write-char #\\ stream) (write-char char stream))
	  (t
	   (write-char char stream)))))

(defun write-tex-escaped-line (string &optional (stream *standard-output*))
  "This behaves like `write-line', but escapes all TeX relevant characters."
  (write-tex-escaped-string string stream)
  (terpri stream))

;;; The transformation proper

;;; During the transformation, *standard-output* will be bound to the
;;; LaTeX output stream.

(define-transformation transform-play)

;;; Top-level transformation

(define-rule transform-play (path (node tei:play))
  (write-line "% LaTeX generated from XML TEI PLAY DTD by CLiX-Transform")
  (write-line "\\documentclass{article}")
  (write-line "\\begin{document}")
  (write-line "\\parindent 0pt")
  (map-children #'transform-play node)
  (write-line "\\end{document}"))

;;; Frontmatter stuff

(define-rule transform-play (bind title (text (path tei:play tei:title)))
  (write-string "\\title{")
  (write-tex-escaped-string title)
  (write-line "}"))

(define-rule transform-play (path (node tei:fm))
  (write-line "\\author{")
  (map-children #'transform-play node)
  (write-line "}")
  (write-line "\\maketitle{}")
  (write-line "\\newpage"))

(define-rule transform-play (bind text (text (path tei:fm tei:p)))
  (write-tex-escaped-string text)
  (write-line "\\\\"))

(define-rule transform-play (path (node tei:personae))
  (map-children #'transform-play node)
  (write-line "\\newpage"))

(define-rule transform-play (bind title (text (path tei:personae tei:title)))
  (write-string "\\section*{")
  (write-tex-escaped-string title)
  (write-line "}"))

(define-rule transform-play (bind name (text (path tei:personae tei:persona)))
  (write-string "{\\tt ")
  (write-tex-escaped-string name)
  (write-line "}\\\\")
  (terpri))

(define-rule transform-play (path (node tei:pgroup))
  (map-children #'transform-play node)
  (terpri))

(define-rule transform-play (bind name (text (path tei:pgroup tei:persona)))
  (write-string "{\\tt ")
  (write-tex-escaped-string name)
  (write-line "}\\\\"))

(define-rule transform-play (bind description (text (path tei:grpdescr)))
  (write-string "{\\it (")
  (write-tex-escaped-string description)
  (write-line ")}\\\\"))

;;; We get to the meat of the play.

(define-rule transform-play (path (node tei:act))
  (map-children #'transform-play node))

(define-rule transform-play (bind title (text (path tei:act tei:title)))
  (write-string "\\section*{")
  (write-tex-escaped-string title)
  (write-line "}"))

(define-rule transform-play (path (node tei:scene))
  (map-children #'transform-play node))

(define-rule transform-play (bind title (text (path tei:scene tei:title)))
  (write-string "\\subsection*{")
  (write-tex-escaped-string title)
  (write-line "}"))

(define-rule transform-play (bind direction (text (path tei:stagedir)))
  (write-string "\\centering{\\it [")
  (write-tex-escaped-string direction)
  (write-line "]}\\\\"))

(define-rule transform-play (path (node tei:speech))
  (write-line "\\begin{description}")
  (map-children #'transform-play node)
  (write-line "\\end{description}"))

(define-rule transform-play (bind speaker (text (path tei:speaker)))
  (write-string "\\item[")
  (write-tex-escaped-string speaker)
  (write-line ":] "))

(define-rule transform-play (bind line (text (path tei:line)))
  (write-string "\\hspace{1pt}")
  (write-tex-escaped-string line)
  (write-line "\\\\"))

(define-rule transform-play (bind dir (text (path tei:speech tei:stagedir)))
  (write-string "{\\it [")
  (write-tex-escaped-string dir)
  (write-line "]}\\\\"))

;;; Default rule for all other stuff (like e.g. PCDATA nodes, PI nodes, etc.)

(define-rule transform-play t
  nil)

;;; Driver code

(defun transform-file (from to)
  (let ((play-xml (parse-xml-from-file from 'tei:tei)))
    (with-open-file (*standard-output* to :direction :output)
      (transform-play play-xml))))
