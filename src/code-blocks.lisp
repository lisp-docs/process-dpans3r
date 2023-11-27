(defpackage process-dpans3r.code-blocks
  (:use :cl)
  (:import-from :alexandria )
  (:import-from  :cl-ppcre)
  (:import-from :str)
  (:import-from :uiop)
  (:export :*headers-regex*
           :*headers-non-regex*
           :*c0*))
(in-package :process-dpans3r.code-blocks)

(defvar *tex-code-block* "\code[\\s\\S\\n.]*?\endcode")
(setf *tex-code-block* "\code([\\w\\W\\s\\S\\n.]*?)\endcode")

(defvar *tex-sample* "It is a description of any of these:

\code
 (x y)
 (x B A C y)
 (x A B B B B B C y)
 (x C B A B B B y)
\endcode

\noindent but not any of these:

\code
 (x B B A A C C y)
 (x C B C y)
\endcode

\noindent In the first case, both {\tt A} and {\tt C} appear too often,
")

(defvar *md-sample* "
means that at most one A, any number of B’s, and at most one C can occur in any order. It is a description of any of these: 

(x y) 

(x B A C y) 

(x A B B B B B C y) 

(x C B A B B B y) 

but not any of these: 

(x B B A A C C y) 

(x C B C y) 

In the first case, both A and C appear too often, and in the second case C appears too often. 
")

(defun find-code-blocks (text)
  (ppcre:all-matches-as-strings *tex-code-block* text))

(defun tex-to-md-code (text)
  (str:replace-using (list "endcode" "```" "code" "```lisp") text))

(defun get-clean-code-block (code-block)
  (str:trim
   (aref
    (nth-value
     1
     (ppcre:scan-to-strings *tex-code-block* code-block))
    0)))

(defun get-code-block-regex (code-block)
  (coerce
   (loop for char across (get-clean-code-block code-block)
	 when (ppcre:scan "\\s" (format nil "~A" char))
					;	   collect #\\ and collect #\\ and collect #\s and collect #\*
	   collect #\[ and collect #\\
	   and collect #\n
	   and collect #\\ 
	   and collect #\s
	   and collect #\] and collect #\*
	 else when (eq #\\ char)
		collect #\\ and collect #\\
	 else when (eq #\( char)
		collect #\\ and collect #\(
	 else when (eq #\) char)
		collect #\\ and collect #\)
					;	 else when ()
	 ;; todo include []{}.*+ and other special characters
	 else collect char)
   'string))

(defun find-md-code-block (md-text code-block))

(str:replace-first "code" "```lisp" (first (find-code-block *tex-sample*)))

(ppcre:all-matches-as-strings "\code[\\n.\\w\\W]*\endcode" "
\code
 (x B B A A C C y)
 (x C B C y)
\endcode
")
