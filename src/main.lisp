(defpackage html-to-md
  (:use :cl)
  (:import-from :alexandria )
  (:import-from  :cl-ppcre)
  (:import-from :uiop)
  (:export :hello
           :load-md-file
           :*curr-file*
           :*md-dir*
           :*regex-strings*
           :*non-regex-strings*
           :*c0*))
(in-package :html-to-md)

;; blah blah blah.

(defvar *curr-file* NIL)

(defun hello ()
  (format T "~A%S" *load-truename*)
  (format T "hello world!"))

;(ql:quickload "cl-ppcre")

;(ql:quickload "alexandria")


(defun load-md-file (filepath)
  (setf *curr-file* (alexandria-2:read-file-into-string filepath)))
  ;(alexandria-2:read-file-into-string filepath))
  
(defvar *md-dir* #P"/Users/danielnussenbaum/Development/projects/lisp/roswell-projects/html-to-md/md-files/")

(defvar *c0* "chap-0\\\chap-0.md")

(uiop:directory-files *md-dir*)

(defvar *non-regex-strings* 
  '("Version 15.17R, X3J13/94-101R." "Fri 12-Aug-1994 6:35pm EDT"))

(defvar *regex-strings* 
  '( "\n\w+(\s+\w+)* [xvi]+\n" 
    "[xvi]+ Programming Language—Common Lisp"
    "\n\w+(\s+\w+)* \d+(–\d+)*\n"
    "\d+(–\d+)* Programming Language—Common Lisp"))


;(defvar *chapter-sections* "\\n\\d\\+.\\d+.*?\\n")
;(defvar *chapter-sections* "\\n**\\d+\\.\\d+.*?**\\n")
(defvar *chapter-sections* "\\n\\*\\*\\d+\\.\\d+[\\w\\s]+?\\*\\*\\s*\\n")

(defun remove-empty-strings (l)
  (remove-if 'empty-string-p l))

(defun split (s)
  (cl-ppcre:split 
   *chapter-sections*
   s
   :with-registers-p t))

(defun get-filepath (filename)
  (merge-pathnames *md-dir* filename))

(defvar *c2* (load-md-file (get-filepath "chap-2.md")))
