(defpackage process-dpans3r.code-blocks
  (:use :cl)
  (:import-from :alexandria-2 )
  (:import-from  :cl-ppcre)
  (:import-from :str)
  (:import-from :uiop)
  (:import-from :process-dpans3r
		:modify-md-files-in-dir)
  (:export :*headers-regex*
           :*headers-non-regex*
           :*c0*))
(in-package :process-dpans3r.code-blocks)

(defvar *tex-code-block* "\code[\\s\\S\\n.]*?\endcode")
(setf *tex-code-block* "\\code([\\w\\W\\s\\S\\n.]*?)\endcode")

(defvar *tex-sample* (alexandria-2:read-file-into-string (asdf:system-relative-pathname "process-dpans3r" "tex-files/concept-definitions.tex")))

;; TODO which file to load...
(defvar *md-sample* (alexandria-2:read-file-into-string (asdf:system-relative-pathname "process-dpans3r" "md-files/chap-1.md")))

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
	 else when (eq #\[ char)
		collect #\\ and collect #\[
	 else when (eq #\] char)
		collect #\\ and collect #\]
	 else when (eq #\{ char)
		collect #\\ and collect #\{
	 else when (eq #\} char)
		collect #\\ and collect #\}
	 else when (eq #\. char)
		collect #\\ and collect #\.
	 else when (eq #\* char)
		collect #\\ and collect #\*
	 else when (eq #\+ char)
		collect #\\ and collect #\+
	 ;; todo include other special characters
	 else collect char)
   'string))

(defun find-md-code-block (md-text code-block)
  (ppcre:scan (get-code-block-regex code-block) md-text))

(defun replace-md-code-block (md-text code-block)
  (multiple-value-bind (start end) (find-md-code-block md-text code-block)
    (concatenate 'string
		 (subseq md-text 0 start)
		 (tex-to-md-code code-block)
		 (subseq md-text end))))

;; Open all tex files, make list of concatenated lists of all find-code-blocks in each file
;; then go through each md file, check each code-block, if matches, remove from list, and replace
;; save files...

(defun build-tex-code-blocks-list ()
  (mapcar (lambda (x) (find-code-blocks (alexandria-2:read-file-into-string x)))
	  (remove-if-not
	   (lambda (it)
	     (str:suffixp (list (namestring it)) ".tex"))
	   (uiop:directory-files
	    (asdf:system-relative-pathname "process-dpans3r" "tex-files/")))))

(defun transform-file-code-blocks (filepath)
  (str:to-file
   filepath
   (remove-headers (load-file filepath))))

(defun process-files ()
  (modify-md-files-in-dir
   (asdf:system-relative-pathname "process-dpans3r" "tex-files/")
   #'transform-file-code-blocks))

