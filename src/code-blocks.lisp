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
(setf *tex-code-block* "\\\\code([\\w\\W\\s\\S\\n.]*?)\\\\endcode")

(defvar *tex-sample* (alexandria-2:read-file-into-string (asdf:system-relative-pathname "process-dpans3r" "tex-files/concept-definitions.tex")))

;; TODO which file to load...
(defvar *md-sample* (alexandria-2:read-file-into-string (asdf:system-relative-pathname "process-dpans3r" "md-files/chap-1.md")))

(defun find-code-blocks (text)
  (ppcre:all-matches-as-strings *tex-code-block* text))

(defun tex-to-md-code (text)
  (str:replace-using (list "\\endcode" "```" "\\code" "```lisp") text))

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
  (apply #'append
	 (mapcar (lambda (x) (find-code-blocks (alexandria-2:read-file-into-string x)))
		 (remove-if-not
		  (lambda (it)
		    (str:suffixp (list (namestring it)) ".tex"))
		  (uiop:directory-files
		   (asdf:system-relative-pathname "process-dpans3r" "tex-files/"))))))

(defun transform-file-code-blocks (filepath code-blocks-list)
  (let ((md-text (alexandria-2:read-file-into-string filepath))
	(used-code-blocks NIL))
    (loop for code-block in code-blocks-list
	  when (find-md-code-block md-text code-block)
	    do (progn
		 ;; add the replace-md-code-block... in the end save the file and return
		 ;; the list of used code-blocks, and remove them like I
		 ;; did in the REPL
		 ;; see lime chat for read and print discussion for saving unused code-blocks...
		 (setf md-text (replace-md-code-block md-text code-block))
		 (push code-block used-code-blocks))
	  finally
	     (str:to-file filepath md-text)
	     (return used-code-blocks))))

(defun save-lisp-data-to-file (data filepath)
  (with-open-file (f filepath
		     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (print data f)))

(defun read-lisp-data-from-file (filepath)
  (with-open-file (f filepath
		     :direction :input)
    (read f)))

(defun process-files ()
  (let ((code-blocks-list (build-tex-code-blocks-list))
	(file-list (remove-if-not
		    (lambda (it)
		      (search ".md" (namestring it)))
		    (uiop:directory-files
		     (asdf:system-relative-pathname "process-dpans3r" "tex-files/")))))
    (loop for filepath in file-list
	 do (mapcar
	     (lambda (x)
	       (setf code-blocks-list
		     (remove x code-blocks-list :count 1 :test #'equalp)))
	     (transform-file-code-blocks filepath code-blocks-list))
	  finally (save-lisp-data-to-file
		   code-blocks-list
		   (asdf:system-relative-pathname
		    "process-dpans3r"
		    "program-state/unused-code-blocks.lispdata")))))


