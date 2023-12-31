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
  (str:replace-using (list
		      "\\endcode" (format NIL "~%```")
		      "\\code" (format NIL "~%```lisp"))
		     text))

(defun get-clean-code-block (code-block)
  (str:trim
   (aref
    (nth-value
     1
     (ppcre:scan-to-strings *tex-code-block* code-block))
    0)))

(defun get-clean-code-block-simple (code-block)
  (str:trim
   (subseq code-block
	   (if (search "\\code" code-block)
	       (search "\\code" code-block)
	       0)
	   (if (search "\\endcode" code-block)
	       (search "\\endcode" code-block)
	       (length code-block)))))
  
(defun get-code-block-regex-experimental (code-block)
  (coerce
   (loop for char across (ppcre:quote-meta-chars (get-clean-code-block code-block))
	 when (ppcre:scan "\\s" (format nil "~A" char))
	   collect #\[ and collect #\\
	   and collect #\n
	   and collect #\\ 
	   and collect #\s
	   and collect #\] and collect #\*
	 else collect char)
   'string))

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
	 else when (eq #\? char)
		collect #\\ and collect #\?
	 else when (eq #\+ char)
		collect #\\ and collect #\+
	 else when (eq #\→ char)
		collect #\\ and collect #\→
	 ;; todo include other special characters
	 else collect char)
   'string))

(defun get-code-block-simple-regex (code-block)
  (if (< (length code-block) 50)
      (get-code-block-regex code-block)
      (concatenate 'string
		   (get-code-block-regex (subseq code-block 0 25))
		   "[.\\s\\n]*?"
		   (get-code-block-regex (subseq code-block
						 (- (length code-block) 25)
						 (length code-block))))))

(defun find-md-code-block (md-text code-block)
  ;; get-code-block-simple-regex
  (ppcre:scan (get-code-block-regex code-block) md-text))

(defun replace-md-code-block (md-text code-block)
  (multiple-value-bind (start end) (find-md-code-block md-text code-block)
    (concatenate 'string
		 (subseq md-text 0 start)
		 (tex-to-md-code code-block)
		 (subseq md-text end))))

;; TODO preprocess all the TeX files to account for the pandocs transformations to MD
;; \\EV\\s+[^\\s]+ becomes *→()*

(defun pre-process-tex-text (tex-text)
  ;; replace "&#60;" with "<" and ">" and also "{" and "}" and see main.lisp for anything else
  ;; try to work with https://plaster.tymoon.eu/edit/4022?password#
  ;; just the text, not the whole file
  ;; until it works, then do the rest... maybe it get's stuck because of those special values...
  (let* ((curr-text tex-text)
	 (tex-arrow-true "\\\\EV \\\\term\\{true\\}")
	 (md-arrow-true "*→ true*")
	 (tex-arrow-false "\\\\EV \\\\term\\{false\\}")
	 (md-arrow-false "*→ false*")
	 (tex-arrow "\\\\EV")
	 (md-arrow "*→*")
	 (tex-true "\\\\term\\{true\\}")
	 (md-true "true")
	 (tex-false "\\\\term\\{false\\}")
	 (md-false "false")
	 ;(md-left-carret "&#60;")
	 ;(cons "\\{"    "&#123;")
	 ;(cons "\\}"    "&#125;")
	 ;(cons "{"    "&#123;")
	 ;(cons "}"    "&#125;")
	 ;(cons "\\<"    "&#60;")
	 ;(cons "<"    "&#60;")
	 ;(cons "\\>"    "&#62;")
	 ;(cons ">"    "&#62;")

	 ;(tex-left-carret)
	 (all-replacements (list
			    (cons tex-arrow-true md-arrow-true)
			    (cons tex-arrow-false md-arrow-false)
			    (cons tex-arrow md-arrow)
			    (cons tex-true md-true)
			    (cons tex-false md-false)
			    (cons "\\{"    "&#123;")
			    (cons "\\}"    "&#125;")
			    (cons "{"    "&#123;")
			    (cons "}"    "&#125;")
			    (cons "\\<"    "&#60;")
			    (cons "<"    "&#60;")
			    (cons "\\>"    "&#62;")
			    (cons ">"    "&#62;")			    
			    )))
  (mapcar (lambda (x)
	    (setf curr-text
		  (ppcre:regex-replace-all
		   (car x) curr-text (cdr x))))
	  all-replacements)
    curr-text))

(defun build-tex-code-blocks-list ()
  (apply #'append
	 (mapcar (lambda (x) (find-code-blocks (pre-process-tex-text
						(alexandria-2:read-file-into-string x))))
		 (remove-if-not
		  (lambda (it)
		    (str:suffixp (list (namestring it)) ".tex"))
		  (uiop:directory-files
		   (asdf:system-relative-pathname "process-dpans3r" "tex-files/"))))))

(defun transform-file-code-blocks (filepath code-blocks-list)
  (format T "~%Processing: ~A~%" filepath)
  (format T "Code Block Quantity: ~A" (length code-blocks-list))
  (let ((md-text (alexandria-2:read-file-into-string filepath))
	(used-code-blocks NIL))
    (loop for code-block in code-blocks-list
	  when (find-md-code-block md-text code-block)
	    do (progn
;		 (format T "Found Code Block: ~A~%" code-block)
		 (setf md-text (replace-md-code-block md-text code-block))
		 (push code-block used-code-blocks))
	  finally
	     (format T "~%Replaced ~A code blocks" (length used-code-blocks))
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
		     (asdf:system-relative-pathname "process-dpans3r" "md-files/")))))
    (format T "Processing ~A Files" (length file-list))
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

(defun replace-html-to-normal-chars (given-string)
  (str:replace-using
   '( "&#123;" "{"
     "&#125;" "}"
     "&#60;" "<"
     "&#62;" ">"
     )
   given-string))

(defun process-tx-code (text)
  (tex-to-md-code (replace-html-to-normal-chars text)))

(defun save-code-block-list ()
  (str:to-file (asdf:system-relative-pathname
		    "process-dpans3r"
		    "output/code-blocks.md")
	       (format NIL "~{~A~%~%~}"
		       (mapcar #'process-tx-code (build-tex-code-blocks-list)))))

(defun save-json-code-blocks ()
  (str:to-file (asdf:system-relative-pathname
		    "process-dpans3r"
		    "output/code-blocks.json")
	       (com.inuoe.jzon:stringify
		(mapcar
		 (lambda (x)
		   (list x (get-code-block-regex x)))
		 (build-tex-code-blocks-list)))))
