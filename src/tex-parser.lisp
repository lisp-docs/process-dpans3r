(defpackage process-dpans3r.tex-parser
  (:use :cl)
  (:import-from :alexandria-2 )
  (:import-from  :cl-ppcre)
  (:import-from :str)
  (:import-from :uiop)
  (:import-from :process-dpans3r
		:modify-md-files-in-dir))
(in-package :process-dpans3r.tex-parser)


(defun get-tex-files ()
  (remove-if-not
   (lambda (it)
     (str:suffixp (list (namestring it)) ".tex"))
   (uiop:directory-files
    (asdf:system-relative-pathname "process-dpans3r" "tex-files/"))))

(defun find-tex-formats (text)
  (ppcre:all-matches-as-strings "\\\\[^\\s\\\\]*" text))

(defun get-all-tex-formats ()
  (let* ((formats (make-hash-table))
	 (add-formats (lambda (format-list)
			(mapcar (lambda (curr-format)
				  (format T "Format: ~A~%" curr-format)
				  (setf (gethash curr-format formats) T))
				format-list))))
    (mapcar (lambda (x)
	      (funcall add-formats
	       (find-tex-formats
	       (alexandria-2:read-file-into-string x))))
	    (get-tex-files))
    (hash-table-count formats)))
