(defpackage process-dpans3r.dictionary-parser
  (:use :cl :process-dpans3r)
  (:import-from :alexandria-2 )
  (:import-from  :cl-ppcre)
  (:import-from :str)
  (:import-from :cl-fad)
  (:import-from :uiop))
(in-package :process-dpans3r.dictionary-parser)

(defun process-md-files-recursively (dir-path)
  (mapcar #'process-file
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))

(defun get-files-recursively (directory)
  (cl-fad:walk-directory directory
			 (lambda (name)
			   (if (search ".md" (format NIL "~A" name))
			       (format t "~A~%" name)))
			 :directories t))

(defun get-md-output-files ()
  (get-files-recursively
   (uiop:merge-pathnames*
    "output/" (asdf:system-source-directory :process-dpans3r))))
