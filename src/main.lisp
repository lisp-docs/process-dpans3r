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
(defvar *chapter-subsections* "\\n\\*\\*\\d+\\.\\d+\\.\\d+[\\w\\s]+?\\*\\*\\s*\\n")
(defvar *chapter-subsubsections* "\\n\\*\\*\\d+\\.\\d+\\.\\d+\\.\\d+[\\w\\s]+?\\*\\*\\s*\\n")

(defun get-text-parts (given-string split-regex)
  (let*
      ((chapter-positions (cl-ppcre:all-matches split-regex given-string))
       (first-section (subseq given-string 0 (first chapter-positions))))
    (cons first-section (loop for x in chapter-positions
	  for y from 0
	  when (and (evenp y) (not (equal y (- (length chapter-positions) 2))))
	    collect (subseq given-string x (nth (+ 2 y) chapter-positions))
	  when (and (evenp y) (equal y (- (length chapter-positions) 2)))
	    collect (subseq given-string x)))))

(defun get-chapter-sections (given-string)
  (get-text-parts given-string *chapter-sections*))

(defun get-chapter-subsections (given-string)
  (get-text-parts given-string *chapter-subsections*))

(defun get-chapter-subsubsections (given-string)
  (get-text-parts given-string *chapter-subsubsections*))


(defun remove-empty-strings (l)
  (remove-if 'empty-string-p l))

(defun split (s)
  (cl-ppcre:split 
   *chapter-sections*
   s
   :with-registers-p t))

(defun get-filepath (filename)
  (merge-pathnames *md-dir* filename))

(defun load-ch2 ()
  (setf *c2*
	(alexandria-2:read-file-into-string
	 (get-filepath "chap-2.md"))))

(defvar *c2* (load-ch2))

(defun save-file (filename string-data)
  (str:to-file (get-filepath filename) string-data))

(defun save-ch2 ()
  (save-file "chap-2.md" *c2*))

(defun replace-bracket-open (given-string)
  (str:replace-all "{" "\\{" given-string))

(defun replace-bracket-close (given-string)
  (str:replace-all "}" "\\}" given-string))

(defun replace-brackets (given-string)
  (replace-bracket-close
   (replace-bracket-open given-string)))

(defun replace-html-chars (given-string)
  (str:replace-using
   '("{" "\\{"
     "}" "\\}"
     "<" "\\<"
     ">" "\\>"
     )
   given-string))

(defun process-ch2 ()
  (load-ch2)
  (setf *c2* (replace-html-chars *c2*))
  (save-ch2))

;; Loop through directory and get all .md files
;; For each file
;;   create a dir called name minues .md
;;   create a file called file (including the .md) in same directory as the new directory
;;   in that file add the contents of the very first subsection AKA X before the X.1
;;   then for each sub section add both a folder and a file with the name of that subsection
;;     inside that folder, create a file for each subsubsection and fill it
;;     in the description file of the subsection add import statements in the very top
;;     `import Hide from './_hide.md';` for each component, need to change dots to dashes
;;      and need to remove spaces, title case, etc...
;;      need to add a category, description not needed, but label, position?,
;;    add the first file as an introduction maybe...
;;   need to add headings where the files are imported, and then import them

;; eventually will need to do the linking...

;; ensure-directories-exist

;; make output dir variable...




(defun get-output-dir ()
  (asdf:system-relative-pathname "html-to-md" "output/"))

(defun get-directory-for-chapter (filename)
  (uiop:merge-pathnames*
   (get-dir-name-for-file (filename-from-pathname filename))
   (get-output-dir)))

(defun get-dir-name-for-file (filename)
  (if (uiop:string-suffix-p filename ".md")
      (subseq filename 0 (- (length filename) 3))))

(defun filename-from-pathname (pathname)
  (car (last (str:split "/" (namestring pathname)))))

(defun process-file (filepath)
  (format T "~A~%" filepath)
  (format T "~A~%~%" (get-directory-for-chapter filepath)))

(defun process-files-in-dir (dir-path)
  (mapcar #'process-file
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))

(process-files-in-dir *md-dir*)
