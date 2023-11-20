(defpackage html-to-md
  (:use :cl)
  (:import-from :alexandria )
  (:import-from  :cl-ppcre)
  (:import-from :uiop)
  (:export :hello
           :load-md-file
           :*curr-file*
           :*md-dir*
           :*headers-regex*
           :*headers-non-regex*
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

(defvar *headers-non-regex*
  '("Version 15.17R, X3J13/94-101R."
    "**Programming Language—Common Lisp**"
    "Fri 12-Aug-1994 6:35pm EDT"))

(defvar *headers-regex*
  '( "\\n\w+(\\s+\\w+)* [xvi]+\\n"
    "\\n\\w+(\\s+\\w+)* \\*\\*[xvi]+\\*\\*\\s*" 
    "[xvi]+ Programming Language—Common Lisp"
    "\\*\\*[xvi]+\\*\\* Programming Language—Common Lisp"
    "\\n\\w+(\\s+\w+)* \\d+(–\\d+)*\\n"
    "\\n\\w+(\\s+\\w+)* \\*\\*\\d+(–\\d+)*\\*\\*\\n"
    ;; this didn't catch what I wanted in the appendix
    "\\n\\w+(\\s+\w+)* \\w+(–\\d+)*\\n"
    ;; this didn't catch what I wanted in the appendix
    "\\n\\w+(\\s+\\w+)* \\*\\*\\w+(–\\d+)*\\*\\*\\n"
    "\\d+(–\\d+)* Programming Language—Common Lisp"
    "\\*\\*\\d+(–\\d+)*\\*\\* Programming Language—Common Lisp"
    "\\w+(–\\d+)* Programming Language—Common Lisp"
    "\\*\\*\\w+(–\\d+)*\\*\\* Programming Language—Common Lisp"))


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

(defun load-file (filepath)
  (alexandria-2:read-file-into-string filepath))

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

;; Loop through directory and get all .md files
;; For each file
;;   create a dir called name minues .md
;;    replace html chars...
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

;; make output dir variable...

;; TODO make function to create folder and save introduction file
;; TODO then make function to process each of the chapter's section
;; Will need to get subsections, if none, then create file and that's it
;; If there are subsections, then will need to create a folder for each one
;;   and again, similarly to the approach taken, will need to create an introduction
;;   file, and then for each subsubsection create a file with those contents
;; here will need to create the files with a "_" prefix

(defun get-title-folder-name (title)
  ;; convert from title name 2.1 Section Name to section-name
  )

(defun create-docusaurus-doc-section (label position contents)
  ;; create folder
  ;; create _intro.md
  ;; create intro.md (which includes _intro) `import Intro from './_intro.md';`
  ;; Make headings in intro.md with the label as `##` label on the top of the file
  ;; Then call #`mapcar on the sections (cdr contents) assuming contents is a list
  )

(defun process-titles (contents)
  (cl-ppcre:regex-replace-all
   "[^\\n]\\*\\*\\d+(\\.\\d+)*(\\s*\\w)+\\*\\*"
   contents
   (concatenate 'string '(#\Newline) "\\&" '(#\Newline))))

(defun remove-strings (contents string-list)
  (loop for curr-string-to-replace in string-list
	;; for curstr = (replace-all-occurences mystring regexp replacement) 
	for curstr = (str:replace-all curr-string-to-replace "" contents)
	  then (str:replace-all curr-string-to-replace "" curstr)
	finally (return curstr)))

(defun remove-regex-list (contents regex-list)
  (loop for regexp in regex-list
	;; for curstr = (replace-all-occurences mystring regexp replacement) 
	for curstr = (cl-ppcre:regex-replace-all regexp contents  "")
	  then (cl-ppcre:regex-replace-all regexp curstr "")
	;; do (format T "~A~%" regexp)
	finally (return curstr)))

(defun remove-headers (contents)
  ;; remove here all the extra titles, headers, and footers...
  ;; TODO remove-regex-list not working
  (remove-strings
   (remove-regex-list contents *headers-regex*)
  *headers-non-regex*))

(defun process-file (filepath)
  (let* ((chapter-contents (load-file filepath))
	 (dir-path (get-directory-for-chapter filepath))
	 (md-escaped-chapter (replace-html-chars chapter-contents))
	 (processed-titles (process-titles md-escaped-chapter))
	 (removed-headers (remove-headers processed-titles))
	 (chapter-sections (get-chapter-sections md-escaped-chapter)))
    (ensure-directories-exist dir-path))
  ;; i should search here for all subsection or section titles which are not in a new
  ;;   line, so anything that's [^\\n]\\*\\*\\d+(.\\d+)*(\\s*\\w)+\\*\\*
  ;; (cl-ppcre:all-matches "[^\\n]\\*\\*\\d+(\\.\\d+)*(\\s*\\w)+\\*\\*" *c2*)
  ;; (cl-ppcre:regex-replace-all
  ;;  "[^\\n]\\*\\*\\d+(\\.\\d+)*(\\s*\\w)+\\*\\*"
  ;;  (first (cl-ppcre:all-matches-as-strings "[^\\n]\\*\\*\\d+(\\.\\d+)*(\\s*\\w)+\\*\\*" *c2*))
  ;;  (concatenate 'string '(#\Newline) "\\&" '(#\Newline)))
  ;; save-file

  (format T "~A~%" filepath)
  (format T "~A~%~%" (get-directory-for-chapter filepath)))

(defun escape-file-html (filepath)
  (str:to-file
   filepath
   (replace-html-chars (load-file filepath))))
 
(defun remove-file-headers (filepath)
  (str:to-file
   filepath
   (remove-headers (load-file filepath))))
 
(defun fix-file-titles (filepath)
  (str:to-file
   filepath
   (process-titles (load-file filepath))))

(defun modify-md-files-in-dir (dir-path modifier-function)
  (mapcar modifier-function
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))

(defun remove-headers-of-md-files ()
  (modify-md-files-in-dir *md-dir* #'remove-file-headers))

(defun escape-md-files ()
  (modify-md-files-in-dir *md-dir* #'escape-file-html))

(defun fix-titles-md-files ()
  (modify-md-files-in-dir *md-dir* #'fix-file-titles))

(defun process-files-in-dir (dir-path)
  (mapcar #'process-file
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))

;;(process-files-in-dir *md-dir*)
;(format T "~A~%" *load-pathname*)
;(format T "~A~%" *load-truename*)
