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
(defvar *chapter-sections* "\\*\\*(\\d+|A)\\.\\d+ [\\W\\w\\s]+?\\*\\*\\s*")
(defvar *chapter-subsections* "\\*\\*(\\d+|A)\\.\\d+\\.\\d+ [\\W\\w\\s]+?\\*\\*\\s*")
(defvar *chapter-subsubsections* "\\*\\*(\\d+|A)\\.\\d+\\.\\d+\\.\\d+ [\\W\\w\\s]+?\\*\\*\\s*")
(defvar *all-subsections* "\\*\\*(\\d+|A)(\\.\\d+)* [\\W\\w\\s]+?\\*\\*")

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
      (str:concat (subseq filename 0 (- (length filename) 3)) "/")))

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

(defun get-context-for-match (start end text)
  (subseq text
	  (max 0 (- start 20))
	  (min (length text) (+ end 20))))

(defun get-contexed-matches (regex text)
  (for (start end) in (cl-ppcre:all-matches regex text)
       collect (get-context-for-match start end text)))

(defun get-title-folder-name (title)
  ;; convert from title name 2.1 Section Name to section-name
  )

(defun make-doc-category-json (label &optional position description)
  ;; TODO test the string, then wrap this into a write to file like
  ;; TODO   I did above str:to-file or something, and need a path,
  ;; TODO   make a constant called category-filename "_category_.json"
  (with-output-to-string (s)
    (format s "{~%  'label': '~A',~%" label)
    (if position (format s"  'position': ~A,~%" position))
    (format s "  'link': {~%    'type': 'generated-index',~%    ")
    (format s "'description': '~A'~%  }~%}~%" (if description description label))))

(defvar *category-filename* "_category_.json")

(defun ensure-slash-ends-path (pathname)
  (if
   (eq #\/ (char (namestring pathname) (- (length (namestring pathname)) 1)))
   pathname
   (concatenate 'string (namestring pathname) #\/)))

(defun create-docusaurus-doc-section (output-dir label &optional position)
  (let* ((folder-name (get-title-folder-name label))
	 (dir-path (uiop:merge-pathnames*
		    (ensure-slash-ends-path folder-name)
		    (ensure-slash-ends-path output-dir)))
	 (category-path (merge-pathnames
			 (ensure-slash-ends-path *category-filename*)
			 (ensure-slash-ends-path dir-path))))
  ;; create folder
    (ensure-directories-exist (ensure-slash-ends-path dir-path))
    ;; create _category_.json
    (str:to-file category-path (make-doc-category-json label))))

  ;; create _intro.md
  ;; create intro.md (which includes _intro) `import Intro from './_intro.md';`
  ;; Make headings in intro.md with the label as `##` label on the top of the file
  ;; Then call #`mapcar on the sections (cdr contents) assuming contents is a list

(defun get-section-meta-contents (meta-contents-list)
  ;; should receive a list of (subsection-title, subsection-filename)
  ;; and with with-output-to-string to produce a md file content that has
  ;; section title
  ;; import statements for each subsection, then a title of appropiate depth for
  ;; that subsection, and then the React MDX component imported and displayed
  ;; for that subsection under it's respective heading
  )

(defun process-subsection ()
  ;; should basically be creating a file with the contents given
  ;; and returning the cons of (subsection-title subsection-filename)
  )

(defun get-all-subsections (given-string)
  ;; TODO
  (get-text-parts given-string *all-subsections*))

(defun process-section (output-dir section-text)
  ;; get from regex the section title
  ;; get the section-dir from merging output dir and get dir for title
  ;; create file in output dir named dir-for-title .md
  ;; create folder and _category_.json
  ;; get section parts and subparts
  ;; create  _intro.md file
  ;; mapcar to each subsection process-subsection
  ;; get from that a list of (filename title)
  ;; in dir-for-title.md file created, import all created files from
  ;;   dir-for-title/_subsection.md including _intro.md, and add them
  ;;   below a heading with the title and the heading level based on the
  ;;   level of the title
  ;;   on the top of the file add a # h1 heading with the section title
  ;;   create _subsection_examples.md file for each subsection, and import
  ;;   them into the file as well adding an Examples subtitle based
  ;;   on the previous level's, just one more? better don't or do conditional
  ;;   rendering for an example page that is empty not to display even the title...

  ;; create file for section
  ;; create hidden folder for section
  ;; find every sub section, independent of depth
  ;; create a file for each one
  ;; then import it and set a heading of corresponding depth (to the depth of the section)
  ;; and display it
  (let* ((section-title (get-section-title section-text))
	 (folder-name (get-title-folder-name label))
	 (section-dir-path (uiop:merge-pathnames*
			    (ensure-slash-ends-path folder-name)
			    (ensure-slash-ends-path output-dir)))
	 (section-filename
	   (uiop:merge-pathnames*
	    (concatenate 'string folder-name ".md")
	   (ensure-slash-ends-path output-dir))))
    (create-docusaurus-doc-section output-dir section-title)
	 (str:to-file
	   section-filename
	   (get-section-meta-contents
	    (mapcar
	     (lambda (x) (process-subsection section-dir-path x))
	     (get-all-subsections section-text))))))

;; TODO need to find code snippets and wrap them in `` or ```lisp ```
;; cases are
;; (code)
;; ;comment
;; (code) ; comment
;; (code) → result
;; '\\w+ → \\w+
;; "\\w+ → (\\w+)
;; I think anything that containes this character → except probably one place
;; where they discuss the function of certain characters...
;; see 2.4.4.1 to see if multiline code was parsed correctly...
;; 2.4.4.2.5
;; need to edit the tables...
;; 2.4.8.3 shar sign expressions...

;; (uiop:merge-pathnames*
;;    (get-dir-name-for-file (filename-from-pathname filename))
;;    (get-output-dir)))

(defun get-chapter-label (text)
  (cl-ppcre:scan-to-strings "((\\d+|A)\\. [^\\*]+)"
   (cl-ppcre:scan-to-strings "\\*\\*((\\d+|A)\\. [\\s\\w\\W]+?)\\*\\*" text)))

(defun process-chapter (filename)
  (let* ((chapter-name
	   (get-dir-name-for-file
	    (filename-from-pathname filename)))
	 (chapter-dir (get-directory-for-chapter filename))
	 (chapter-text (load-file filename))
	 (label (get-chapter-label chapter-text))
	 (category-path (uiop:make-pathname* *category-filename* chapter-dir))
	 (section-list (get-chapter-sections chapter-text)))
    (ensure-directories-exist chapter-dir)
    (str:to-file category-path (make-doc-category-json label))
    (mapcar (lambda (x) (process-section chapter-dir x)) section-list)))

;; make function to replace symbols missing
;; *hh ii* *.*

(defun process-unicode-charachters (text)
  ;; https://graphemica.com/%E2%96%B7
  (replace-strings text '(("*hh" "&#10216;") ("ii*" "&#10217;") ("*.*" "&#9655;"))))

(defun process-titles (contents)
  (cl-ppcre:regex-replace-all
   "[^\\n]\\*\\*(\\d+|A)(\\.\\d+)*(\\s*\\w)+\\*\\*"
   contents
   (concatenate 'string '(#\Newline) "\\&" '(#\Newline))))

(defun replace-strings (contents string-cons-list)
  (loop for (curr-string replacement) in string-cons-list
	for curr-contents = (str:replace-all curr-string replacement contents)
	  then (str:replace-all curr-string replacement curr-contents)
	finally (return curr-contents)))

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

(defun update-md-files (update-function)
  (modify-md-files-in-dir
   *md-dir*
   (lambda (filepath)
     (str:to-file
      filepath
      (funcall update-function (load-file filepath))))))

;; replace unicode characters
;; (update-md-files #'process-unicode-charachters)

(defun process-files-in-dir (dir-path)
  (mapcar #'process-file
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))

;;(process-files-in-dir *md-dir*)
;(format T "~A~%" *load-pathname*)
;(format T "~A~%" *load-truename*)
