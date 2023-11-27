(defpackage process-dpans3r
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
(in-package :process-dpans3r)


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
(defvar *chapter-sections-title* "\\*\\*(\\d+|A)\\.\\d+ [\\W\\w\\s]+?\\*\\*(?:(?!\\n)\\s)*")
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

(defun load-file (filepath)
  (alexandria-2:read-file-into-string filepath))

(defun save-file (filename string-data)
  (str:to-file (get-filepath filename) string-data))

(defun replace-bracket-open (given-string)
  (str:replace-all "{" "\\{" given-string))

(defun replace-bracket-close (given-string)
  (str:replace-all "}" "\\}" given-string))

(defun replace-brackets (given-string)
  (replace-bracket-close
   (replace-bracket-open given-string)))

(defun replace-html-chars (given-string)
  (str:replace-using
   '("\\{" "&#123;"
     "\\}" "&#125;"
     "{" "&#123;"
     "}" "&#125;"
     "\\<" "&#60;"
     "<" "&#60;"
     "\\>" "&#62;"
     ">" "&#62;"
;     "{" "\\{"
;     "}" "\\}"
;     "<" "\\<"
;     ">" "\\>"
     )
   given-string))

(defun get-output-dir ()
  (asdf:system-relative-pathname "process-dpans3r" "output/"))

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
  ;; (str:downcase
  ;;  (str:trim
  ;;   (str:join
  ;;    "-"
  ;;    (get-text-parts
  ;;     (get-mdx-component-name
  ;;      (get-section-title title))
  ;;     "[A-Z]"))
  ;;   :char-bag "-"))
  ;; (format T "get-title-folder-name~%" title)
  ;; (format T "~A~%" title)
  ;; (format T "~A~%" (str:downcase (get-react-component-name (string-trim '(#\^M #\ ) title))))
  (str:downcase
   (get-react-component-name
    (string-trim '(#\^M #\ ) title))))

(defun get-position-from-label (label)
  (let ((chapter-key (aref
		      (nth-value
		       1
		       (ppcre:scan-to-strings
			"\\*?\\*?(((\\d+|\\w)(\\.\\d+)*)[\\s\\w\\W\"“”]*)\\*?\\*?"
			label))
		      1)))
    (coerce
     (loop for char across "A.1"
	   when (equalp #\A char)
	     collect #\2 and collect #\7
	   else collect char)
     'string)))

(defun get-title-from-label (label)
  (let ((trimed-title
	  (str:trim 
	   (aref
	    (nth-value
	     1
	     (ppcre:scan-to-strings
	      "\\*?\\*?(((\\d+|\\w)(\\.\\d+)*)[ \\w\\W\"“”]*)\\*?\\*?"
	      label))
	    0))))
    (if (str:suffixp (list trimed-title) "**")
	(subseq trimed-title 0 (- (length trimed-title) 2))
	trimed-title)))

(defun make-doc-category-json (label &optional position description)
  ;; TODO test the string, then wrap this into a write to file like
  ;; TODO   I did above str:to-file or something, and need a path,
  ;; TODO   make a constant called category-filename "_category_.json"

  ;; TODO remove white space from label and *
  ;; TODO change from single to double quotes ' "
  ;; get position from label 
  (if (equal label NIL) ""
    (with-output-to-string (s)
      (format s "{~%  \"label\": \"~A\",~%" (get-title-from-label label))
      (if position
	  (format s"  \"position\": ~A,~%" position)
	  (format s"  \"position\": ~A,~%" (get-position-from-label label)))
      (format s "  \"link\": {~%    \"type\": \"generated-index\",~%    ")
      (format s "\"description\": \"~A\"~%  }~%}~%"
	      (if description description (get-title-from-label label))))))

(defvar *category-filename* "_category_.json")

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun ensure-slash-ends-path (pathname)
  (if
   (equal #\/ (char (namestring pathname) (- (length (namestring pathname)) 1)))
   pathname
   (let ((s (make-adjustable-string (namestring pathname))))
     (vector-push-extend #\/ s)
     s)))

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

(defun replace-integer-char-with-letter (x)
  (code-char (+ (char-code #\a) (- (char-code x) 48))))

(defun add-integer-char-to-letter (x base-char)
  (code-char (+ (char-code base-char) (- (char-code x) 48))))

(defun char-is-integerp (given-char)
  (and
   (>= (char-code given-char) 48)
   (<= (char-code given-char) 57)))

(defun char-is-letter (given-char)
  (or
   (and
    (>= (char-code given-char) 65)
    (<= (char-code given-char) 90))
   (and
    (>= (char-code given-char) 97)
    (<= (char-code given-char) 122))))

(defun replace-string-integers (given-string)
  (map 'string
       (lambda (x)
	 (if (char-is-integerp x)
	     (replace-integer-char-with-letter x)
	     x))
       given-string))

(defun get-react-component-name (title)
  ;; (let ((new-number T))
  ;;   (map 'string
  ;; 	 (lambda (x)
  ;; 	   (cond
  ;; 	     (((not (char-is-integerp x)) x)
  ;; 	      ((and new-number (char-is-integerp x)) (add-integer-char-to-letter x #\A))
  ;; 	      ((and (not new-number) (char-is-integerp x)) (add-integer-char-to-letter x #\a)))))
  ;; 	 title))
  (coerce
   (remove
    nil
    (loop for i from 1 below (length title)
	  collect (cond
		    ((equal #\. (char title i)) #\-)
		    ((equal #\  (char title i)) #\-)
		    ((and
		      (not (char-is-integerp (char title i)))
		      (not (char-is-letter (char title i))))
		     NIL)
		    ((not (char-is-integerp (char title i))) (char title i))
		    ((and (char-is-integerp (char title (1- i))) (char-is-integerp (char title i)))
		     (add-integer-char-to-letter (char title i) #\a))
		    ((and
		      (not (char-is-integerp (char title (1- i))))
		      (char-is-integerp (char title i)))
		     (add-integer-char-to-letter (char title i) #\A)))))
   'string))

(defvar *subsection-title* "(\\d+|\\w+)(\\.\\d+)* ([\"“”\\w]+\\s*)+")

(defun get-display-title (md-title)
  (first (cl-ppcre:all-matches-as-strings *subsection-title* md-title)))

(defun remove-title (section-text)
  (str:replace-first
   (get-subsection-title section-text)
   ""
   section-text))

(defun get-mdx-component-name (title)
  (str:replace-all
   " " ""
   (str:remove-punctuation
    (replace-string-integers (get-display-title title))))) ; React Component Names seem to fail with Numbers in them

(defun get-heading-depth (original-title)
  (- (length
      (str:split
       "."
       (first
	(cl-ppcre:all-matches-as-strings
	 "(\\d+|\\w+)(\\.\\d+)*"
	 original-title))))
     1))

(defun get-heading (original-title)
  ;; ,(format NIL "~v@{str~}" 10 T)
  (with-output-to-string (s)
    (format s "~A " (make-array
		     (get-heading-depth original-title)
		     :element-type 'character :initial-element #\#))
    (format s "~A~%" (get-display-title original-title))))

(defun get-section-meta-contents (meta-contents-list section-title folder-name)
  ;; should receive a list of (subsection-title, subsection-filename)
  ;; and with with-output-to-string to produce a md file content that has
  ;; section title
  ;; import statements for each subsection, then a title of appropiate depth for
  ;; that subsection, and then the React MDX component imported and displayed
  ;; for that subsection under it's respective heading
  (with-output-to-string (s)
    (format s "---~%")
    (format s "title: \"~A\"~%" (get-display-title section-title))
    (format s "---~%~%")
    (loop for (title . filename) in meta-contents-list
	  do (format s "~A~%" (get-heading title))
	  do (format s "import ~A from './~A/_~A.md';~%~%"
		     (str:replace-all
		      "-" ""
		      (get-react-component-name title))
		     folder-name
		     (get-title-folder-name title))
	  do (format s "<~A />~%~%"
		     (str:replace-all
		      "-" ""
		      (get-react-component-name title))))))

(defun process-subsection (section-dir-path subsection-text)
  ;; TODO
  ;; should basically be creating a file with the contents given
  ;; and returning the cons of (subsection-title subsection-filename)
  (let* ((subsection-title (get-subsection-title subsection-text))
	 (subsection-filename (concatenate 'string "_" (get-title-folder-name subsection-title)))
	 (subsection-filepath
	   (uiop:merge-pathnames*
	    (concatenate 'string subsection-filename ".md")
	    (ensure-slash-ends-path section-dir-path))))
    (str:to-file subsection-filepath (remove-title subsection-text))
    (cons subsection-title subsection-filename)))

(defun get-subsection-title (subsection-text)
  (first (cl-ppcre:all-matches-as-strings *all-subsections* subsection-text)))

(defun get-all-subsections (given-string)
  (remove-if
   (lambda (x) (or (equal x NIL) (equal x "")))
   (get-text-parts given-string *all-subsections*)))

(defun get-section-title (section-text)
  (let ((match (cl-ppcre:all-matches-as-strings *chapter-sections-title* section-text)))
    (if match (first match) NIL)))

(defun process-chapter-intro (chapter-dir intro-text)
  (let* ((section-filename
	   (uiop:merge-pathnames*
	    "intro.md"
	    (ensure-slash-ends-path chapter-dir))))
;    (create-docusaurus-doc-section chapter-dir "Introduction")
    (str:to-file
     section-filename
     intro-text)))

(defun process-section (output-dir section-text)
  (declaim (optimize (debug 3)))
  ;; (format T "proess-section~%")
  ;; (format T "~A~%" (get-section-title section-text))
  ;; (format T "~A~%" (get-title-folder-name (get-section-title section-text)))
  ;; (format T "~A~%" output-dir)
  ;; (format T "~A~%" (ensure-slash-ends-path
  ;; 		    (get-title-folder-name (get-section-title section-text))))
  ;; (format T "~A~%" (ensure-slash-ends-path output-dir))
  (let* ((section-title (get-section-title section-text))
	 (folder-name (get-title-folder-name section-title))
	 (section-dir-path (uiop:merge-pathnames*
			    (ensure-slash-ends-path folder-name)
			    (ensure-slash-ends-path output-dir)))
	 (section-filename
	   (uiop:merge-pathnames*
	    (concatenate 'string folder-name ".md")
	    (ensure-slash-ends-path output-dir))))
    (create-docusaurus-doc-section output-dir section-title)
;    (break )
    (str:to-file
     section-filename
     (get-section-meta-contents
      (mapcar
       (lambda (x) (process-subsection section-dir-path x))
       (get-all-subsections section-text))
      section-title folder-name))))

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
	 (category-path (uiop:merge-pathnames* *category-filename* chapter-dir))
	 (section-list (get-chapter-sections chapter-text)))
    (ensure-directories-exist chapter-dir)
    (str:to-file category-path (make-doc-category-json label))
    (process-chapter-intro chapter-dir (car section-list))
    (mapcar (lambda (x) (process-section chapter-dir x)) (cdr section-list))))

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

  ;; (format T "~A~%" filepath)
  ;; (format T "~A~%~%" (get-directory-for-chapter filepath))
  )

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

(defun slice-files-from-dir-to-output (dir-path)
  (mapcar #'process-chapter
	  (remove-if-not
	   (lambda (it)
	     (search ".md" (namestring it)))
	   (uiop:directory-files dir-path))))


;;(process-files-in-dir *md-dir*)
;(format T "~A~%" *load-pathname*)
;(format T "~A~%" *load-truename*)

; (slice-files-from-dir-to-output #P"./process-dpans3r/md-files/")

;; TODO
;; Need to replace "{" with "&#123;"
