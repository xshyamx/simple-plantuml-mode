;;; plantuml-mode.el -- Major mode for plantuml diagrams -*- lexical-binding: t -*-

;; Author: xshyamx
;; Maintainer: xshyamx
;; Homepage: https://github.com/xshyamx/simple-plantuml-mode
;; Created: 28 Oct 2023
;; Version: 0.0.4
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, convenience

;; Define xref backend
(require 'cl-lib)
(require 'compile)
(require 'seq)
(require 'xref)
(require 'plantuml-colors)
(require 'plantuml-xref)
(require 'plantuml-keywords)

;;; Customizable variables
(defgroup plantuml-mode nil
  "Major mode for plantuml" :group
  'languages)

(defcustom plantuml-java-cmd "java"
  "Path to java executable"
  :type 'string
  :group 'plantuml)

(defcustom plantuml-jar-path "plantuml.jar"
  "Path to plantuml.jar file"
  :type 'string
  :group 'plantuml)

(defcustom plantuml-insert-basename-comment t
  "Insert basename comment when inserting the initial diagram template"
  :type 'boolean
  :group 'plantuml)

(defun plantuml--default-preview-program ()
  (pcase system-type
    ('windows-nt "msphotos")
    ('darwin "/usr/bin/open")
    ('ms-dos "mspaint")
    (_ "/usr/bin/eog")))

(defcustom plantuml-preview-program (plantuml--default-preview-program)
  "Program to use for opening preview files"
  :type 'string
  :options '(mspaint msphotos)
  :group 'plantuml)

(defcustom plantuml-force-save-before-preview t
  "Save file before generating preview"
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-show-failed-preview t
  "Show the failed preview image with error message"
  :type 'boolean
  :group 'plantuml)

(defcustom plantuml-close-compilation-buffer 5
  "Number of secods after which preview compilation buffer will be killed"
  :type 'integer
  :group 'plantuml)

;;; default mode map
(defvar plantuml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'plantuml-compile)
    (define-key keymap (kbd "C-c C-p") #'plantuml-preview)
    (define-key keymap (kbd "C-c C-o") #'plantuml-open-preview)
    (define-key keymap (kbd "C-c !") #'plantuml-select-diagram)
    (define-key keymap (kbd "C-c i") #'plantuml-insert-element)
    keymap))
(defvar plantuml-mode-hook nil "Standard mode hook for plantuml-mode")

;;; syntax table
(defvar plantuml-mode-syntax-table
  (make-syntax-table)
  "Syntax table for plantuml-mode")

(modify-syntax-entry ?\' "<" plantuml-mode-syntax-table)
(modify-syntax-entry ?\n ">" plantuml-mode-syntax-table)

;;; constants
(defconst plantuml--output-file-extensions '(png cmapx)
  "File extensions of files generated when exporting plantuml diagrams")

(defconst plantuml--deployment-elements
  '(component database rectangle artifact frame actor interface file
	      folder package agent boundary card circle cloud collections control
	      entity hexagon label node queue stack storage usecase)
  "Elements in a deployment diagram")

(defconst plantuml--sequence-elements
  '(participant actor database collections boundary control entity
		queue)
  "Elements in a sequence diagram")

;; diagram types
(defconst plantuml-diagram-types
  '(("u" "UML Diagram" "uml")
    ("m" "Mindmap" "mindmap")
    ("s" "Salt UI wireframe" "salt")
    ("g" "Gantt chart" "gantt")
    ("w" "Work breakdown structure" "wbs")
    ("r" "Regex diagram" "regex")
    ("c" "Chronology diagram" "chronology")
    ("j" "JSON" "json")
    ("y" "YAML" "yaml"))
  "Types of PlantUML diagrams")

(defconst plantuml-component-elements
  '(("c" "component")("d" "database")("a" "actor")("t" "artifact")("f" "file")
    ("d" "folder")("m" "frame")("i" "interface")("p" "package")("r" "rectangle")
    ("l" "collections")("q" "queue") ("b" "label")("n" "node")("v" "actor/")
    ("g" "agent")("y" "boundary")("g" "card")("o" "circle")("x" "cloud")
    ("z" "control")("e" "entity")("h" "hexagon")("k" "stack")("s" "storage")
    ("u" "usecase")("w" "usecase/"))
  "Elements for PlantUML component diagram")

;;; font-lock-keywords
(defconst plantuml--font-lock-components
  (list
   ;; regexp
   (regexp-opt plantuml--component-types)
   ;; font-face
   '(0 font-lock-type-face)))

(defconst plantuml--font-lock-diagrams
  (list
   ;; regexp
   (rx-to-string `(seq "a" (or "start" "end") (or ,@plantuml--diagram-types)))
   ;; font-face
   '(0 font-lock-builtin-face)))

(defconst plantuml--font-lock-preprocessor
  (list
   ;; regexp
   (rx-to-string `(seq "!" (or ,@plantuml--preprocessor-keywords)))
   ;; font-face
   '(0 font-lock-preprocessor-face)))

;;; font-lock-keywords
(defconst plantuml--font-lock-components
  (list
   ;; regexp
   (regexp-opt plantuml--component-types)
   ;; font-face
   '(0 font-lock-type-face)))

(defconst plantuml--font-lock-diagrams
  (list
   ;; regexp
   (rx-to-string `(seq "@" (or "start" "end") (or ,@plantuml--diagram-types)))
   ;; font-face
   '(0 font-lock-builtin-face)))

(defconst plantuml--font-lock-preprocessor
  (list
   ;; regexp
   (rx-to-string `(seq "!" (or ,@plantuml--preprocessor-keywords)))
   ;; font-face
   '(0 font-lock-preprocessor-face)))

(defconst plantuml--font-lock-keywords
  (list
   ;; regexp
   (concat "\\<" (regexp-opt plantuml--keywords) "\\>" )
   ;; font-face
   '(0 font-lock-keyword-face)))

(defconst plantuml--font-lock-skin-parameters
  (list
   ;; regexp
   (rx-to-string
    `(seq "skinparam"
	  (one-or-more whitespace)
	  (group (or ,@plantuml--skin-parameters))
	  (one-or-more whitespace)
	  (group (one-or-more word))
	  eol))
   ;; font-face
   '(1 font-lock-variable-name-face)
   '(2 font-lock-constant-face)))

(defconst plantuml--font-lock-sequence-connectors
  (list
   (rx-to-string
    `(seq bol (+ whitespace)
	  (group (+ (or word "_")))
	  (*? whitespace)
	  (group (seq (or "?-" "[-" "_" "\\\\" "//" "o\\\\" "<" ">")
		      (*? (seq "[#" (or (repeat 6 hex-digit) (or ,@plantuml--color-names)) "]"))
		      (or ">" ">>" "\\-" "-" "--"  ">o" "->" "->o" ">]" ">?")
		      ))
	  (*? whitespace)
	  (group (+ (or word "_")))
	  (*? whitespace)
	  ":"
	  (+? whitespace)
	  (group (* any))
	  ))
   ;; faces
   '(1 font-lock-constant-face)
   '(2 font-lock-warning-face)
   '(3 font-lock-constant-face)
   '(4 font-lock-string-face))
  "Face definition for sequence diagram syntax of the form Alice
-> Bob: Secret")

(defconst plantuml--font-lock-mindmap-headers
  (mapcar
   (lambda (x)
     (list
      ;; regexp
      (rx-to-string `(seq bol (repeat ,x (or "*" "+" "-"))
			  (? "[" (group "#" (or (repeat 6 digit) (or ,@plantuml--color-names))) "]")
			  (? (not (any "*" "_" "+" )))
			  (+ space)
			  (group (* any))))
      ;; face sexp
      `(2 (if (match-string-no-properties 1)
	      (add-text-properties (match-beginning 2) (match-end 2)
				   (list 'font-lock-face (list :foreground (plantuml--find-color (match-string-no-properties 1)))))
	    ,(format "outline-%d" x)))
      ))
   (number-sequence 1 8))
  "Face definitions for mindmap headers")

(defun add-font-lock-face-props (start end props)
  (let ((existing-face-props (plist-get (text-properties-at start) 'font-lock-face)))
    (add-text-properties
     start end
     (list 'font-lock-face (append existing-face-props props)))))

;; creole
(defconst plantuml--font-lock-creole-bold
  '("\\*\\*[^\n]+?\\*\\*"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:weight bold)))))

(defconst plantuml--font-lock-separator
  '("==[^\n]+?=="
    (0 (add-text-properties (match-beginning 0) (match-end 0) '(font-lock-face (:weight bold :height 1 :width expanded))))))

(defconst plantuml--font-lock-creole-italic
  '("//[^\nn]+?//"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:slant italics)))))

(defconst plantuml--font-lock-creole-underline
  '("__[^\n]+?__"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:underline t)))))

(defconst plantuml--font-lock-creole-monospace
  '("\"\"[^\n]+?\"\"" . 'header-line))

(defconst plantuml--font-lock-creole-strikethrough
  '("--[^\n]+?--"
    (0 (add-font-lock-face-props (match-beginning 0) (match-end 0) '(:strike-through t)))))

(defconst planutml--font-lock-alias
  '("\\<as[[:space:]]+\\([[:alnum:]_]+\\)[[:space:]]*$"
    (1 font-lock-variable-name-face)))

(defconst plantuml--font-lock-condition
  '("(\\([^\n)]+?\\))" . 'font-lock-string-face))

(defconst plantuml--font-lock-swimlane
  '("|\\(?:\\([^|\n]+|\\)?\\([^|\n]+\\)\\)|" . 'lazy-highlight))

(defconst plantuml--font-lock-sequence-arrows
  (cons
   (regexp-opt plantuml--sequence-arrows)
   'font-lock-warning-face))


(defconst plantuml--font-lock-defaults
  (list
   ;; group syntax
   plantuml--font-lock-skin-parameters
   planutml--font-lock-alias
   plantuml--font-lock-sequence-connectors
   ;; keywords
   plantuml--font-lock-diagrams
   plantuml--font-lock-preprocessor
   plantuml--font-lock-components
   plantuml--font-lock-keywords
   ;; special syntax
   plantuml--font-lock-swimlane
   plantuml--font-lock-condition
   plantuml--font-lock-sequence-arrows
   plantuml--font-lock-separator
   ;; creole
   plantuml--font-lock-creole-bold
   plantuml--font-lock-creole-italic
   plantuml--font-lock-creole-underline
   plantuml--font-lock-creole-monospace
   plantuml--font-lock-creole-strikethrough
   ))

;;;###autoload
(define-derived-mode plantuml-mode
  prog-mode "plantuml" "Major mode for plantuml"
  (setq
   font-lock-defaults '((plantuml--font-lock-defaults) nil t))
  (unless (executable-find plantuml-java-cmd)
    (setq plantuml-java-cmd (get-java-cmd)))

  (unless (file-exists-p plantuml-jar-path)
    (setq plantuml-jar-path (get-plantuml-jar)))

  (font-lock-add-keywords 'plantuml-mode plantuml--font-lock-mindmap-headers)
  (add-hook 'xref-backend-functions #'plantuml-xref-backend)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode) t)

;;; compilation helpers
(defun plantuml--kill-inactive (buffer)
  (unless (equal (window-buffer) buffer)
    (when-let ((bw (seq-find (lambda (w) (equal buffer (window-buffer w))) (window-list))))
      (delete-window bw))
    (kill-buffer buffer)))

(defun plantuml--compile-close (buffer status)
  (when (string-prefix-p "*plantuml-" (buffer-name buffer))
    (run-with-timer plantuml-close-compilation-buffer nil #'plantuml--kill-inactive buffer))
  (when (buffer-local-boundp 'source-buffer-file buffer)
    (let ((file (buffer-local-value 'source-buffer-file buffer))
	  (bufname (buffer-name buffer)))
      (when (and (string= "finished\n" status)
		 (string-prefix-p "*plantuml-open" bufname))
	(plantuml--open-preview-program (concat (file-name-sans-extension file) ".png"))))))

;; setup compilation error processing
(let ((form '(plantuml "^Error line \\([[:digit:]]+\\) in file: \\(.*\\)$" 2 1 nil (2))))
  (add-to-list 'compilation-error-regexp-alist (car form))
  (add-to-list 'compilation-error-regexp-alist-alist form)
  (add-to-list 'compilation-finish-functions #'plantuml--compile-close))

(defun plantuml--compilation-command (action)
  (pcase action
    ("compile" #'plantuml--compile-command)
    (_ #'plantuml--preview-command)))

(defun plantuml--compile-command (file)
  (string-join
   (list
    (shell-quote-argument plantuml-java-cmd) "-Djava.awt.headless=true"
    "-jar" (shell-quote-argument plantuml-jar-path)
    "-failfast" file)
   " "))

(defun plantuml--preview-command (file)
  (string-join
   (list
    (shell-quote-argument plantuml-java-cmd) "-Djava.awt.headless=true"
    "-jar" (shell-quote-argument plantuml-jar-path)
    "-tpng" "-failfast" file)
   " "))

(defmacro plantuml--compilation-buffer-name (action)
  `(lambda (_) (format "*plantuml-%s*" ,action)))

(defun plantuml--open-preview-program (filename)
  "Launch preview program to preview generated png file"
  (let ((default-directory (file-name-directory filename))
	(target-file (file-name-nondirectory filename))
	(buffer-name "*plantuml-open-preview*"))
    (if (string-equal plantuml-preview-program "msphotos")
	(start-process buffer-name nil "cmd.exe" "/c" "start" "" target-file)
      (start-process buffer-name nil plantuml-preview-program target-file))))

(defun plantuml--run-compile (action)
  "Run specified compilation action to display compilation buffer"
  ;;save
  (when plantuml-force-save-before-preview
    (save-buffer))
  (let* ((filename (buffer-file-name))
	 (file (file-name-nondirectory filename))
	 (default-directory (file-name-directory filename))
	 (name-fn (plantuml--compilation-buffer-name action)))
    (with-current-buffer
	(compilation-start
	 (funcall (plantuml--compilation-command action) file)
	 'compilation-mode
	 name-fn
	 t)
      (setq-local source-buffer-file (expand-file-name file default-directory)))))

(defun plantuml-compile ()
  "Compile plantuml diagram to display syntax errors"
  (interactive)
  (plantuml--run-compile "compile"))

(defun plantuml-preview ()
  "Generate plantuml diagram preview png. Use with universal
argument (C-u) to clear existing preview output files"
  (interactive)
  (when current-prefix-arg
    (plantuml--clear-outputs (buffer-file-name)))
  (plantuml--run-compile "preview"))

(defun plantuml-open-preview ()
  "Generate plantuml diagram preview png. Use with universal
argument (C-u) to clear existing preview output files"
  (interactive)
  (when current-prefix-arg
    (plantuml--clear-outputs (buffer-file-name)))
  (plantuml--run-compile "open-preview"))

(defun plantuml--preview-candidate-files (base)
  (mapcar (lambda (ext) (concat base "." (symbol-name ext))) plantuml--output-file-extensions))

(defun plantuml--delete-if-exists (file)
  (when (file-exists-p file)
    (delete-file file)))

(defun plantuml--clear-outputs (file)
  (let ((candidates (plantuml--preview-candidate-files (file-name-sans-extension file))))
    (mapc #'plantuml--delete-if-exists candidates)))

;;; preview helper functions
(defun get-java-cmd ()
  "Returns absolute path to the java executable assuming that the
JAVA HOME environment variable is set"
  (expand-file-name
   "bin/java"
   (let ((java-home (getenv "JAVA_HOME" )))
     (if (string-prefix-p "%" java-home)
	 (getenv (replace-regexp-in-string "%" "" java-home))
       java-home
       ))))

(defun get-plantuml-jar ()
  "Returns absolute path to plantuml jar file assuming APPS_HOME
environment variable is defined and only one jar file is present
in the $APPS_HOME/plantuml folder"
  (let ((plantuml-home (expand-file-name "plantuml" (getenv "APPS_HOME" )))
	(find-jar (lambda (x) (string-suffix-p ".jar" x) )))
    (let ((plantuml-jar (car
			 (seq-filter
			  find-jar
			  (directory-files plantuml-home)))))
      (when plantuml-jar
	(expand-file-name plantuml-jar plantuml-home)))))

(defun plantuml-select-diagram ()
  "Insert bootstrap template based on the diagram type selected"
  (interactive)
  (when-let ((diagram
	      (plantuml--select-from-table
	       plantuml-diagram-types
	       "Select diagram" )))
    (let* ((suffix (car (last diagram)))
	   (insert-bnc (and plantuml-insert-basename-comment (buffer-file-name)))
	   (cursor))
      (goto-char (point-min))
      (insert (format "@start%s\n" suffix))
      (when insert-bnc
	(insert (format "' %s\n"
			(file-name-sans-extension
			 (file-name-nondirectory (buffer-file-name))))))
      (setq cursor (point))
      (insert (format "\n@end%s\n" suffix))
      (goto-char cursor))))

(defun plantuml--make-alias (s)
  "Construct an alias from the string"
  (mapconcat
   (lambda (x)
     (char-to-string
      (car (append
	    (replace-regexp-in-string
	     (rx (not (any word digit)))
	     "" x)
	    nil))))
   (split-string (downcase s))
   ""))

(defmacro plantuml--pair-formatter (pf)
  "Return function to format the pair"
  `(lambda (item)
     (format ,pf (propertize (car item) 'font-lock-face 'font-lock-constant-face)
	     (propertize (cadr item) 'font-lock-face 'font-lock-function-name-face))))

(defun plantuml--1-column (table)
  "Format TABLE in a single column"
  (mapconcat
   (plantuml--pair-formatter "[%s]...%s")
   table
   "\n" ))

(defun plantuml--2-column (table)
  (let ((col1) (col2) (el)
	(col1f (plantuml--pair-formatter " [%s]...%-14s"))
	(col2f (plantuml--pair-formatter (concat (make-string 5 ? ) " [%s]...%-14s")))
	(e2) (rtn))
    (dolist (i (number-sequence 0 (1- (length table))))
      (if (= (mod i 2) 0)
	  (push (nth i table) col1)
	(push (nth i table) col2)))
    (setq col1 (reverse col1)
	  col2 (reverse col2))
    (setq el (pop col1)
	  e2 (pop col2))
    (while (or el e2)
      (push (concat
	     (if el
		 (funcall col1f el)
	       (make-string 20 ? ))
	     (if e2
		 (funcall col2f el)
	       ""))
	    rtn)
      (setq el (pop col1)
	    e2 (pop col2))
      )
    (string-join (reverse rtn) "\n")))

(defun plantuml--3-column (table)
  (let ((col1) (col2) (col3)
	(col1f (plantuml--pair-formatter "[%s]...%-14s"))
	(col2f (plantuml--pair-formatter (concat (make-string 5 ? ) " [%s]...%-14s")))
	(e1) (e2) (e3) (rtn))
    (dolist (i (number-sequence 0 (1- (length table))))
      (pcase (mod i 3)
	(0 (push (nth i table) col1)) (1 (push (nth i table) col2))
	(2 (push (nth i table) col3))
	))
    (setq col1 (reverse col1)
	  col2 (reverse col2)
	  col3 (reverse col3))
    (setq e1 (pop col1)
	  e2 (pop col2)
	  e3 (pop col3))
    (while (or e1 e2 e3)
      (push (concat
	     (if e1
		 (funcall col1f e1)
	       (make-string 20 ? ))
	     (if e2
		 (funcall col2f e2)
	       (make-string 25 ? ))
	     (if e3
		 (funcall col2f e3)
	       ""))
	    rtn)
      (setq e1 (pop col1)
	    e2 (pop col2)
	    e3 (pop col3))
      )
    (string-join (reverse rtn) "\n")))

(defun plantuml--select-from-table (table prompt &optional format)
  (let (allowed-keys rtn pressed formatter buffer (inhibit-quit t))
    (save-window-excursion
      (setq buffer (switch-to-buffer-other-window "*plantuml-select*"))
      (catch 'exit
	(while t
	  (erase-buffer)
	  (font-lock-mode 1)
	  (insert prompt "\n\n")
	  (setq allowed-keys (mapcar #'car table))
	  (let ((tl (length table)))
	    (setq formatter (or
			     format
			     (cond
			      ((< tl 10) #'plantuml--1-column)
			      ((and (> tl 10) (< tl 20)) #'plantuml--2-column)
			      (t #'plantuml--3-column)))))
	  (insert (funcall formatter table))
	  (push "\C-g" allowed-keys)
	  (goto-char (point-min))
	  (message prompt)
	  (setq pressed (char-to-string (read-char-exclusive)))
	  (while (not (member pressed allowed-keys))
	    (message "Invalid key `%s'" pressed)
	    (sit-for 1)
	    (message prompt)
	    (setq pressed (char-to-string (read-char-exclusive))))
	  (when (equal pressed "\C-g")
	    (kill-buffer buffer)
	    (error "Abort" ))
	  (throw 'exit
		 (setq rtn (seq-find (lambda (x) (string= pressed (car x))) table))))))
    (when buffer (kill-buffer buffer))
    rtn))

(defun plantuml-insert-element (prefix)
  "Insert deployment diagram element"
  (interactive "p")
  (when-let ((element
	      (plantuml--select-from-table
	       plantuml-component-elements
	       "Select element" )))
    (if (eql 16 prefix)
	(let ((als (read-string "Alias: "))
	      (ci (make-string (current-indentation) ? )))
	  (insert (format "%s %s [\n%s]" (cadr element) als ci)))
      (let* ((desc (read-string "Element name: "))
	     (als (if (eql 4 prefix)
		      (read-string "Alias: ")
		    (plantuml--make-alias desc))))
	(insert
	 (format "%s %s as \"%s\"" (cadr element) als desc))))))


(provide 'plantuml-mode)
;;; plantuml-mode.el -- Ends here
