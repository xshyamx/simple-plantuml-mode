;;; generate-stdlib.el --- Generate cloud stdlib components  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;;

;;; Code:


(defun repo-and-tag (dir)
  (let ((default-directory (expand-file-name dir))
	(repo-cmd (list (executable-find "git")
			"remote" "-v"
			"|" "head" "-1"
			"|" "awk" "'{print $2}'"))
	(tag-cmd (list (executable-find "git")
		       "describe" "--tags")))
    (cons (string-trim (shell-command-to-string (string-join repo-cmd " ")))
	  (string-trim (shell-command-to-string (string-join tag-cmd " "))))))

(defun extract-components (file)
  (let ((regexp (rx bol "!define" (+ space) (group (+ (not "(")))))
	(els))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(push (match-string-no-properties 1) els))
      (mapcar (lambda (el)
		(cons el (nth 1 (reverse (file-name-split file)))))
	      (seq-uniq els #'string=)))))

(defvar cloud-labels
  '(("aws" . "AWS")
    ("azure" . "Azure")
    ("gcp" . "GCP"))
  "Lookup label for cloud prefix")

(defun write-stdlib-file (cps rat file)
  (let* ((fbn (file-name-nondirectory file))
	 (cloud (replace-regexp-in-string
		 (rx (or (seq bol "plantuml-stdlib-")
			 (seq ".el" eol)))
		 "" fbn))
	 (prefix (if (string= cloud "aws") "awslib" cloud))
	 (label (alist-get cloud cloud-labels nil nil #'string=))
	 (preamble-format ";;; %s --- PlantUML %s components  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Components extracted from %s on tag `%s'

;;; Code:

(require 'cl-generic)
")
	 (postamble-format "(cl-defmethod plantuml-stdlib--common-include ((library (eql '%s)))
  \"!include <%s/%sCommon>\")

(cl-defmethod plantuml-stdlib--prefix ((library (eql '%s)))
  \"%s\")

(cl-defmethod plantuml-stdlib--components ((library (eql '%s)))
  plantuml-%s-components)

(provide '%s)
;;; %s
")
	 (nl "\n"))
    (with-temp-buffer
      (erase-buffer)
      ;; file-base-name label repo tag
      (insert (format preamble-format fbn label (car rat) (cdr rat)))
      (insert (format "(defconst plantuml-%s-components" cloud))
      (insert nl "  '(")
      (insert (format "%S" (car cps)))
      (dolist (cp (cdr cps))
	(insert nl (format "    %S" cp)))
      (insert ")" nl (format "  \"List of PlantUML %s components\")" label))
      (insert nl nl (format postamble-format
			    cloud prefix label cloud prefix
			    cloud cloud
			    (file-name-base file) fbn))
      (write-region (point-min) (point-max) file))))

(when noninteractive
  (when (< (length argv) 2)
    (message "Require directory & file")
    (kill-emacs 1))
  (let* ((basedir (expand-file-name (expand-file-name (car argv))))
	 (file (expand-file-name (nth 1 argv)))
	 (rat (repo-and-tag basedir))
	 (files (directory-files-recursively (expand-file-name "dist" basedir) "all.puml\\'" nil))
	 (cps (apply #'append
		     (mapcar #'extract-components (mapcar #'expand-file-name files)))))
    (write-stdlib-file (seq-sort-by #'car #'string< cps) rat file)))

(provide 'generate-stdlib)
;;; generate-stdlib.el
