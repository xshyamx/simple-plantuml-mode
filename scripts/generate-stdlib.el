;;; generate-stdlib.el --- Generate cloud stdlib components  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;;

;;; Code:


(defun jar-version (dir)
  (let ((default-directory (replace-regexp-in-string "/stdlib/.*$" "" dir)))
    (let ((m-file (expand-file-name "META-INF/MANIFEST.MF")))
      (when (file-exists-p m-file)
	(with-temp-buffer
	  (insert-file-contents m-file)
	  (goto-char (point-min))
	  (when (re-search-forward "Implementation-Version: ")
	    (buffer-substring-no-properties (match-end 0) (line-end-position))))))))

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

(defun write-stdlib-file (prefix cps jv file)
  (let* ((fbn (file-name-nondirectory file))
	 (cloud (replace-regexp-in-string
		 (rx (or (seq bol "plantuml-stdlib-")
			 (seq ".el" eol)))
		 "" fbn))
	 (label (alist-get cloud cloud-labels nil nil #'string=))
	 (preamble-format ";;; %s --- PlantUML %s components  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Components extracted from plantuml.jar version `%s'

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
      (insert (format preamble-format fbn label jv))
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
	 (jv (jar-version basedir))
	 (files (directory-files-recursively (expand-file-name basedir) "all.puml\\'" nil))
	 (prefix (file-name-nondirectory basedir))
	 (cps (apply #'append
		     (mapcar #'extract-components (mapcar #'expand-file-name files)))))
    (write-stdlib-file prefix (seq-sort-by #'car #'string< cps) jv file)))

(provide 'generate-stdlib)
;;; generate-stdlib.el
