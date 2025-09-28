;;; plantuml-stdlib.el --- Insert compnents from PlantUML stdlib  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Insert components from the PlantUML standard library
;; https://plantuml.com/stdlib

;;; Code:

(require 'cl-generic)
(require 'plantuml-stdlib-aws)
(require 'plantuml-stdlib-azure)
(require 'plantuml-stdlib-gcp)

(defvar plantuml-stdlib-alist
  '((aws . "AWS")
    (gcp . "GCP")
    (azure . "Azure"))
  "List of supported plantuml standard libraries")

(defvar plantuml-stdlib-selected nil
  "The current selected standard library")

(cl-defgeneric plantuml-stdlib--common-include (library)
  "Returns the common include for the LIBRARY")

(cl-defgeneric plantuml-stdlib--prefix (library)
  "Returns the library-prefix for the include statement for the LIBRARY")

(cl-defgeneric plantuml-stdlib--components (library)
  "Returns the  include for the LIBRARY")

(defun plantuml-stdlib--insert-component (library component)
  (let ((inserted nil) (common nil) (p nil)
	(common-include (plantuml-stdlib--common-include library))
	(name (car component))
	(component-include (cdr component)))

    ;; common-include present
    (save-excursion
      (setq common (re-search-backward common-include  nil t)))
    ;; component-include present
    (save-excursion
      (setq inserted (re-search-backward
		      (rx-to-string
		       `(seq
			 "!include"
			 (+ space)
			 ,(format
			   "<%s/%s>"
			   (plantuml-stdlib--prefix library)
			   component-include)))
		      nil t)))
    ;; insert below the last include if available
    (save-excursion
      (when (re-search-backward "!include" nil t)
	(goto-char (line-end-position))
	(unless common
	  (insert "\n" common-include)
	  (setq common t))
	(unless inserted
	  (insert (format "\n!include <%s/%s>"
			  (plantuml-stdlib--prefix library)
			  component-include))
	  (setq inserted t))))
    ;; if no include present so far add it now
    (unless common
      (insert "\n" common-include))
    (unless inserted
      (insert (format "\n!include <%s/%s>\n"
		      (plantuml-stdlib--prefix library)
		      component-include)))
    (insert (format "%s(" name))
    (setq p (point))
    (let ((is-group (string-suffix-p "Group" name)))
      (if is-group
	  (insert ") {}")
	(insert ",\"\", \"\")")))
    (goto-char p)))

(defun plantuml-stdlib-insert (prefix)
  (interactive "p")
  (when (or (> prefix 1)
	    (null plantuml-stdlib-selected))
    (setq plantuml-stdlib-selected
	  (car (rassoc (completing-read
			"Select library: "
			(mapcar #'cdr plantuml-stdlib-alist) nil t)
		       plantuml-stdlib-alist))))
  (when-let* ((selected (assoc plantuml-stdlib-selected
			       plantuml-stdlib-alist))
	      (library (car selected))
	      (stdlib (plantuml-stdlib--components library))
	      (name (cdr selected)))
    (when-let (comp (assoc (completing-read
			    (concat name " Component: ")
			    stdlib)
			   stdlib))
      (plantuml-stdlib--insert-component library comp))))

(keymap-set plantuml-mode-map "C-c u" #'plantuml-stdlib-insert)
(provide 'plantuml-stdlib)
;;; plantuml-stdlib.el
