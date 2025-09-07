;;; plantuml-extras.el --- PlantUML Extras  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Extra helper functionalities

;;; Code:
;; Add fragments

(defun plantuml--make-title (file)
  "Extract the filename slug and capitalize after splitting by `-'"
  (string-join
   (mapcar #'capitalize
 	   (seq-filter
	    (lambda (s) (> (length s) 1))
	    (split-string
	     (file-name-sans-extension
	      (file-name-base file))
	     "-")))
   " "))

(defun plantuml-add-header (prefix)
  "Add header if not already present in file. Force override with
`C-u'"
  (interactive "p")
  (save-excursion
    (goto-char 0)
    (condition-case nil
	(when (re-search-forward
	       (rx bol (* space) "header" eow (* any) eol))
	  (if (> prefix 1)
	      (atomic-change-group
		(delete-region (match-beginning 0) (match-end 0))
		(insert "header Draft"))
	    (message "Header already present")))
      ((error)
       (goto-char 0)
       (forward-line)
       (while (looking-at (rx bol (* space) "'" (* any) eol))
	 (forward-line))
       (insert "header Draft" "\n")))))

(defun plantuml-add-title (prefix)
  "Add title if not already present in file based on the
filename. Force override with `C-u'"
  (interactive "p")
  (when (buffer-file-name)
    (save-excursion
      (goto-char 0)
      (condition-case nil
	  (when (re-search-forward
		 (rx bol (* space) "title" eow (* any) eol))
	    (if (> prefix 1)
		(atomic-change-group
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert "title "
			  (plantuml--make-title (buffer-file-name))))
	      (message "Title already present")))
	((error)
	 (goto-char 0)
	 (forward-line)
	 (while (looking-at (rx bol (* space) "'" (* any) eol))
	   (forward-line))
	 (insert "title "
		 (plantuml--make-title (buffer-file-name)) "\n"))))))

(defun plantuml-add-footer (prefix)
  "Add footer if not already present in file. Force override with
`C-u'"
  (interactive "p")
  (save-excursion
    (goto-char 0)
    (condition-case nil
	(when (re-search-forward
	       (rx bol (* space) "footer" eow (* any) eol))
	  (if (> prefix 1)
	      (atomic-change-group
		(delete-region (match-beginning 0) (match-end 0))
		(insert (format-time-string "footer %Y-%m-%d")))
	    (message "Footer already present")))
      ((error)
       (goto-char 0)
       (forward-line)
       (while (looking-at (rx bol (* space) "'" (* any) eol))
	 (forward-line))
       (insert (format-time-string "footer %Y-%m-%d") "\n")))))

(defun plantuml-add-slug-comment (prefix)
  "Add slug comment if not already present in file. Force override
with `C-u'"
  (interactive "p")
  (when (buffer-file-name)
    (save-excursion
      (goto-char 0)
      (condition-case nil
	  (when (re-search-forward
		 (rx bol (* space) "'" (* any) eol))
	    (if (> prefix 1)
		(atomic-change-group
		  (delete-region (match-beginning 0) (match-end 0))
		  (insert "' " (file-name-base (buffer-file-name))))
	      (message "Footer already present")))
	((error)
	 (goto-char 0)
	 (forward-line)
 	 (insert "' " (file-name-base (buffer-file-name)) "\n"))))))

;;; Add keybindings
(let ((keymap plantuml-add-map))
  (keymap-set keymap "h" #'plantuml-add-header)
  (keymap-set keymap "t" #'plantuml-add-title)
  (keymap-set keymap "f" #'plantuml-add-footer)
  (keymap-set keymap "s" #'plantuml-add-slug-comment))

;; Enclose
(defun plantuml-open-include-file ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (when (and
 	   (buffer-file-name)
	   (re-search-forward
	    (rx bol (* space) "!include" (+ space)
		(group (+ any)) eol)
	    (line-end-position) t))
      (let ((file (expand-file-name
		   (match-string-no-properties 1)
		   (file-name-directory (buffer-file-name)))))
	(when (file-exists-p file)
	  (find-file file))))))

(defun plantuml-enclose-color (start end color)
  "Enclose the selected region with creole `color' tag with the selected
color"
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read "Select color: "
			  (mapcar #'car plantuml--colors) nil t)))
  (when (use-region-p)
    (let ((s (buffer-substring-no-properties start end)))
      (atomic-change-group
	(delete-region start end)
	(insert "<color:" color ">"
		s "</color>")))))

(defun plantuml--enclose-monospace (start end)
  "Enclose the selected region as monospace using `\"\"'"
  (interactive "r")
  (when (use-region-p)
    (let ((s (buffer-substring-no-properties start end)))
      (atomic-change-group
 	(delete-region start end)
	(insert (format "\"\"%s\"\"" s))))))

(defun plantuml-enclose-size (start end size)
  "Enclose the selected region with creole `size' tag with the selected
size"
  (interactive
   (list (region-beginning) (region-end)
	 (string-to-number (read-string "Select size: "))))
  (when (and (use-region-p)
	     (> size 0))
    (let ((s (buffer-substring-no-properties start end)))

      (atomic-change-group
	(delete-region start end)
	(insert "<size:" (number-to-string size) ">"
		s "</size>")))))

(defun plantuml-unenclose-tag ()
  "Remove any creole tag of the form <xxx>t|ext</xxx> when point is
positioned inside the tag contents"
  (interactive)
  (let ((p (point))
	(lb (line-beginning-position))
	(le (line-end-position))
	(found))
    (save-excursion
      (goto-char lb)
      (while (and (not found)
		  (re-search-forward
		   (rx "<" (not (or "<" "/")) (+ (not ">")) ">"
		       (group (+ (not "<")))
 		       "</" (+ (not ">")) ">")
		   le t))
	(when-let (found (and (< (match-beginning 0) p)
			      (< p (match-end 0))))
	  (replace-match
	   (match-string-no-properties 1) t t))))))

;;; Add keybindings
(let ((keymap plantuml-enclose-map))
  (keymap-set keymap "c" #'plantuml--enclose-monospace)
  (keymap-set keymap "h" #'plantuml-enclose-color)
  (keymap-set keymap "s" #'plantuml-enclose-size)
  (keymap-set keymap "u" #'plantuml-unenclose-tag))

;; Update footer date hook command

(defun plantuml-update-footer-date ()
  "Update footer date if available"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (and
	   (buffer-file-name)
	   (re-search-forward
	    (rx bol "footer" (+ space)
		(repeat 4 digit)
		"-" (repeat 2 digit)
		"-" (repeat 2 digit)
		(* any) eol)))
      (replace-match (format-time-string "footer %Y-%m-%d") t t))))

;; Add `before-save-hook'
;; (add-hook 'before-save-hook #'plantuml-update-footer-date)

(provide 'plantuml-extras)
;;; plantuml-extras.el
