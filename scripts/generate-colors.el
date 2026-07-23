;;; generate-colors.el --- Generate colors file  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Generates the plantuml-colors.el file

;;; Code:

(defun plantuml-extract-colors (java-cmd plantuml-jar)
  (with-temp-buffer
    (let ((cmd (string-join
		(list "echo"
		      "colors"
		      "|"
		      java-cmd
		      "-Djava.awt.headless=true"
		      "-jar"
		      plantuml-jar
		      "--svg"
		      "--pipe")
		" "))
	  (colors) (color)
	  (regexp (rx (or
		       (seq "<rect fill=\"" (group (+ (not "\""))))
		       (seq "<text" (+ (not ">")) ">" (group (+ (not "<"))))))))
      (shell-command cmd (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(let ((code (match-string-no-properties 1))
	      (name (match-string-no-properties 2)))
	  (if (null name)
	      (setq color code)
	    (push (cons (downcase name) (downcase color)) colors))))
      (reverse colors))))

(defun write-colors-file (colors file)
  (let ((preamble ";;; plantuml-colors.el --- Insert colors sprite  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Insert colors sprite for PlantUML

;;; Code:

;; color codes extracted from plantuml `color'
")
	(postamble "(provide 'plantuml-colors)
;;; plantuml-colors.el
")
	(extra-functions "(defconst plantuml--color-names
  (mapcar #'car plantuml--colors)
  \"Plantuml color names only\")

(defun plantuml--find-color (color)
  \"Find hex color code for named color (strips prefix # if present)
or if in the form of #ffffff return that\"
  (if (string-match-p \"#[[:digit:]]\\\\{6\\\\}\" color)
      color
    (let ((name (replace-regexp-in-string \"^#\" \"\" color)))
      (cdr (assoc name plantuml--colors)))))")
	(nl "\n"))
    (with-temp-buffer
      (erase-buffer)
      (insert preamble)
      (insert "(defconst plantuml--colors
  '(")
      (insert (format "%S" (car colors)))
      (dolist (color (cdr colors))
	(insert nl (format "    %S" color)))
      (insert
       ")
  \"colors sprite names for PlantUML\")")

      (goto-char (point-max))
      (insert nl nl extra-functions)
      (insert nl nl postamble)
      (write-region (point-min) (point-max) file))))

(defun maven-jar-path (m2-repo group-id artifact-id version)
  (expand-file-name
   (format "%s/%s/%s/%s-%s.jar" group-id artifact-id version artifact-id version)
   m2-repo))

(when noninteractive
  (when (< (length argv) 1)
    (message "Require file argument")
    (kill-emacs 1))

  (let ((home-dir (getenv "HOME"))
	(plantuml-java-cmd (executable-find "java"))
	(plantuml-jar-path (maven-jar-path
			    (expand-file-name ".m2/repository" (getenv "HOME"))
			    "net/sourceforge/plantuml"
			    "plantuml"
			    "1.2026.1")))
    (let ((colors (plantuml-extract-colors plantuml-java-cmd plantuml-jar-path))
	  (file (car argv)))
      (write-colors-file colors file))))

(provide 'generate-colors)
;;; generate-colors.el
