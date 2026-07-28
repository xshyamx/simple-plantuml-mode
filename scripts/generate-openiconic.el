;;; generate-openiconic.el --- Generate openiconic file  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Generates the plantuml-openiconic.el file

;;; Code:

(defun plantuml-extract-icons (java-cmd plantuml-jar)
  (with-temp-buffer
    (let ((cmd (string-join
		(list "echo"
		      "listopeniconic"
		      "|"
		      java-cmd
		      "-Djava.awt.headless=true"
		      "-jar"
		      plantuml-jar
		      "--svg"
		      "--pipe")
		" "))
	  (icons)
	  (regexp (rx "<text" (+ (not ">")) ">" (group (+ (not "<"))))))
      (shell-command cmd (current-buffer))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(push (match-string-no-properties 1) icons))
      (seq-subseq (reverse  icons) 4))))

(defun write-openiconic-file (icons file)
  (let ((preamble ";;; plantuml-openiconic.el --- Insert openiconic sprite  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;; Insert openiconic sprite for PlantUML

;;; Code:

")
	(postamble "(provide 'plantuml-openiconic)
;;; plantuml-openiconic.el
")
	(command-keybinding "(defun plantuml-insert-icon ()
  (interactive)
  (when-let (icon (completing-read \"Select icon:\" plantuml-icons-openiconic nil t))
    (insert \"<&\" icon \">\")))

(keymap-set plantuml-mode-map \"C-c o\" #'plantuml-insert-icon)
")
	(nl "\n")
	(inhibit-message t))
    (with-temp-buffer
      (erase-buffer)
      (insert preamble)
      (insert (format "(defconst plantuml-icons-openiconic
  '%S
  \"OpenIconic sprite names for PlantUML\")" icons))
      (when (re-search-backward "'(" nil t)
	(forward-char)
	(mark-sexp)
	(fill-paragraph nil t))
      (goto-char (point-max))
      (insert nl nl command-keybinding)
      (insert nl postamble)
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
    (let ((icons (plantuml-extract-icons plantuml-java-cmd plantuml-jar-path))
	  (file (car argv)))
      (write-openiconic-file icons file))))

(provide 'generate-openiconic)
;;; generate-openiconic.el
