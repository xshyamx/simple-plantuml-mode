;;; generate-keywords.el --- Generate plantuml keywords file  -*- lexical-binding: t; -*-

;; Author: shyam
;;; Commentary:

;;

;;; Code:

(defun write-keywords-file (lmap file)
  (with-temp-buffer
    (insert ";;; plantuml-keywords.el -- Plantuml keyword constants -*- lexical-binding: t -*-

;; most of the following constants are extracted from output of
;; 'java -jar plantuml.jar -language'
")
    (when-let ((types (gethash "type" lmap)))
      (insert (format "
(defconst plantuml--component-%ss
\t'%S
\t\"Plantuml component types\")
" "type" (seq-sort #'string< types))))

    (when-let ((dts (mapcar (lambda (s) (replace-regexp-in-string "@start" "" s))
			    (seq-filter (lambda (s) (string-prefix-p "@start" s)) (gethash "keyword" lmap)))))
      (insert (format "
(defconst plantuml--diagram-types
\t'%S
\t\"Plantuml diagram types\")
" (seq-sort #'string< dts))))

    (when-let ((kws (seq-filter
		     (lambda (s) (not (string-prefix-p "@" s)))
		     (gethash "keyword" lmap))))
      (insert (format "
(defconst plantuml--keywords
\t'%S
\t\"Plantuml keywords\")
" (seq-sort #'string< kws))))

    (when-let ((sps (gethash "skinparameter" lmap)))
      (insert (format "
(defconst plantuml--skin-parameters
\t'%S
\t\"Plantuml skin parameters\")
" (seq-sort #'string< sps))))

    (when-let ((pps (mapcar (lambda (s) (substring s 1))
			    (gethash "preprocessor" lmap))))
      (insert (format "
(defconst plantuml--preprocessor-keywords
\t'%S
\t\"Plantuml preprocessor instructions\")
" (seq-sort #'string< pps))))

    (insert (format "
(defconst plantuml--sequence-arrows
\t'%S)
" '("->x" "->" "->>" "-\\" "\\\\-" "//--" "->o" "o\\\\--" "<->" "<->o")))
    (insert "

(provide 'plantuml-keywords)
;;; plantuml-keywords.el -- Ends here
")
    (goto-char (point-min))
    (while (re-search-forward "'(" nil t)
      (fill-region (1+ (match-beginning 0))
		   (1- (line-end-position))))
    (goto-char (point-min))
    (while (re-search-forward "^(defconst" nil t)
      (goto-char (line-beginning-position))
      (indent-sexp)
      (forward-line))
    (write-region (point-min) (point-max) file)))

(defun extract-keywords ()
  (let ((lmap (make-hash-table :test #'equal)))
    (with-temp-buffer
      (shell-command
       (string-join
	(list
	 (shell-quote-argument plantuml-java-cmd) "-Djava.awt.headless=true"
	 "-jar" (shell-quote-argument plantuml-jar-path)
	 "-language")
	" ")
       (current-buffer))
      (goto-char (point-min))
      (let ((token) (lkey) (tokens) (count))
	(while (not (eobp))
	  (setq token (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	  (cond
	   ((string-prefix-p ";" token)
	    (setq token (substring token 1)
		  tokens nil)
	    (if (string-match (rx bol (+ digit) eol) token)
		(setq count (string-to-number token))
	      (setq lkey token)
	      (puthash lkey tokens lmap)))
	   ((string= "" token)
	    (puthash lkey tokens lmap)
	    (setq tokens nil
		  lkey nil))
	   (t
	    (push token tokens)))
	  (forward-line))
	))
    lmap))

(defun maven-jar-path (m2-repo group-id artifact-id version)
  (expand-file-name
   (format "%s/%s/%s/%s-%s.jar" group-id artifact-id version artifact-id version)
   m2-repo))


(when noninteractive
  (when (< (length argv) 1)
    (message "Require output filename")
    (kill-emacs 1))
  (let ((home-dir (getenv "HOME")))
    (setq
     plantuml-java-cmd (executable-find "java")
     plantuml-jar-path
     (maven-jar-path
      (expand-file-name ".m2/repository" (getenv "HOME"))
      "net/sourceforge/plantuml"
      "plantuml"
      "1.2025.8")))
  (when-let ((lmap (extract-keywords)))
    (write-keywords-file lmap (car argv))))

(provide 'generate-keywords)
;;; generate-keywords.el
