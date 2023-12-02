;;; plantuml-xref.el -- plantuml xref backend -*- lexical-binding: t -*-
(require 'xref)
(require 'cl-lib)

(defun plantuml-xref-backend ()
  "xref backend for plantuml files"
  'plantuml)

(cl-defmethod xref-backend-identifier-at-point ((_ (eql plantuml)))
  "Return the identifier to lookup"
  ;; (message "plantuml-xref (%s): Identifier %s (point=%d)" (buffer-name) (symbol-at-point) ( point ))
  (symbol-name (symbol-at-point))
  )
;; xref-backend
(cl-defmethod xref-backend-identifier-completion-table ((_ (eql plantuml)))
  "Return list of terms for completion from the current buffer"
  (plantuml--find-definitions nil))
(cl-defmethod xref-backend-definitions ((_ (eql plantuml)) symbol)
  ;; (message "plantuml-xref (%s) : %s" (buffer-name) symbol)
  (plantuml--find-definitions symbol t))
(cl-defmethod xref-backend-references ((_ (eql plantuml)) symbol)
  "List of references matching symbol"
  (plantuml--find-references symbol))

;; xref helper funtion
(defun plantuml--find-references (symbol)
  "Find references of symbol in the buffer"
  (let ((case-fold-search t)
	(regexp (rx-to-string `(seq bow ,symbol eow)))
	(matches))
    (save-excursion
      (save-restriction
	(widen)
	(while (re-search-forward regexp nil t)
	  (push
	   (xref-make
	    (buffer-substring (line-beginning-position) (line-end-position))
	    (xref-make-buffer-location (current-buffer) (match-beginning 0)))
	   matches))))
    matches))

(defun plantuml--find-definitions (symbol &optional ref)
  "Find definitions in buffer if 'REF' is + retun matches"
  (let ((case-fold-search t)
	(regexp (if symbol
		    (rx-to-string `(seq bow (or ,@plantuml--component-types) eow (+ space) bow (group ,symbol) eow))
		  (rx-to-string `(seq bow (or ,@plantuml--component-types) eow (+ space) bow (group (+ (any word "_"))) eow)))))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(let (matches)
	  (while (re-search-forward regexp nil t)
	    (push (if ref
		      (xref-make
		       (buffer-substring-no-properties (line-beginning-position) (line-end-position))
		       (xref-make-buffer-location (current-buffer) (match-beginning 1)))
		    (match-string-no-properties 1))
		  matches))
	  matches)
	))
    )
  )

(provide 'plantuml-xref)
;;; plantuml-xref.el -- Ends here
