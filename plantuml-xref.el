;;; plantuml-xref.el -- plantuml xref backend -*- lexical-binding: t -*-
(defun plantuml-xref-backend ()
  "xref backend for plantuml files"
  'plantuml)

(cl-defmethod ref-backend-identifier-at-point ((_ (eql plantuml)))
  "Return the identifier to lookup"
  ;; (message "plantuml-xref (%s): Identifier %s (point=%d)" (buffer-name) (symbol-at-point) ( point ))
  (symbol-name (symbol-at-point))
  )
;; xref-backend
(cl-defmethod ref-backend-identifier-completion-table ((_ (eql plantuml)))
  "Return list of terms for completion from the current buffer"
  (plantuml--find-definitions nil))
(cl-defmethod ref-backend-definitions ((_ (eql plantuml)) symbol)
  ;; (message "plantuml-xref (%s) : %s" (buffer-name) symbol)
  (plantuml--find-definitions symbol t))
(cl-defmethod ref-backend-references ((_ (eql plantuml)) symbol)
  "List of references matching symbol"
  (plantuml--find-definitions symbol t))
;; xref helper funtion
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
