;;; plantuml-tests.el --- Tests for simple-plantuml-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the simple-plantuml-mode

;;; Code:

(require 'ert)

(ert-deftest plantuml--make-alias-test ()
  "Tests for making aliases"
  (let ((scenarios '(("ABCD" . "a")
		     ("Frontend \n Application" . "fa")
		     ("Frontend \\n Application" . "fa")
		     ("Database 1" . "d1")
		     ("one 1 2 3" . "o123")
		     ("Jack & Jill" . "jj")
		     ("find-replace" . "fr")
		     ("One \n Two & Three " . "ott"))))
    (dolist (scenario scenarios)
      (should
       (string= (cdr scenario)
		(plantuml--make-alias (car scenario)))))))

(ert-deftest plantuml--make-container-test ()
  "Tests for making containers"
  (let ((scenarios
	 '((1  "Plain" "rectangle \"Plain\" #transparent;line.dashed {")
	   (4  "Plain" "rectangle \"Plain\" #transparent;line.dotted {")
	   (16 "Plain" "rectangle \"Plain\" #transparent {"))))
    (dolist (scenario scenarios)
      (cl-destructuring-bind (prefix name expected) scenario
	(with-temp-buffer
	  (plantuml-insert-container prefix name)
	  (goto-char 0)
	  (should (re-search-forward expected nil t)))))))

(provide 'plantuml-mode-test)
;;; plantuml-mode-test.el ends here
