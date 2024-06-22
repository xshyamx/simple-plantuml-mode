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
		     ("one 1 2 3" . "o123"))))
    (dolist (scenario scenarios)
      (should
       (string= (cdr scenario)
		(plantuml--make-alias (car scenario)))))))

(provide 'plantuml-mode-test)
;;; plantuml-mode-test.el ends here
