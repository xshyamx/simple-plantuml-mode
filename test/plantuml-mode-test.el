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

(ert-deftest plantuml--expand-special ()
  "Tests for expanding special"
  (let ((scenarios '(("a->" . nil)
		     ("a->b,c" . ("a --> b"
				  "a --> c"))
		     ("a,b->c->d,e" . ("a --> c"
				       "b --> c"
				       "c --> d"
				       "c --> e"))
		     ("a,b->c<-d,e->f" . ("a --> c"
					  "b --> c"
					  "c <-- d"
					  "c <-- e"
					  "d --> f"
					  "e --> f")))))
    (dolist (scenario scenarios)
      (cl-destructuring-bind (input . expected) scenario
	(should (equal (plantuml--make-pairs input "-")
		       expected))))))

(ert-deftest plantuml--convert-region ()
  "Tests for converting regions to declarations"
  (let ((scenarios '((("" "component")  . nil)
		     (("User" "component") . ("component u as \"User\""))
		     (("User" "participant" t) . ("participant \"User\" as u"))
		     (("User\nRole\nUser Role" "entity" t) . ("entity \"User\" as u"
							      "entity \"Role\" as r"
							      "entity \"User Role\" as ur")))))
    (dolist (scenario scenarios)
      (cl-destructuring-bind (input . expected) scenario
	(should (equal (apply #'plantuml--make-declarations input)
		       expected))))))

(provide 'plantuml-mode-test)
;;; plantuml-mode-test.el ends here
