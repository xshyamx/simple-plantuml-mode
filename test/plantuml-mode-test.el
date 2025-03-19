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
		     ("One \n Two & Three " . "ott")
				 ("one_two_three" . "ott"))))
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

(defun plantuml--temp-dir ()
	"Return a temporary directory for plantuml testing"
	(expand-file-name (make-temp-name "plantuml")
										(temporary-file-directory)))

(defmacro plantuml--file-test (&rest body)
	"Run test with a temporary directory cleaning up after test
completes. Provides `basedir' for creating test files"
	(declare (indent 1) (debug t))
	`(let* ((basedir ,(plantuml--temp-dir))
					(default-directory basedir)
					(plantuml-jar-path basedir))
		 (unwind-protect
				 (progn
					 (mkdir (expand-file-name "plantuml" basedir) t)
					 ,@body)
			 (delete-directory basedir t))))

(ert-deftest plantuml--include-file-open ()
  "Test for opening included file"
	(plantuml--file-test
			(let* ((plantuml-jar-path basedir)
						 (include-file (expand-file-name "inc.plantuml" basedir))
						 (test-file (expand-file-name "test.plantuml" basedir)))
				(with-temp-buffer
					(insert "included file")
					(write-file include-file))
				(with-temp-buffer
					(insert (string-join
									 '("@startuml"
										 "!include inc.plantuml"
										 "@enduml")
									 "\n"))
					(write-file test-file))
				(find-file test-file)
				(with-current-buffer (get-file-buffer test-file)
					(goto-char 0)
					(forward-line)
					(call-interactively #'plantuml-open-include-file))
				(should (get-file-buffer include-file)))))

(ert-deftest plantuml--add-title ()
	"Test adding title based on filename"
	(plantuml--file-test
	 (let ((scenarios '(("one" "@startuml\n@enduml\n" t)
											("two" "@startuml\ntitle One\n@enduml\n" nil))))
		 (dolist (scenario scenarios)
			 (cl-destructuring-bind (slug content match) scenario
				 (let ((file (expand-file-name (concat slug ".plantuml") basedir))
							 (m))
					 (with-temp-buffer
						 (insert content)
						 (write-file file))
					 (find-file file)
					 (with-current-buffer (get-file-buffer file)
						 (call-interactively #'plantuml-add-title)
						 (goto-char 0)
						 (setq m (re-search-forward
											(rx-to-string
											 `(: bol "title " ,(plantuml--make-title slug)))
											nil t))
						 (if match
								 (should (> m 0))
							 (should (eq nil m))))))))))

(ert-deftest plantuml--add-footer ()
	"Test adding footer based on the date"
	(plantuml--file-test
	 (let ((scenarios '(("one" "@startuml\n@enduml\n" t)
											("two" "@startuml\nfooter One\n@enduml\n" nil))))
		 (dolist (scenario scenarios)
			 (cl-destructuring-bind (slug content match) scenario
				 (let ((file (expand-file-name (concat slug ".plantuml") basedir))
							 (m))
					 (with-temp-buffer
						 (insert content)
						 (write-file file))
					 (find-file file)
					 (with-current-buffer (get-file-buffer file)
						 (call-interactively #'plantuml-add-footer)
						 (goto-char 0)
						 (setq m (re-search-forward
											(rx-to-string `(: bol ,(format-time-string "footer %Y-%m-%d")))
											nil t))
						 (if match
								 (should (> m 0))
							 (should (eq nil m))))))))))


(ert-deftest plantuml--update-header ()
	"Test adding header"
	(plantuml--file-test
	 (let ((scenarios '(("one" "@startuml\n@enduml\n" t)
											("two" "@startuml\nheader Anything\n@enduml\n" nil))))
		 (dolist (scenario scenarios)
			 (cl-destructuring-bind (slug content match) scenario
				 (let ((file (expand-file-name (concat slug ".plantuml") basedir))
							 (m))
					 (with-temp-buffer
						 (insert content)
						 (write-file file))
					 (find-file file)
					 (with-current-buffer (get-file-buffer file)
						 (call-interactively #'plantuml-add-header)
						 (goto-char 0)
						 (setq m (re-search-forward
											(rx bol "header Draft" eol)
											nil t))
						 (if match
								 (should (> m 0))
							 (should (eq nil m))))))))))

(ert-deftest plantuml--update-footer-hook ()
	"Test the update footer hook"
	(let* ((basedir (plantuml--temp-dir))
				 (default-directory basedir)
				 (plantuml-jar-path basedir))
		(unwind-protect
				(progn
					(mkdir basedir t)
					(let ((file (expand-file-name "one.plantuml" basedir))
								(buffer))
						(with-temp-buffer
							(insert "@startuml\n"
											"footer 2020-12-25\n"
											"@enduml\n")
							(write-file file))
						(setq buffer
									(find-file file))
						(with-current-buffer buffer
							(add-hook 'before-save-hook #'plantuml-update-footer-date nil t)
							(goto-char 0)
							(forward-line 1)
							(insert "skinparam defaultTextAlignment center\n")
							(save-buffer))
						(with-temp-buffer
							(insert-file-contents file)
							(goto-char 0)
							(should (re-search-forward (format-time-string "footer %Y-%m-%d") nil t)))
						)
					)
			(delete-directory basedir t)
			)))

(provide 'plantuml-mode-test)
;;; plantuml-mode-test.el ends here
