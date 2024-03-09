.PHONY: compile clean autoloads

compile:
	emacs -Q --batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc plantuml-mode-autoloads.el

autoloads:
	emacs -Q -L -l compile . -l package --batch \
	--eval '(package-generate-autoloads "plantuml-mode" default-directory)'
