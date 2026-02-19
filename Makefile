.PHONY: compile clean autoloads test generate

KWEL = scripts/plantuml-keywords.el

compile:
	emacs -Q --batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc plantuml-mode-autoloads.el $(KWEL)

autoloads:
	emacs -Q -L . -l compile -l package --batch \
	--eval '(package-generate-autoloads "plantuml-mode" default-directory)'

test:
	emacs -Q --batch -L . -l ert \
	-l plantuml-mode.el \
	-l test/plantuml-mode-test.el \
	-f ert-run-tests-batch-and-exit

generate: $(KWEL)

$(KWEL):
	emacs -q -x scripts/generate-keywords.el "$(KWEL)"
