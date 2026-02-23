.PHONY: compile clean autoloads test generate

KWEL = scripts/plantuml-keywords.el
OIEL = scripts/plantuml-openiconic.el
CLEL = scripts/plantuml-colors.el

compile:
	emacs -Q --batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc plantuml-mode-autoloads.el $(KWEL) $(OIEL) $(CLEL)

autoloads:
	emacs -Q -L . -l compile -l package --batch \
	--eval '(package-generate-autoloads "plantuml-mode" default-directory)'

test:
	emacs -Q --batch -L . -l ert \
	-l plantuml-mode.el \
	-l test/plantuml-mode-test.el \
	-f ert-run-tests-batch-and-exit

generate: $(KWEL) $(OIEL) $(CLEL)

$(KWEL):
	emacs -q -x scripts/generate-keywords.el "$(KWEL)"

$(OIEL):
	emacs -q -x scripts/generate-openiconic.el "$(OIEL)"

$(CLEL):
	emacs -q -x scripts/generate-colors.el "$(CLEL)"
