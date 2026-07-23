.PHONY: compile clean autoloads test generate

PUML_JAR ?= $(shell pwd)/plantuml.jar

KWEL = plantuml-keywords.el
OIEL = plantuml-openiconic.el
CLEL = plantuml-colors.el
WORKDIR = work
STDLIB_BASE = $(WORKDIR)/stdlib
AWSEL = plantuml-stdlib-aws.el
AZEL  = plantuml-stdlib-azure.el
GCPEL = plantuml-stdlib-gcp.el

compile:
	emacs -Q --batch -L . -f batch-byte-compile *.el

clean:
	rm -f *.elc plantuml-mode-autoloads.el $(KWEL) $(OIEL) $(CLEL) $(AWSEL) $(AZEL) $(GCPEL)

autoloads:
	emacs -Q -L . -l compile -l package --batch \
	--eval '(package-generate-autoloads "plantuml-mode" default-directory)'

test:
	emacs -Q --batch -L . -l ert \
	-l plantuml-mode.el \
	-l test/plantuml-mode-test.el \
	-f ert-run-tests-batch-and-exit

generate: $(KWEL) $(OIEL) $(CLEL) stdlib

stdlib: $(AWSEL) $(AZEL) $(GCPEL)

$(KWEL):
	emacs -q -x scripts/generate-keywords.el "$(KWEL)"

$(OIEL):
	emacs -q -x scripts/generate-openiconic.el "$(OIEL)"

$(CLEL):
	emacs -q -x scripts/generate-colors.el "$(CLEL)"

$(AWSEL): $(STDLIB_BASE)/awslib20
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/awslib20" "$(AWSEL)"

$(AZEL): $(STDLIB_BASE)/azure
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/azure" "$(AZEL)"

$(GCPEL): $(STDLIB_BASE)/gcp
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/gcp" "$(GCPEL)"

clean-work:
	rm -fr $(WORKDIR)

$(STDLIB_BASE):
	mkdir -p $(WORKDIR)
	cd $(WORKDIR) ; jar xf $(PUML_JAR) stdlib META-INF
	mv $(STDLIB_BASE) $(WORKDIR)/spm

prepare:
	uv venv --clear
	uv pip install --system-certs -r ./scripts/requirements.txt

$(STDLIB_BASE)/awslib20: $(STDLIB_BASE) prepare
	uv run ./scripts/extract_puml_spm.py $(WORKDIR)/spm/awslib20 $(STDLIB_BASE)

$(STDLIB_BASE)/gcp: $(STDLIB_BASE) prepare
	uv run ./scripts/extract_puml_spm.py $(WORKDIR)/spm/gcp $(STDLIB_BASE)

$(STDLIB_BASE)/azure: $(STDLIB_BASE) prepare
	uv run ./scripts/extract_puml_spm.py $(WORKDIR)/spm/azure $(STDLIB_BASE)
