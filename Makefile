.PHONY: compile clean autoloads test generate

KWEL = scripts/plantuml-keywords.el
OIEL = scripts/plantuml-openiconic.el
CLEL = scripts/plantuml-colors.el
WORKDIR = work
STDLIB_BASE = $(WORKDIR)/stdlib
AWSEL = scripts/plantuml-stdlib-aws.el
AZEL = scripts/plantuml-stdlib-azure.el
GCPEL = scripts/plantuml-stdlib-gcp.el

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

$(STDLIB_BASE):
	mkdir -p $(STDLIB_BASE)

$(STDLIB_BASE)/aws: $(STDLIB_BASE)
	[ -d $(STDLIB_BASE)/aws ] || git clone https://github.com/awslabs/aws-icons-for-plantuml.git -b v14.0 $(STDLIB_BASE)/aws

$(STDLIB_BASE)/azure: $(STDLIB_BASE)
	[ -d $(STDLIB_BASE)/azure ] || git clone https://github.com/plantuml-stdlib/Azure-PlantUML.git -b v2.2 $(STDLIB_BASE)/azure

$(STDLIB_BASE)/gcp: $(STDLIB_BASE)
	[ -d $(STDLIB_BASE)/gcp ] || git clone https://github.com/Crashedmind/PlantUML-icons-GCP.git $(STDLIB_BASE)/gcp

$(AWSEL): $(STDLIB_BASE)/aws
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/aws" "$(AWSEL)"

$(AZEL): $(STDLIB_BASE)/azure
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/azure" "$(AZEL)"

$(GCPEL): $(STDLIB_BASE)/gcp
	emacs -q -x scripts/generate-stdlib.el "$(STDLIB_BASE)/gcp" "$(GCPEL)"
