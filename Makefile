.PHONY: compile clean autoloads test generate

PUML_VERSION ?= 1.2026.6
PUML_JAR ?= $(shell pwd)/plantuml-$(PUML_VERSION).jar

KWEL = plantuml-keywords.el
OIEL = plantuml-openiconic.el
CLEL = plantuml-colors.el
WORKDIR = work
STDLIB_BASE = $(WORKDIR)/stdlib
AWSEL = plantuml-stdlib-aws.el
AZEL  = plantuml-stdlib-azure.el
GCPEL = plantuml-stdlib-gcp.el

BUILDDIR = $(WORKDIR)/classes

compile:
	emacs -Q --batch -L . -f batch-byte-compile *.el

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	rm -rf *.elc $(WORKDIR) plantuml-mode-autoloads.el

autoloads:
	emacs -Q -L . -l compile -l package --batch \
	--eval '(package-generate-autoloads "plantuml-mode" default-directory)'

test:
	emacs -Q --batch -L . -l ert \
	-l plantuml-mode.el \
	-l test/plantuml-mode-test.el \
	-f ert-run-tests-batch-and-exit

clean-generated:
	rm -f $(KWEL) $(OIEL) $(CLEL) $(AWSEL) $(AZEL) $(GCPEL)

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

EXTRACT_CLASS = ExtractPumlSpm
EXTRACT_JAVA  = scripts/$(EXTRACT_CLASS).java
EXTRACT_CP    = $(PUML_JAR):$(BUILDDIR)

$(STDLIB_BASE):
	mkdir -p $(WORKDIR)
	cd $(WORKDIR) ; jar xf $(PUML_JAR) stdlib META-INF
	mv $(STDLIB_BASE) $(WORKDIR)/spm

$(BUILDDIR)/$(EXTRACT_CLASS).class: $(EXTRACT_JAVA) $(BUILDDIR)
	javac -cp $(PUML_JAR) -d $(BUILDDIR) $(EXTRACT_JAVA)

$(STDLIB_BASE)/awslib20: $(STDLIB_BASE) $(BUILDDIR)/$(EXTRACT_CLASS).class
	java -cp $(EXTRACT_CP) $(EXTRACT_CLASS) $(WORKDIR)/spm/awslib20 $(STDLIB_BASE)

$(STDLIB_BASE)/gcp: $(STDLIB_BASE) $(BUILDDIR)/$(EXTRACT_CLASS).class
	java -cp $(EXTRACT_CP) $(EXTRACT_CLASS) $(WORKDIR)/spm/gcp $(STDLIB_BASE)

$(STDLIB_BASE)/azure: $(STDLIB_BASE) $(BUILDDIR)/$(EXTRACT_CLASS).class
	java -cp $(EXTRACT_CP) $(EXTRACT_CLASS) $(WORKDIR)/spm/azure $(STDLIB_BASE)

plantuml-$(PUML_VERSION).jar:
	mvn dependency:get \
		-DgroupId=net.sourceforge.plantuml \
		-DartifactId=plantuml \
		-Dversion=$(PUML_VERSION)
	cp $(HOME)/.m2/repository/net/sourceforge/plantuml/plantuml/$(PUML_VERSION)/plantuml-$(PUML_VERSION).jar .
