plantuml-mode.elc:
	emacs -Q --batch -L . -f batch-byte-compile plantuml-mode.el

clean:
	rm -f plantuml-mode.elc
