# Simple PlantUML Mode #

Major mode for [PlantUML](https://plantuml.com/) files.

Supports

- Xref navigation of component definitions & references
- Generating png preview files using the plantuml jar & java

Adds the following keybindings

| Keybinding                | Feature                                                            |
|---------------------------|--------------------------------------------------------------------|
| <kbd>C-c C-c</kbd>        | Compile diagram to check for errors                                |
| <kbd>C-c C-p</kbd>        | Generate diagram preview                                           |
| <kbd>C-c C-o</kbd>        | Generate diagram preview & open in external image viewer           |
| <kbd>C-c !</kbd>          | Select diagram type when starting a diagram                        |
| <kbd>C-c i</kbd>          | Insert a deployment diagram element with automatic alias generated |
| <kbd>C-u C-c i</kbd>      | Insert diagram element with prompt for alias                       |
| <kbd>M-&lt;up&gt;</kbd>   | Move line up                                                       |
| <kbd>M-&lt;down&gt;</kbd> | Move line down                                                     |


## Why? ##

While there is already an excellent [plantuml-mode](https://github.com/skuro/plantuml-mode/) available what is the reason of this package existing?

- Did not like `planutml-init` loading language keywords for each session
- Did not like preview buffer popping up new window distracting the diagramming session
- Wanted errors in plantuml compilation to be easily accessible via the compilation-mode
- Wanted quick creation of plantuml diagram components
- Wanted jump-to-definition & list-references within the file

## Installation ##

### Pre-requisites ###

1. Ensure a compatible Java Runtime Environment (JRE) is installed

2. Install the latest PlantUML jar file from https://github.com/plantuml/plantuml

### For Emacs ###

1. Clone the repository into site-lisp in the emacs user directory

	``` sh
	git clone https://github.com/xshyamx/simple-plantuml-mode \
	    $HOME/.emacs.d/site-lisp/simple-plantuml-mode
	```

2. Add to `load-path`

    ```emacs-lisp
    (add-to-list
    	'load-path
    	(expand-file-name "site-lisp/simple-plantuml-mode" user-emacs-directory))
    ```

3. Load & configure

	```emacs-lisp
	(require 'plantuml-mode)
	(setq plantuml-java-cmd "java"
		  plantuml-jar-path
		  (expand-file-name
		   "~/.m2/repository/net/sourceforge/plantuml/plantuml/1.2022.12/plantuml-1.2022.12.jar"))
	```
