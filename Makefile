# EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACS = emacs

CASK = ~/.cask/bin/cask
CASKEMACS = $(CASK) exec $(EMACS)
LOAD =  -L org-mode/lisp -l org-query.el -l test.el

# http://stackoverflow.com/questions/3931741/why-does-make-think-the-target-is-up-to-date
.PHONY: cask

all: test

cask:
	$(shell EMACS=$(EMACS) $(CASK))

compile:
	$(CASKEMACS) -q  $(LOAD) org-query.el \
	--eval "(progn (mapc #'byte-compile-file '(\"org-query.el\")) (switch-to-buffer \"*Compile-Log*\") (ert t))"

test:
	$(CASKEMACS) -batch ${LOAD} --eval="(message (concat \"Org version: \" (org-version) \" on Emacs version: \" (emacs-version)))"
	$(CASKEMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
