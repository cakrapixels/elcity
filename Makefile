.PHONY: run test lint compile clean

EMACS ?= emacs-30.1
EL_FILES := $(shell find . -maxdepth 1 -name "elcity*.el" -print | sort)

run: clean compile
	eask exec $(EMACS) -Q -l elcity.el -f elcity-start

test: clean
	eask test ert-runner

lint: clean
	eask lint package
	eask lint checkdoc
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(EL_FILES)

compile: clean
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(EL_FILES)

clean:
	find . -name "*.elc" -o -name "*.eln" | xargs -r rm -f
