# -*- mode: makefile-gmake; -*-
project := filtercsvxacts

STACK := stack

install-root := $(shell $(STACK) path --local-install-root)
executable := $(install-root)/bin/$(project)-exe
sources := Setup.hs $(wildcard app/*.hs src/*.hs)
tests := $(wildcard test/*.hs)

$(executable) : $(sources)
	$(STACK) build

.PHONY : clean
clean :
	-rm -r $(project).cabal .stack-work

.PHONY : test
test :
	$(STACK) test
