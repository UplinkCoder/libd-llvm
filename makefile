DMD ?= dmd
ARCHFLAG ?= 
DFLAGS = $(ARCHFLAG) -w -debug -gc -unittest

LIBD_LLVM_ROOT = .

include makefile.common

clean:
	@rm -rf obj lib

.PHONY: clean
