# vim: ft=make tw=78 ts=4 sw=4 noet

# PKG_ROOT is the root of this package (i.e. git repository)
PKG_ROOT :=../..

.PHONY: default
default: bin

include ../Makefile.src_shared

# EXE_EXT is defined only after Make.options is included
.PHONY: bin
bin: mpm$(EXE_EXT)

.PHONY: install
install: mpm.install

.PHONY: clean
clean: mpm.clean

.PHONY: realclean
realclean: mpm.realclean
