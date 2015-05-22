# vim: ft=make tw=78 ts=4 sw=4 noet

MPM := mpm
DEFAULT_TARGET := bootstrap_install
INSTALL_TARGET := $(DEFAULT_TARGET)

ifeq ($(firstword $(shell $(MPM) --version 2>&1)),mpm:)
    DEFAULT_TARGET := src
    INSTALL_TARGET := mpm_install
endif

.PHONY: default
default: $(DEFAULT_TARGET)

include Make.options

.PHONY: src
src: $(SELF_TEST)
	cd src && $(MAKE) default

.PHONY: install
install: $(INSTALL_TARGET)

.PHONY:
mpm_install:
	$(MPM) install

.PHONY: bootstrap_install
bootstrap_install: $(SELF_TEST)
	cd src && $(MAKE) install

.PHONY: runtests
runtests: $(SELF_TEST)
	@echo run all tests in tests dir
	@cd tests && $(MAKE) runtests

.PHONY: distclean
distclean: realclean

.PHONY: realclean
realclean: $(SELF_TEST)
	cd src && $(MAKE) realclean
	cd tests && $(MAKE) realclean
