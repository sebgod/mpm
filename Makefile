.PHONY: default
default: src

include Make.options

.PHONY: src
src: $(SELF_TEST)
	cd src && $(MAKE) default

.PHONY: install
install: $(SELF_TEST)
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
