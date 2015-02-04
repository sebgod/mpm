include Make.options

.PHONY: default
default:
	cd src && $(MAKE) lib_exe

.PHONY: install
install:
	cd src && $(MAKE) install

.PHONY: runtests
runtests:
	cd tests && $(MAKE) runtests

.PHONY: distclean
distclean: realclean

.PHONY: realclean
realclean:
	cd src && $(MAKE) realclean
	cd tests && $(MAKE) realclean
