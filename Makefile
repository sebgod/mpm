include Make.options

.PHONY: default
default:
	cd src && $(MAKE) default

.PHONY: runtests
runtests:
	cd tests && $(MAKE) runtests

.PHONY: realclean
realclean:
	cd src && $(MAKE) realclean
	cd tests && $(MAKE) realclean
