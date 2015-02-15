# vim: ft=make tw=78 ts=4 sw=4 noet

PKG_ROOT :=..

.PHONY: default
default:
	cd lib && $(MAKE) default
	cd bin && $(MAKE) default

include $(PKG_ROOT)/Make.options
include $(PKG_ROOT)/mpm.options

.PHONY: install
install: default install_share install_etc install_bin

.PHONY: install_share
install_share: $(wildcard $(PKG_ROOT)/src/share/*)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_SHARE_DIR)"
	@$(INSTALL_FILES) -t "$(MPM_SHARE_DIR)" -p $^

.PHONY: install_etc
install_etc: $(wildcard $(PKG_ROOT)/*.options)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_ETC_DIR)"
	@$(INSTALL_FILES_COMPARE) -t "$(MPM_ETC_DIR)" --backup=existing $^

.PHONY: install_bin
install_bin:
	@cd bin && $(MAKE) install

.PHONY: clean
clean:
	cd lib && $(MAKE) clean
	cd bin && $(MAKE) clean

.PHONY: distclean
distclean: realclean

.PHONY: realclean
realclean:
	cd lib && $(MAKE) realclean
	cd bin && $(MAKE) realclean
