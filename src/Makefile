# PKG_ROOT is the root of this package (i.e. git repository)
PKG_ROOT :=..
include $(PKG_ROOT)/Make.options
include $(PKG_ROOT)/mpm.options

MI_TMPL := $(PKG_ROOT)/share/meta_info.template

.PHONY: default
default: libmercury_mpm mpm$(EXE_EXT)

.PHONY: install
install: default install_share install_etc install_bin

.PHONY: install_share
install_share: $(wildcard $(PKG_ROOT)/share/*)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_SHARE_DIR)"
	@$(INSTALL) -t "$(MPM_SHARE_DIR)" -p -m 644 $^

.PHONY: install_etc
install_etc: $(wildcard $(PKG_ROOT)/*.options)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_ETC_DIR)"
	@$(INSTALL) -t "$(MPM_ETC_DIR)" --compare --backup=existing -m 644 $^

.PHONY: install_bin
install_bin:
	@echo INSTALL mpm
	@$(MKDIR) -p "$(MPM_BIN_DIR)"
	@$(INSTALL) -t "$(MPM_BIN_DIR)" "$(PKG_ROOT)/src/mpm$(EXE_EXT)"

.PHONY: clean
clean:
	@echo MMC mercury_mpm.clean
	@$(MMC) --make mercury_mpm.clean
	@echo MMC mpm.clean
	@$(MMC) --make mpm.clean

.PHONY: distclean
distclean: realclean

.PHONY: realclean
realclean:
	@echo MMC mercury_mpm.realclean
	@$(MMC) --make mercury_mpm.realclean
	@echo MMC mpm.realclean
	@$(MMC) --make mpm.realclean
	@echo RM Mercury code directory
	@$(DEL_DIR) Mercury
	@echo RM *.err
	@$(DEL_FILE) $(wildcard *.err)
	@echo RM *.mh
	@$(DEL_FILE) $(wildcard *.mh)
	@echo RM *.meta_info.m
	@$(DEL_FILE) $(wildcard *.meta_info.m)

include $(PKG_ROOT)/share/Makefile.generic
