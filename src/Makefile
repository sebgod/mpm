include ../Make.options
PKG_ROOT =..
USER_MPM_DIR = $(HOME)/.mpm
INSTALL_PREFIX = $(USER_MPM_DIR)/mpm_packages
MI_TMPL = $(PKG_ROOT)/share/meta_info.template

.PHONY: default
default: libmercury_mpm mpm$(EXE_EXT)

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
