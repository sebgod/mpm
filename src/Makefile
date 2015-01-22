include ../Make.options
PKG_ROOT =..
USER_MPM_DIR = $(HOME)/.mpm
INSTALL_PREFIX = $(USER_MPM_DIR)/mpm_packages
META_INFO_TMPL = $(PKG_ROOT)/share/meta_info.template

MPM_LOCAL_DEPS := libmercury_mpm/0.*.*
MPM_LOCAL_LIBS := $(foreach DEP,$(MPM_LOCAL_DEPS),$(shell echo $(DEP) | $(EXTRACT) -e $(PKG_LIBNAME_REGEX)))
MPM_DEPS := $(foreach LIB,$(MPM_LOCAL_LIBS),lib$(LIB).local)

.PHONY: default
default: mpm.exe

%.exe: %.meta_info.m $(MPM_DEPS)
	@echo MMC --make $* --ml-local $(MPM_LOCAL_DEPS)
	@$(MMC) --make $* \
		$(foreach DEP,$(MPM_LOCAL_DEPS),--mld $(INSTALL_PREFIX)/$(DEP)/lib/mercury) \
		$(foreach LIB,$(MPM_LOCAL_LIBS),--ml $(LIB))

lib%.local: %.meta_info.m Mercury.modules
	@echo "MMC --install-prefix=$(INSTALL_PREFIX) --make lib$*.install"
	@$(MMC) --install-prefix="$(INSTALL_PREFIX)/lib$*/$(shell $(EXTRACT) -e $(SEMVER_REGEX) $<)" \
		--make lib$*.install

Mercury.modules: $(wildcard *.m)
	@echo "MMC -f $^"
	@$(MMC) -f $^

.PRECIOUS: %.meta_info.m
%.meta_info.m :: $(PKG_ROOT)/lib%.package $(META_INFO_TMPL)
	@echo GEN $*.meta_info.m
	@$(SED) -e "s/<MODULE>/$*/g" < $(META_INFO_TMPL) >$@
	@$(CAT) $< >>$@

%.meta_info.m :: $(PKG_ROOT)/%.package $(META_INFO_TMPL)
	@echo GEN $*.meta_info.m
	@$(SED) -e "s/<MODULE>/$*/g" < $(META_INFO_TMPL) >$@
	@$(CAT) $< >>$@

.PHONY: clean
clean:
	@echo MMC mercury_mpm.clean
	@$(MMC) --make mercury_mpm.clean
	@echo MMC mpm.clean
	@$(MMC) --make mpm.clean

.PHONY: realclean
realclean:
	@echo MMC mercury_mpm.realclean
	@$(MMC) --make mercury_mpm.realclean
	@echo MMC mpm.realclean
	@$(MMC) --make mpm.realclean
	@echo RM Mercury code directory
	@$(RM) -rf Mercury
	@echo RM *.err
	@$(RM) -f $(wildcard *.err)
	@echo RM *.mh
	@$(RM) -f $(wildcard *.mh)
	@echo RM *.meta_info.m
	@$(RM) -f $(wildcard *.meta_info.m)
