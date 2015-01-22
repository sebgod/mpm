include ../Make.options
PKG_ROOT := ".."

MPM_LOCAL_DEPS := libmercury_mpm/0.*.*
MPM_LOCAL_LIBS := $(foreach DEP,$(MPM_LOCAL_DEPS),$(shell echo $(DEP) | $(EXTRACT) -e $(PKG_LIBNAME_REGEX)))
MPM_DEPS := $(patsubst %,%.lib-local-install,$(MPM_LOCAL_LIBS))

.PHONY: default
default: mpm

mpm: $(MPM_DEPS)
	@echo MMC --make $@ --ml-local $(MPM_LOCAL_DEPS)
	@$(MMC) --make $@ \
		$(foreach DEP,$(MPM_LOCAL_DEPS),--mld $(PKG_ROOT)/mpm_packages/$(DEP)/lib/mercury) \
		$(foreach LIB,$(MPM_LOCAL_LIBS),--ml $(LIB))

%.lib_local: %.lib_meta_info.m Mercury.modules
	@echo "MMC --install-prefix=$(PKG_ROOT)/mpm_packages/$*/VERSION --make lib$*.install"
	@$(MMC) --install-prefix="$(PKG_ROOT)/mpm_packages/$*/$(shell $(EXTRACT) -e $(SEMVER_REGEX) $<)" \
		--make lib$*.install

Mercury.modules: $(wildcard *.m)
	@echo "MMC -f $^"
	@$(MMC) -f $^

.PRECIOUS: %.lib_meta_info.m
%.lib_meta_info.m: $(PKG_ROOT)/lib%.package lib_meta_info.template
	@echo GEN $*.lib_meta_info.m
	@$(SED) -e "s/<MODULE>/$*/g" < lib_meta_info.template >$@
	@$(CAT) $< >>$@

.PHONY: install
install:
	$(MMC) --make libmercury_mpm.install

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
	@$(RM) -f $(wildcard *.*_meta_info.m)
