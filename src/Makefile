include ../Make.options
PKG_ROOT := ".."

MPM_LOCAL_DEPS := mercury_mpm/0.*.*
MPM_LOCAL_PKGS := $(foreach DEP,$(MPM_LOCAL_DEPS),$(shell echo $(DEP) | $(EXTRACT) $(PKG_NAME_REGEX)))

.PHONY: default
default: mpm

mpm: $(patsubst %,lib%.local-install,$(MPM_LOCAL_PKGS))
	@echo MMC --make $@ --ml-local $(MPM_LOCAL_DEPS)
	@$(MMC) --make $@ \
		$(foreach DEP,$(MPM_LOCAL_DEPS),--mld $(PKG_ROOT)/mpm_packages/$(DEP)/lib/mercury) \
		$(foreach PKG,$(MPM_LOCAL_PKGS),--ml $(PKG))

lib%.local-install: %.meta_info.m Mercury.modules
	@echo "MMC --install-prefix=$(PKG_ROOT)/mpm_packages/$*/VERSION --make lib$*.install"
	@$(MMC) --install-prefix="$(PKG_ROOT)/mpm_packages/$*/$(shell $(EXTRACT) $(SEMVER_REGEX) $<)" \
		--make lib$*.install

Mercury.modules: $(wildcard *.m)
	@echo "MMC -f $^"
	@$(MMC) -f $^

.PRECIOUS: %.meta_info.m
%.meta_info.m: $(PKG_ROOT)/%.package meta_info.template
	@echo GEN $*.meta_info.m
	@sed -e "s/<MODULE>/$*/g" < meta_info.template >$@
	@cat  $< >>$@

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
	@$(RM) -f $(wildcard *.meta_info.m)
