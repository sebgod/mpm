include ../Make.options

MPM_PACKAGES_DIR :=../mpm_packages

MPM_LOCAL_DEPS := mercury_mpm/0.*.*
MPM_LOCAL_PKGS := $(foreach DEP,$(MPM_LOCAL_DEPS),$(shell echo $(DEP) | $(EXTRACT) $(PKG_NAME_REGEX)))

.PHONY: default
default: mpm

mpm: $(patsubst %,lib%.local-install,$(MPM_LOCAL_PKGS))
	@echo MMC --make $@ --ml-local $(MPM_LOCAL_DEPS)
	@$(MMC) --make $@ \
		$(foreach DEP,$(MPM_LOCAL_DEPS),--mld $(MPM_PACKAGES_DIR)/$(DEP)/lib/mercury)  \
		$(foreach PKG,$(MPM_LOCAL_PKGS),--ml $(PKG))

lib%.local-install: %.meta_info.m Mercury.modules
	@echo "MMC --install-prefix=$(MPM_PACKAGES_DIR)/$*/VERSION --make lib$*.install"
	@$(MMC) --install-prefix="$(MPM_PACKAGES_DIR)/$*/$(shell $(EXTRACT) $(SEMVER_REGEX) $<)" \
		--no-verbose-make --make lib$*.install

Mercury.modules: $(wildcard *.m)
	@echo "MMC -f $^"
	@$(MMC) -f $^

.PRECIOUS: %.meta_info.m
%.meta_info.m: %.package meta_info.template
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
	@echo RM *.err *.mh tags
	@$(RM) -f $(wildcard *.err) $(wildcard *.mh)
	@$(RM) -f tags
