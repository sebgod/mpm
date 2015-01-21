include ../Make.options

MPM_PACKAGES_DIR :=../mpm_packages

MPM_LOCAL_DEPS := mercury_mpm/0.*.*
MPM_LOCAL_PKGS := $(foreach DEP,$(MPM_LOCAL_DEPS),$(shell echo $(DEP) | $(EXTRACT) $(PKG_NAME_REGEX)))

MERCURY_MODULES := $(wildcard *.m)

.PHONY: default
default: mpm

mpm: $(patsubst %,lib%.local-install,$(MPM_LOCAL_PKGS))
	@echo MMC --make $@ --ml-local $(MPM_LOCAL_DEPS)
	@$(MMC) --make $@ \
		$(foreach DEP,$(MPM_LOCAL_DEPS),--mld $(MPM_PACKAGES_DIR)/$(DEP)/lib/mercury)  \
		$(foreach PKG,$(MPM_LOCAL_PKGS),--ml $(PKG))

lib%.local-install: %.meta_info.m Mercury.modules
	@echo "MMC --install-prefix=$(MPM_PACKAGES_DIR)/$*/VERSION --make lib$*.install"
	@$(MMC) --no-verbose-make --install-prefix="$(MPM_PACKAGES_DIR)/$*/$(shell $(EXTRACT) $(SEMVER_REGEX) $<)" \
		--make lib$*.install

Mercury.modules: $(MERCURY_MODULES)
	$(MMC) -f $^

%.meta_info.m: %.package
	@echo GEN $*.meta_info.m
	@echo ":- module $*.meta_info."                >$@
	@echo ":- interface."                         >>$@
	@echo ":- pred version(string::out) is det."  >>$@
	@echo ":- pred revision(string::out) is det." >>$@
	@echo ":- implementation."                    >>$@
	@cat  $<                                      >>$@
	@echo "revision(\"<NOT IMPLEMENTED>\")."      >>$@
	@echo ":- end_module $*.meta_info."           >>$@

.PHONY: install
install:
	$(MMC) --make libmercury_mpm.install

tags: $(wildcard *.m)
	mtags $^

.PHONY: clean
clean:
	$(MMC) --make mercury_mpm.clean
	$(MMC) --make mpm.clean

.PHONY: realclean
realclean:
	$(MMC) --make mercury_mpm.realclean
	$(MMC) --make mpm.realclean
	/bin/rm -rf Mercury
	/bin/rm -f $(wildcard *.err) $(wildcard *.mh)
	/bin/rm -f tags
