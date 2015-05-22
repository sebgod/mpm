# vim: ft=make tw=78 ts=4 sw=4 noet

PKG_ROOT :=..

MERCURY_SRC_DIRS :=mercury-slre lib bin

.PHONY: default
default: $(patsubst %,default_%,$(MERCURY_SRC_DIRS))

include $(PKG_ROOT)/Make.options
include $(PKG_ROOT)/mpm.options

.PHONY: install
install: $(patsubst %,install_%,share etc lib bin)

.PHONY: install_share
install_share: $(wildcard $(PKG_ROOT)/src/share/*)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_SHARE_DIR)"
	@$(INSTALL) -t "$(MPM_SHARE_DIR)" -p $^

.PHONY: install_etc
install_etc: $(wildcard $(PKG_ROOT)/*.options)
	@echo INSTALL $^
	@$(MKDIR) -p "$(MPM_ETC_DIR)"
	@$(INSTALL_COMPARE) -t "$(MPM_ETC_DIR)" --backup=existing $^

.PHONY: clean
clean: $(patsubst %,clean_%,$(MERCURY_SRC_DIRS))

.PHONY: distclean
distclean: realclean

.PHONY: realclean
realclean: $(patsubst %,realclean_%,$(MERCURY_SRC_DIRS))

default_%: %
	cd "$*" && $(MAKE) default

install_%: %
	cd "$*" && $(MAKE) install

clean_%: %
	cd "$*" && $(MAKE) clean

realclean_%: %
	cd "$*" && $(MAKE) realclean

