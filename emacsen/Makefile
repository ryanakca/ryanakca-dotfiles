ROOT=$(shell pwd)/..
export ROOT
EMACSD=$(ROOT)/build/.emacs.d/
export ELISPDIR

SUBDIRS = auto-complete \
	  auto-indent-mode \
	  magit
#	  color-theme-6.6.0 \
#	  ProofGeneral-4.1

INSTALLDIRS = $(SUBDIRS:%=install-%)
CLEANDIRS = $(SUBDIRS:%=clean-%)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

install: $(INSTALLDIRS)
$(INSTALLDIRS):
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) install DIR=$(EMACSD)/$(@:install-%=%)

clean: $(CLEANDIRS)
$(CLEANDIRS):
	$(MAKE) -C $(@:clean-%=%) clean

# Manual overrides:
auto-indent-mode clean-auto-indent-mode:
	echo $@

install-auto-complete:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e DIR=$(EMACSD)/$(@:install-%=%) install

install-auto-indent-mode:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	cp $(@:install-%=%)/auto-indent-mode.el $(EMACSD)/$(@:install-%=%)
	
install-color-theme-6.6.0:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e ELISPDIR=$(EMACSD)/$(@:install-%=%) install

install-magit:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e PREFIX="" -e DESTDIR=$(shell pwd)/$(@:install-%=%) \
		-e SYSCONFDIR="" -e ELISP_INSTALL_DIR=$(EMACSD)/$(@:install-%=%) install

clean-magit:
	$(MAKE) -C $(@:clean-%=%) clean
	rm -fr $(@:clean-%=%)/share

all: subdirs

.PHONY: all subdirs $(SUBDIRS) install $(INSTALLDIRS) clean $(CLEANDIRS)
