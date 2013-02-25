ROOT=$(shell pwd)/..
export ROOT
EMACSD=$(ROOT)/build/.emacs.d/
export ELISPDIR

SUBDIRS = auto-complete \
	  auto-indent-mode \
	  dictem \
	  magit \
	  markdown-mode \
	  popup \
	  proofgeneral \
	  vm
#	  color-theme-6.6.0 \

MAKEDIRS = $(SUBDIRS:%=make-%)
INSTALLDIRS = $(SUBDIRS:%=install-%)
CLEANDIRS = $(SUBDIRS:%=clean-%)

all: subdirs

subdirs: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@:make-%=%)
	touch $@

install: $(INSTALLDIRS)
install-% : make-%
$(INSTALLDIRS):
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) install DIR=$(EMACSD)/$(@:install-%=%)

clean: $(CLEANDIRS)
	rm -f make-*

$(CLEANDIRS):
	$(MAKE) -C $(@:clean-%=%) clean

# Manual overrides:
make-auto-indent-mode make-markdown-mode make-popup make-dictem:
	touch $@

clean-auto-indent-mode clean-markdown-mode clean-popup make-dictem:
	echo $@

make-auto-complete:
	$(MAKE) -C $(@:make-%=%) -e EMACS="emacs -L ../popup/"
	touch $@

make-proofgeneral:
	$(MAKE) -C $(@:make-%=%) compile

make-vm:
	cd $(@:make-%=%) && autoconf && ./configure --prefix=$(EMACSD)
	$(MAKE) -C $(@:make-%=%)

install-auto-complete:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e DIR=$(EMACSD)/$(@:install-%=%) install

install-auto-indent-mode:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	cp $(@:install-%=%)/auto-indent-mode.el $(EMACSD)/$(@:install-%=%)

install-dictem:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/*.el $(EMACSD)/$(@:install-%=%)
	
install-color-theme-6.6.0:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e ELISPDIR=$(EMACSD)/$(@:install-%=%) install

install-magit:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e PREFIX="" -e DESTDIR=$(EMACSD) \
		-e SYSCONFDIR="" -e ELISP_INSTALL_DIR=$(EMACSD)/$(@:install-%=%) install
install-markdown-mode:
	mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/markdown-mode.el $(EMACSD)/$(@:install-%=%)/

install-popup:
	mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/popup.el $(EMACSD)/$(@:install-%=%)/

install-proofgeneral:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e ELISP=$(EMACSD)/$(@:install-%=%) install-el install-elc

install-vm:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e lispdir=$(EMACSD)/$(@:install-%=%) install

clean-magit:
	$(MAKE) -C $(@:clean-%=%) clean

clean-vm:
	$(MAKE) -C $(@:clean-%=%) distclean

.PHONY: all install clean $(CLEANDIRS)
