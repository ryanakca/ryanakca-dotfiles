ROOT=$(shell pwd)/..
export ROOT
EMACSD=$(ROOT)/build/.emacs.d/
export ELISPDIR
EMACSEN=$(ROOT)/emacsen

SUBDIRS = auto-complete \
	  auto-indent-mode \
	  bbdb \
	  dictem \
	  haskell-mode \
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
	install-info $(EMACSD)/share/info/*.info $(EMACSD)/share/info/dir

install-% : make-%
$(INSTALLDIRS):
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) install DIR=$(EMACSD)/$(@:install-%=%)

clean: $(CLEANDIRS)
	rm -f make-*

$(CLEANDIRS):
	[ ! -f $(@:clean-%=%)/Makefile ] || $(MAKE) -C $(@:clean-%=%) clean

# Manual overrides:
make-auto-indent-mode make-markdown-mode make-popup make-dictem:
	touch $@

clean-auto-indent-mode clean-markdown-mode clean-popup clean-dictem:
	echo $@

make-auto-complete:
	$(MAKE) -C $(@:make-%=%) -e EMACS="emacs -L ../popup/"
	touch $@

make-bbdb: make-vm
	cd $(@:make-%=%) && autoconf && ./configure --prefix=$(EMACSD) --without-tex-dir --with-vm-dir=$(EMACSEN)/vm/lisp
	$(MAKE) -C $(@:make-%=%)
	touch $@

make-haskell-mode:
	$(MAKE) -C $(@:make-%=%) compile
	touch $@

make-proofgeneral:
	$(MAKE) -C $(@:make-%=%) compile
	touch $@

make-vm:
	cd $(@:make-%=%) && autoconf && ./configure --prefix=$(EMACSD) --with-other-dirs=$(EMACSEN)/bbdb/lisp
	$(MAKE) -C $(@:make-%=%)
	touch $@

install-auto-complete:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e DIR=$(EMACSD)/$(@:install-%=%) install

install-auto-indent-mode:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	cp $(@:install-%=%)/auto-indent-mode.el $(EMACSD)/$(@:install-%=%)

install-bbdb:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e lispdir=$(EMACSD)/$(@:install-%=%) install

install-dictem:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/*.el $(EMACSD)/$(@:install-%=%)
	
install-color-theme-6.6.0:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) -e ELISPDIR=$(EMACSD)/$(@:install-%=%) install

install-haskell-mode: make-haskell-mode
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/*.elc $(EMACSD)/$(@:install-%=%)

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

clean-vm:
	[ ! -f $(@:clean-%=%)/Makefile ] || $(MAKE) -C $(@:clean-%=%) distclean

clobber:
	for dir in $(SUBDIRS); do \
	    git --git-dir=$${dir}/.git reset --hard HEAD; \
	    git --git-dir=$${dir}/.git clean -qxdf; \
	done

.PHONY: all install clean $(CLEANDIRS) clobber
