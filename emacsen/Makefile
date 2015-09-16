ROOT=$(shell pwd)/..
export ROOT
EMACSD=$(ROOT)/build/.emacs.d/
export ELISPDIR
EMACSEN=$(ROOT)/emacsen

SUBDIRS = dictem

MAKEDIRS = $(SUBDIRS:%=make-%)
INSTALLDIRS = $(SUBDIRS:%=install-%)
CLEANDIRS = $(SUBDIRS:%=clean-%)

all: subdirs

subdirs: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@:make-%=%)
	touch $@

install: $(INSTALLDIRS)
	[ ! -d $(EMACSD)/share/info ] || install-info $(EMACSD)/share/info/*.info $(EMACSD)/share/info/dir

install-% : make-%
$(INSTALLDIRS):
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	$(MAKE) -C $(@:install-%=%) install DIR=$(EMACSD)/$(@:install-%=%)

clean: $(CLEANDIRS)
	rm -f make-*

$(CLEANDIRS):
	[ ! -f $(@:clean-%=%)/Makefile ] || $(MAKE) -C $(@:clean-%=%) clean

# Manual overrides:
make-dictem:
	touch $@

clean-dictem:
	echo $@

install-dictem:
	-mkdir -p $(EMACSD)/$(@:install-%=%)
	install -m 644 $(@:install-%=%)/*.el $(EMACSD)/$(@:install-%=%)

clobber:
	for dir in $(SUBDIRS); do \
	    git --git-dir=$${dir}/.git reset --hard HEAD; \
	    git --git-dir=$${dir}/.git clean -qxdf; \
	done

.PHONY: all install clean $(CLEANDIRS) clobber
