#!gmake -f
SUBSTS_FILE=SUBSTS.local

# Files that need changes to work locally or that contain sensitive
# information
LOCAL_FILES = \
    .config/beets/config.yaml \
    .config/catgirl/bitlbee \
    .config/catgirl/libera \
    .config/catgirl/oftc \
    .config/catgirl/sdf \
    .config/catgirl/tilde \
    .mutt/accounts.rc \
    .mutt/gpg.rc \
    .mutt/ssl.rc \
    .s-nail.rc \
    .netrc \
    .xinitrc \
    .xmonad/xmonad.hs \
    .zsh/func/prompt_wunjo_setup \
    .zshrc

# GPG encrypted files. Must be included in GLOBAL_FILES
GPG_FILES = \
    .config/wpa_supplicant.conf \
    .mutt/alias.rc \
    .mutt/alias-cmu.rc \
    .mutt/savehooks.rc

# Files that are system independent.
# IMPORTANT: directories must have trailing slash
GLOBAL_FILES = \
    .Xresources \
    .Xsession \
    .asoundrc \
    .cache/mailx/ \
    .caffrc \
    .calendar/ \
    .config/ \
    .cmus/ \
    .dput.cf \
    .dzen/ \
    .emacs \
    .emacs.d \
    .gitconfig \
    .gitconfig-debian \
    .inputrc \
    .latexmkrc \
    .lbdbrc \
    .local/ \
    .mailcap \
    .mailcheckrc \
    .mailrc \
    .mutt/ \
    .mutt/alias.rc \
    .mutt/alias-cmu.rc \
    .mutt/savehooks.rc \
    .muttrc \
    .notmuch-config \
    .pulse/ \
    .pythonrc.py \
    .quiltrc-dpkg \
    .reportbugrc \
    .sbuildrc \
    .screenlayout/ \
    .signature \
    .ssh/ \
    .tmux.conf \
    .tmux-mail.conf \
    .urxvt/ \
    .vim/ \
    .vimrc \
    .vm \
    .xinitrc \
    .xkb/ \
    .xmobarrc \
    .xmonad/ \
    .zsh/ \
    .zshenv \
    .zlogout \
    bin/ \

get-val = $(if $(C_$1),$(C_$1),$(shell egrep '^$1' $(SUBSTS_FILE) | cut -f2- -d' '))
get-sed-args = $(foreach var,$($(1)),-e 's|$(var)|$(call get-val,$(var))|g')

CURRENT_BRANCH = $(shell git branch --no-color | colrm 1 2)

SHA256        = $(call get-val,SHA256)
GPG_DISABLED  = $(call get-val,GPG_DISABLED)
EMACS_DISABLED= $(call get-val,EMACS_DISABLED)
BUILD_FONTS   = $(call get-val,BUILD_FONTS)
C_SENDMAIL      = $(shell command -v sendmail)

MAIL_PASS = GMAIL_PASS QUEENSU_PASS RYANAKCA_PASS LOCAL_PASS CMU_PASS SOCS_PASS

VARS_.config/beets/config.yaml = MBUSER MBPASS
VARS_.config/catgirl/bitlbee = LOCALHOST
VARS_.config/catgirl/libera = LOCALHOST
VARS_.config/catgirl/oftc = LOCALHOST
VARS_.config/catgirl/sdf = LOCALHOST
VARS_.config/catgirl/tilde = LOCALHOST
VARS_.mutt/accounts.rc  = $(MAIL_PASS)
VARS_.mutt/gpg.rc       = PGPEWRAP_BINARY
VARS_.mutt/ssl.rc       = SSL_CERTS
VARS_.s-nail.rc		= SENDMAIL SSL_CERTS
VARS_.netrc             = $(MAIL_PASS)
VARS_.xinitrc           = SCREENLAYOUT PULSE
VARS_.xmonad/xmonad.hs  = XMONAD_DZEN_W XMONAD_DZEN_X XMONAD_DZEN_Y
VARS_.zsh/func/prompt_wunjo_setup = ZSH_HOST_COLOUR
VARS_.zshrc             = LOCALE SUBSTS_RM SUBSTS_LS KEYCHAIN

all: clean build

# This target relies on GLOBAL_FILES being before LOCAL_FILES so that the
# build/LOCAL_FILES targets overwrite what was copied in GLOBAL_FILES.
BUILD = $(patsubst %,build/%,$(GLOBAL_FILES) $(LOCAL_FILES) $(GPG_FILES))
LOCALS = $(patsubst %,build/%,$(LOCAL_FILES))

build: $(BUILD) fonts

# We must force these with a phony target, otherwise, make will see that they're
# already there (for example, from installing the rest of .mutt or .zsh) and
# will skip them---which means they don't get their substitutions
build/.mutt/accounts.rc: FORCE
build/.xmonad/xmonad.hs: FORCE
build/.zsh/func/prompt_wunjo_setup: FORCE
FORCE:

.mutt/alias.rc: gpg/.mutt/alias.rc.gpg
.mutt/alias-cmu.rc: gpg/.mutt/alias-cmu.rc.gpg
.mutt/savehooks.rc: gpg/.mutt/savehooks.rc.gpg
.ssh/id_%: gpg/.ssh/id_%.gpg
# $(patsubst gpg/,,$(wildcard gpg/.* gpg/*))
$(GPG_FILES):
	for f in $@ ; do \
	    touch $$f; \
	    chmod 600 $$f; \
	    [ "$(GPG_DISABLED)" = "True" ] || gpg --decrypt gpg/$$f.gpg > $$f; \
	done

emacsen:
	[ "$(EMACS_DISABLED)" = "True" ] || $(MAKE) -C $@
	[ "$(EMACS_DISABLED)" = "True" ] || $(MAKE) -C $@ install

fonts:
	[ "$(BUILD_FONTS)" != "True" ] || $(MAKE) -C .fonts install

$(LOCALS): .FORCE
build/%: % $(SUBSTS_FILE)
	@[ -d $(dir $@) ] || mkdir -p $(dir $@)
	( [ -d $< ] && rsync -a $</* $@/ ) || rsync -a $< $@
	@# sed will only be called if $* is in LOCAL_FILES.
	@# Thought of using ifeq/ifneq, but the gmake manual reads:
	@# "make evaluates conditionals when it reads a makefile. Consequently,
	@# you cannot use automatic variables in the tests of conditionals
	@# because they are not defined until recipes are run (see Automatic
	@# Variables)."
	@[ "$(filter $*,$(LOCAL_FILES))" != "$*" ] || sed $(call get-sed-args,VARS_$*) $< > $@

install: build
	-diff -u ~/.mutt/alias.rc build/.mutt/alias.rc
	rsync -a build/ ~
	chmod 600 ~/.netrc ~/.ssh/id_*
	chmod 700 ~/.ssh
	-[ "$(BUILD_FONTS)" != "True" ] || fc-cache ~/.fonts

merge: SUBSTS $(SUBSTS_FILE)
	# sdiff has exit status 1 if files are different. Ignore
	- sdiff -o SUBSTS.merged $^
	@echo ""
	@echo "Please review SUBSTS.merged, then run"
	@echo "mv SUBSTS.merged $(SUBSTS_FILE)"

udh: udh-master

# As a backup for when master changes
udh-%:
	# -L to follow symlinks
	rsync -avzL $(@:udh-%=%).debian.org:/var/lib/misc/ssh_known_hosts .ssh/known_hosts.d/debian

clean:
	rm -fr build
	$(MAKE) -C emacsen clean

clobber: clean
	rm -f $(GPG_FILES)
	$(MAKE) -C emacsen clobber

diff: build
	for file in $(GLOBAL_FILES) $(LOCAL_FILES); do \
	    diff -u build/$${file} ../$$file; \
	done

ugpg/%:
	cp -f ../$* $* && \
	    gpg -r4E469519ED677734268FBD958F7BF8FC4A11C97A --encrypt $* && \
	    mv -f $*.gpg gpg/$*.gpg

aliases: ugpg/.mutt/alias.rc ugpg/.mutt/alias-cmu.rc

.FORCE:
.PHONY: build install clean verify merge udh emacsen diff .FORCE aliases
