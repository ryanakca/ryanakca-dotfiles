#!gmake -f
SUBSTS_FILE=SUBSTS.local

# Files that need changes to work locally or that contain sensitive
# information
LOCAL_FILES = \
    .config/beets/config.yaml \
    .config/nitrogen/nitrogen.cfg \
    .imapfilter/config.lua \
    .mutt/accounts.rc \
    .mutt/gpg.rc \
    .mutt/ssl.rc \
    .s-nail.rc \
    .netrc \
    .offlineimaprc \
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
    .imapfilter/ \
    .inputrc \
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
    .offlineimap.py \
    .pulse/ \
    .pythonrc.py \
    .quiltrc-dpkg \
    .reportbugrc \
    .sbuildrc \
    .screenlayout/ \
    .signature \
    .ssh/ \
    .tmux/ \
    .tmux.conf \
    .tmux-mail.conf \
    .tmux-powerlinerc \
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
C_SENDMAIL      = $(shell which sendmail)

MAIL_PASS = GMAIL_PASS QUEENSU_PASS RYANAKCA_PASS LOCAL_PASS CMU_PASS

VARS_.config/beets/config.yaml = MBUSER MBPASS
VARS_.config/nitrogen/nitrogen.cfg = HOMEDIR
VARS_.imapfilter/config.lua = LOCAL_PASS IMAPFILTER_GMAIL_SERVER GMAIL_PASS IMAP_FOLDER_SEP IMAPFILTER_LOCAL
VARS_.mutt/accounts.rc  = $(MAIL_PASS)
VARS_.mutt/gpg.rc       = PGPEWRAP_BINARY
VARS_.mutt/ssl.rc       = SSL_CERTS
VARS_.s-nail.rc		= SENDMAIL SSL_CERTS
VARS_.netrc             = $(MAIL_PASS)
VARS_.offlineimaprc     = $(MAIL_PASS) SSL_CERTS
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
build/.imapfilter/config.lua: FORCE
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

sha256sums: .git/refs/heads/$(CURRENT_BRANCH)
	$(SHA256) `git ls-files | grep -v $@` > $@
	[ $(SHA256) = 'sha256' ] || awk '{ \
	    sum = $$1; \
	    $$1 = ""; \
	    # We want (filename), not ( filename). \
	    gsub(/^ /,""); \
	    print "SHA256 (" $$0 ") = " sum; \
	    }' $@ > $@.temp
	mv $@.temp $@

sha256sums.asc: sha256sums
	rm -f $@
	gpg --armor --detach-sign $<

merge: SUBSTS $(SUBSTS_FILE)
	# sdiff has exit status 1 if files are different. Ignore
	- sdiff -o SUBSTS.merged $^
	@echo ""
	@echo "Please review SUBSTS.merged, then run"
	@echo "mv SUBSTS.merged $(SUBSTS_FILE)"

verify:
	# BSD sha256 sum command doesn't have a -c option.
	# BSD and coreutils sha256 commands have different outputs, however, the
	# file is always in field two. The sum's location varies.
	awk --posix 'BEGIN {\
		mismatch_count = 0; \
		match_count = 0; \
	    } { \
		# The file associated with the sum we`re checking \
		# Appears to be field 2 in BSD and coreutils sum \
		file = $$2; \
		# We want `filename`, not `(filename)` \
		sub(/^\(/, "", file); \
		sub(/\)$$/, "", file); \
		# Generate the sum to compare with \
		cmd = "$(SHA256) " file; \
		cmd | getline gensum; \
		close(cmd); \
		split(gensum, gensum_fields); \
		gensum = ""; \
		for (field in gensum_fields) { \
		    if (gensum_fields[field] ~ /[[:xdigit:]]{64}/) { \
			gensum = gensum_fields[field]; \
		    } \
		} \
		for (i = 1; i <= NF; i++) { \
		    if ($$i ~ /[[:xdigit:]]{64}/) { \
			filesum = $$i; \
		    } \
		} \
		if (filesum == gensum) { \
		    print "Match: " file; \
		    match_count += 1; \
		} else { \
		    print "Mismatch: " file; \
		    mismatch_count += 1; \
		} \
	    } END { \
		print match_count, "matches, ", mismatch_count, "mismatches." \
	}' sha256sums
	gpg --verify sha256sums.asc

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
