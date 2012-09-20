#!gmake -f
SUBSTS_FILE=SUBSTS.local

# Files that need changes to work locally or that contain sensitive
# information
LOCAL_FILES = \
    .devscripts \
    .gitconfig \
    .imapfilter/config.lua \
    .msmtprc \
    .mutt/accounts.rc \
    .mutt/score.rc \
    .muttrc \
    .netrc \
    .offlineimaprc \
    .screenrc \
    .xinitrc \
    .xmonad/xmonad.hs \
    .zsh/func/prompt_wunjo_setup \
    .zshrc \
    bin/run-mailcheck

# GPG encrypted files
GPG_FILES = \
    .mutt/alias.rc \
    .ssh/id_ecdsa \
    .ssh/id_rsa \
    .ssh/id_rsa.lambda

# Files that are system independent.
# IMPORTANT: directories must have trailing slash
GLOBAL_FILES = \
    .Xdefaults \
    .Xsession \
    .beetsconfig \
    .config/nitrogen/ \
    .cmus/ \
    .dput.cf \
    .dzen/ \
    .fonts.conf \
    .gitconfig \
    .imapfilter/ \
    .inputrc \
    .local/share/wallpapers/ \
    .mailcap \
    .mailcheckrc \
    .mutt/ \
    .mutt/alias.rc \
    .muttrc \
    .notmuch-config \
    .offlineimap.py \
    .pythonrc.py \
    .quiltrc-dpkg \
    .reportbugrc \
    .sbuildrc \
    .screenlayout/ \
    .screenrc-mail \
    .signature \
    .ssh/ \
    .urxvt/ \
    .vim/ \
    .vimperatorrc \
    .vimrc \
    .xinitrc \
    .xkb/ \
    .xmobarrc \
    .xmonad/ \
    .zsh/ \
    .zlogout \
    bin/ \

get-val = $(shell awk '{if (match($$0, /$1/)) { print $$2 } }' $(SUBSTS_FILE))
get-sed-args = $(foreach var,$($(1)),-e 's|$(var)|$(call get-val,$(var))|g')

CURRENT_BRANCH = $(shell git branch --no-color | colrm 1 2)

SHA256        = $(call get-val,SHA256)
GPG_DISABLED  = $(call get-val,GPG_DISABLED)

VARS_.devscripts        = MSMTP_PATH
VARS_.gitconfig         = MSMTP_PATH
VARS_.imapfilter/config.lua = LOCAL_PASS PM_EMAIL
VARS_.msmtprc           = GMAIL_PASS QUEENSU_PASS LOCALHOST MCGSOCS_PASS MCGILL_PASS SSL_CERTS
VARS_.mutt/accounts.rc  = LOCAL_PASS GMAIL_PASS QUEENSU_PASS MSMTP_PATH MCGILL_PASS
VARS_.mutt/score.rc     = MUTT_10_SCORE MUTT_20_SCORE
VARS_.muttrc            = MSMTP_PATH
VARS_.netrc             = LOCAL_PASS GMAIL_PASS
VARS_.offlineimaprc     = LOCAL_PASS GMAIL_PASS QUEENSU_PASS MCGSOCS_PASS MCGILL_PASS
VARS_.screenrc          = ZSH_PATH SCREEN_HOST_COLOUR
VARS_.xinitrc           = SCREENLAYOUT REDSHIFT_MODE GSD_PATH
VARS_.xmonad/xmonad.hs  = XMONAD_DZEN_W XMONAD_DZEN_X XMONAD_DZEN_Y
VARS_.zsh/func/prompt_wunjo_setup = ZSH_HOST_COLOUR
VARS_.zshrc             = LOCALE SUBSTS_RM SUBSTS_LS MSMTP_PATH KEYCHAIN
VARS_bin/run-mailcheck  = ZSH_PATH GREP_PATH

all: clean build

# This target relies on GLOBAL_FILES being before LOCAL_FILES so that the
# build/LOCAL_FILES targets overwrite what was copied in GLOBAL_FILES.
BUILD = $(patsubst %,build/%,$(GLOBAL_FILES) $(LOCAL_FILES) $(GPG_FILES))

build: $(BUILD)

# We must force these with a phony target, otherwise, make will see that they're
# already there (for example, from installing the rest of .mutt or .zsh) and
# will skip them---which means they don't get their substitutions
build/.imapfilter/config.lua: FORCE
build/.mutt/accounts.rc: FORCE
build/.xmonad/xmonad.hs: FORCE
build/.zsh/func/prompt_wunjo_setup: FORCE
FORCE:

.mutt/alias.rc: gpg/.mutt/alias.rc.gpg
.ssh/id_%: gpg/.ssh/id_%.gpg
# $(patsubst gpg/,,$(wildcard gpg/.* gpg/*))
$(GPG_FILES):
	touch $@ && chmod 600 $@
	[ "$(GPG_DISABLED)" = "True" ] || gpg --decrypt gpg/$@.gpg > $@

build/%: % $(SUBSTS_FILE)
	[ -d $(dir $@) ] || mkdir -p $(dir $@)
	( [ -d $< ] && rsync -a $</* $@/ ) || rsync -a $< $@
	@# sed will only be called if $* is in LOCAL_FILES.
	@# Thought of using ifeq/ifneq, but the gmake manual reads:
	@# "make evaluates conditionals when it reads a makefile. Consequently,
	@# you cannot use automatic variables in the tests of conditionals
	@# because they are not defined until recipes are run (see Automatic
	@# Variables)."
	[ "$(filter $*,$(LOCAL_FILES))" != "$*" ] || sed $(call get-sed-args,VARS_$*) $< > $@

install: $(BUILD)
	rsync -a build/ ~
	chmod 600 ~/.msmtprc ~/.netrc ~/.ssh/id_*
	chmod 700 ~/.ssh

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

udh:
	rsync -avz master.debian.org:/var/lib/misc/master.debian.org/ssh_known_hosts .ssh/known_hosts.d/debian

clean:
	rm -fr build

clobber: clean
	rm -f $(GPG_FILES)
.PHONY: build install clean verify merge udh
