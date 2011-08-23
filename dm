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
    .muttrc \
    .netrc \
    .offlineimaprc \
    .screenrc \
    .xmonad/xmonad.hs \
    .zsh/func/prompt_wunjo_setup \
    .zshrc \

# Files that are system independent.
# IMPORTANT: directories must have trailing slash
GLOBAL_FILES = \
    .Xdefaults \
    .Xsession \
    .config/nitrogen/ \
    .dzen/ \
    .gitconfig \
    .imapfilter/ \
    .inputrc \
    .local/share/wallpapers/ \
    .mailcheckrc \
    .mutt/ \
    .muttrc \
    .notmuch-config \
    .offlineimap.py \
    .pythonrc.py \
    .quiltrc-dpkg \
    .sbuildrc \
    .screenlayout/ \
    .signature \
    .vim/ \
    .vimperatorrc \
    .vimrc \
    .xinitrc \
    .xkb/ \
    .xmobarrc \
    .xmonad/ \
    .zsh/ \
    .zsh_logout \
    bin/ \

get-val = $(shell awk '{if (match($$0, /$1/)) { print $$2 } }' $(SUBSTS_FILE))
get-sed-args = $(foreach var,$($(1)),-e 's|$(var)|$($(var))|g')

CURRENT_BRANCH = $(shell git branch --no-color | colrm 1 2)

SHA256        = $(call get-val,SHA256)

LOCAL_PASS    = $(call get-val,LOCAL_PASS)
GMAIL_PASS    = $(call get-val,GMAIL_PASS)
QUEENSU_PASS  = $(call get-val,QUEENSU_PASS)
PM_EMAIL      = $(call get-val,PM_EMAIL)
SCREENLAYOUT  = $(call get-val,SCREENLAYOUT)
XMONAD_DZEN_W = $(call get-val,XMONAD_DZEN_W)
XMONAD_DZEN_X = $(call get-val,XMONAD_DZEN_X)
XMONAD_DZEN_Y = $(call get-val,XMONAD_DZEN_Y)
LOCALE	      = $(call get-val,LOCALE)
SUBSTS_LS     = $(call get-val,SUBSTS_LS)
MSMTP_PATH    = $(call get-val,MSMTP_PATH)
ZSH_PATH      = $(call get-val,ZSH_PATH)
ZSH_HOST_COLOUR = $(call get-val,ZSH_HOST_COLOUR)
SCREEN_HOST_COLOUR = $(call get-val,SCREEN_HOST_COLOUR)
SUBSTS_RM     = $(call get-val,SUBSTS_RM)

VARS_.devscripts        = MSMTP_PATH
VARS_.gitconfig         = MSMTP_PATH
VARS_.imapfilter/config.lua = LOCAL_PASS PM_EMAIL
VARS_.msmtprc           = GMAIL_PASS QUEENSU_PASS
VARS_.mutt/accounts.rc  = LOCAL_PASS GMAIL_PASS QUEENSU_PASS MSMTP_PATH
VARS_.muttrc            = MSMTP_PATH
VARS_.netrc             = LOCAL_PASS
VARS_.offlineimaprc     = LOCAL_PASS GMAIL_PASS QUEENSU_PASS
VARS_.screenrc          = ZSH_PATH SCREEN_HOST_COLOUR
VARS_.xmonad/xmonad.hs  = XMONAD_DZEN_W XMONAD_DZEN_X XMONAD_DZEN_Y
VARS_.zsh/func/prompt_wunjo_setup = ZSH_HOST_COLOUR
VARS_.zshrc             = LOCALE SUBSTS_RM SUBSTS_LS MSMTP_PATH

all: clean build

# This target relies on GLOBAL_FILES being before LOCAL_FILES so that the
# build/LOCAL_FILES targets overwrite what was copied in GLOBAL_FILES.
BUILD = $(patsubst %,build/%,$(GLOBAL_FILES) $(LOCAL_FILES))

build: $(BUILD)

# We must force these with a phony target, otherwise, make will see that they're
# already there (for example, from installing the rest of .mutt or .zsh) and
# will skip them---which means they don't get their substitutions
build/.imapfilter/config.lua: FORCE
build/.mutt/accounts.rc: FORCE
build/.xmonad/xmonad.hs: FORCE
build/.zsh/func/prompt_wunjo_setup: FORCE
FORCE:

build/%: % $(SUBSTS_FILE)
	[ -d $(dir $@) ] || mkdir -p $(dir $@)
	rsync -a $< $@
	@# sed will only be called if $* is in LOCAL_FILES.
	@# Thought of using ifeq/ifneq, but the gmake manual reads:
	@# "make evaluates conditionals when it reads a makefile. Consequently,
	@# you cannot use automatic variables in the tests of conditionals
	@# because they are not defined until recipes are run (see Automatic
	@# Variables)."
	[ "$(filter $*,$(LOCAL_FILES))" != "$*" ] || sed $(call get-sed-args,VARS_$*) $< > $@

install: $(BUILD)
	rsync -a build/ ~

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

clean:
	rm -fr build

.PHONY: build install clean verify merge
