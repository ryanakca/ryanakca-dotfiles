#!gmake -f
SUBSTS_FILE=SUBSTS.local

# Files that need changes to work locally or that contain sensitive
# information
LOCAL_FILES = \
    .imapfilter/config.lua \
    .mutt/accounts.rc \
    .netrc \
    .offlineimaprc \
    .xmonad/xmonad.hs \
    .zshrc \

# Files that are system independent.
# IMPORTANT: directories must have trailing slash
GLOBAL_FILES = \
    .Xdefaults \
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
    .quiltrc-dpkg \
    .screenrc \
    .screenlayout/ \
    .signature \
    .vim/ \
    .vimrc \
    .xinitrc \
    .xkb/ \
    .xmobarrc \
    .xmonad/ \
    .zsh/ \
    .zsh_logout \
    bin/ \

get-val = $(shell awk '{if (match($$0, /$1/)) { print $$2 } }' $(SUBSTS_FILE))

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

# This target relies on GLOBAL_FILES being before LOCAL_FILES so that the
# build/LOCAL_FILES targets overwrite what was copied in GLOBAL_FILES.
BUILD = $(patsubst %,build/%,$(GLOBAL_FILES) $(LOCAL_FILES))

build: $(BUILD)

build/.offlineimaprc: .offlineimaprc $(SUBSTS_FILE)
	[ -d build ] || mkdir build
	sed -e 's/LOCAL_PASS/$(LOCAL_PASS)/g' \
	    -e 's/GMAIL_PASS/$(GMAIL_PASS)/g' \
	    -e 's/QUEENSU_PASS/$(QUEENSU_PASS)/g' $< > $@

build/.imapfilter/config.lua: .imapfilter/config.lua $(SUBSTS_FILE)
	[ -d build/.imapfilter ] || mkdir -p build/.imapfilter
	sed -e 's/LOCAL_PASS/$(LOCAL_PASS)/g' \
	    -e 's/PM_EMAIL/$(PM_EMAIL)/g' $< > $@

build/.netrc: .netrc $(SUBSTS_FILES)
	[ -d build ] || mkdir build
	sed -e 's/LOCAL_PASS/$(LOCAL_PASS)/g' $< > $@

build/.mutt/accounts.rc: .mutt/accounts.rc $(SUBSTS_FILE)
	[ -d build/.mutt ] || mkdir -p build/.mutt
	sed -e 's/LOCAL_PASS/$(LOCAL_PASS)/g' \
	    -e 's/GMAIL_PASS/$(GMAIL_PASS)/g' \
	    -e 's/QUEENSU_PASS/$(QUEENSU_PASS)/g' $< > $@

build/.xinitrc: .xinitrc $(SUBSTS_FILE)
	[ -d build ] || mkdir build
	sed -e 's/SCREENLAYOUT/$(SCREENLAYOUT)/g' $< > $@

build/.xmonad/xmonad.hs: .xmonad/xmonad.hs $(SUBSTS_FILE)
	[ -d build/.xmonad ] || mkdir -p build/.xmonad
	sed -e 's/XMONAD_DZEN_W/$(XMONAD_DZEN_W)/g' \
	    -e 's/XMONAD_DZEN_X/$(XMONAD_DZEN_X)/g' \
	    -e 's/XMONAD_DZEN_y/$(XMONAD_DZEN_Y)/g' $< > $@

build/.zshrc: .zshrc $(SUBSTS_FILE)
	[ -d $(dir $@) ] || mkdir -p $(dir $@)
	sed -e 's/LOCALE/$(LOCALE)/g' $< > $@

build/%: %
	[ -d $(dir $@) ] || mkdir -p $(dir $@)
	rsync -a $< $@

install: $(BUILD)
	rsync -a build/ ~

sha256sums: $(LOCAL_FILES) $(GLOBAL_FILES) dm
	$(SHA256) `git ls-files | grep -v $@` > $@

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
