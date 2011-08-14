SUBSTS_FILE=SUBSTS.local

# Files that need changes to work locally or that contain sensitive
# information
LOCAL_FILES = \
    .imapfilter/config.lua \
    .mutt/accounts.rc \
    .netrc \
    .offlineimaprc \
    .xmonad/xmonad.hs \

# Files that are system independent.
# IMPORTANT: directories must have trailing slash
GLOBAL_FILES = \
    .Xdefaults \
    .config/nitrogen/ \
    .dzen/ \
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
    .xmobarrc \
    .xmonad/ \
    .zsh/ \
    .zsh_logout \
    .zshrc \
    bin/ \

get-val = $(shell awk '{if (match($$0, /$1/)) { print $$2 } }' $(SUBSTS_FILE))

LOCAL_PASS    = $(call get-val,LOCAL_PASS)
GMAIL_PASS    = $(call get-val,GMAIL_PASS)
QUEENSU_PASS  = $(call get-val,QUEENSU_PASS)
PM_EMAIL      = $(call get-val,PM_EMAIL)
SCREENLAYOUT  = $(call get-val,SCREENLAYOUT)
XMONAD_DZEN_W = $(call get-val,XMONAD_DZEN_W)
XMONAD_DZEN_X = $(call get-val,XMONAD_DZEN_X)
XMONAD_DZEN_Y = $(call get-val,XMONAD_DZEN_Y)

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

build/%: %
	[ -d $(dir $@) ] || mkdir -p $(dir $@)
	rsync -a $< $@

install: $(BUILD)
	rsync -a build/ ~/

sha256sums: $(LOCAL_FILES) $(GLOBAL_FILES) Makefile
	sha256sum `git ls-files | grep -v sha256sums` > $@

sha256sums.asc: sha256sums
	rm -f $@
	gpg --clearsign --detach-sign $<

verify:
	sha256sum -c sha256sums
	gpg --verify sha256sums.asc

clean:
	rm -fr build

.PHONY: build install clean verify
