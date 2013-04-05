fonts = InconsolataHellenic.otf \
	ubuntu-font-family-0.80 \
	Fell \
	Vollkorn

all: $(fonts)

clean:
	rm -f $(fonts)

install: $(fonts)
	[ -d ../build/.fonts ] || mkdir -p ../build/.fonts
	rsync -av $^ ../build/.fonts/

# Requires fontforge
InconsolataHellenic.otf: Inconsolata-Hellenic/Inconsolata-Hellenic.sfd convert.pe
	fontforge -script convert.pe $<
	mv -f $(<:.sfd=.otf) $@

.PHONY: all clean install
