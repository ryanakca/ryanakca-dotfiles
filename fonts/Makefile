fonts = Brill_Typeface_2.06 \
	InconsolataHellenic.otf \
	ubuntu-font-family-0.80 \
	Courier-Prime \
	Fell \
	SourceCodePro_FontsOnly-1.017 \
	Vollkorn

all: $(fonts)

clean:
	rm -f $(fonts)

install: $(fonts)
	[ -d ~/.fonts ] || mkdir -p ~/.fonts
	rsync -av $^ ~/.fonts
	fc-cache ~/.fonts

# Requires fontforge
InconsolataHellenic.otf: Inconsolata-Hellenic/Inconsolata-Hellenic.sfd convert.pe
	fontforge -script convert.pe $<
	mv -f $(<:.sfd=.otf) $@

.PHONY: all clean install
