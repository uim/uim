pixmapsdir =  $(datadir)/uim/pixmaps

# input methods
SVGS = direct.svg skk.svg tcode.svg tutcode.svg

# input modes
SVGS += direct_input.svg on.svg off.svg \
        ja_direct.svg ja_hiragana.svg ja_katakana.svg \
        ja_halfwidth_katakana.svg \
        ja_halfwidth_alnum.svg ja_fullwidth_alnum.svg \
        prime_mode_application.svg

# Japanese kana input methods
SVGS += ja_romaji.svg ja_kana.svg ja_azik.svg ja_nicola.svg ja_pocketbell.svg

# Tools
SVGS += uim-dict.svg

SVG_PNGS = direct.png skk.png tcode.png tutcode.png \
           direct_input.png on.png off.png \
           ja_direct.png ja_hiragana.png ja_katakana.png \
           ja_halfwidth_katakana.png \
           ja_halfwidth_alnum.png ja_fullwidth_alnum.png \
           prime_mode_application.png \
           ja_romaji.png ja_kana.png ja_azik.png ja_nicola.png \
           ja_pocketbell.png \
	   uim-dict.png

NATIVE_PNGS = switcher-icon.png configure-qt.png uim-icon.png null.png \
              anthy.png canna.png prime.png

PNGS = $(NATIVE_PNGS) $(SVG_PNGS)

dist_pixmaps_DATA = $(PNGS) $(SVGS)

EXTRA_DIST = indication-id.txt

.svg.png:
	if test -n "$(RSVG)"; then		\
	    $(RSVG) -w48 -h48 -fpng $< $@;	\
	fi

clean-svgpng:
	rm -f $(SVG_PNGS)
