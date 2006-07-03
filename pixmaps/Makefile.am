pixmapsdir =  $(datadir)/uim/pixmaps

SVGS = unknown.svg

# input methods
SVGS += direct.svg skk.svg tcode.svg tutcode.svg byeoru.svg

# input modes
SVGS += direct_input.svg on.svg off.svg \
        ja_direct.svg ja_hiragana.svg ja_katakana.svg \
        ja_halfwidth_katakana.svg \
        ja_halfwidth_alnum.svg ja_fullwidth_alnum.svg \
        ja_ascii_with_preedit.svg \
        prime_mode_application.svg \
	ko_direct.svg ko_hangulchar.svg ko_hangulword.svg

# Japanese kana input methods
SVGS += ja_romaji.svg ja_kana.svg ja_azik.svg ja_nicola.svg ja_pocketbell.svg

# Tools
SVGS += uim-dict.svg

# input method substitutions
IM_SUBST_SVGS = py.svg pyunihan.svg pinyin-big5.svg \
                mana.svg \
                hangul2.svg hangul3.svg romaja.svg \
                viqr.svg ipa-x-sampa.svg latin.svg \
                scim.svg
SVGS += im_subst.svg $(IM_SUBST_SVGS)

IM_SUBST_PNGS = py.png pyunihan.png pinyin-big5.png \
                mana.png \
                hangul2.png hangul3.png romaja.png \
                viqr.png ipa-x-sampa.png latin.png \
                scim.png

SVG_PNGS = unknown.png \
           direct.png skk.png tcode.png tutcode.png byeoru.png \
           direct_input.png on.png off.png \
           ja_direct.png ja_hiragana.png ja_katakana.png \
           ja_halfwidth_katakana.png \
           ja_halfwidth_alnum.png ja_fullwidth_alnum.png \
           ja_ascii_with_preedit.png \
           prime_mode_application.png \
	   ko_direct.png ko_hangulchar.png ko_hangulword.png \
           ja_romaji.png ja_kana.png ja_azik.png ja_nicola.png \
           ja_pocketbell.png \
	   uim-dict.png $(IM_SUBST_PNGS)

NATIVE_PNGS = null.png uim-icon64.png uim-icon48.png uim-gray.png \
              switcher-icon.png \
              anthy.png canna.png prime.png

PNGS = $(NATIVE_PNGS) $(SVG_PNGS) uim-icon.png

dist_pixmaps_DATA = $(PNGS) $(SVGS)

EXTRA_DIST = indication-id.txt

uim-icon.png: uim-icon48.png
	cp $< $@

$(IM_SUBST_SVGS): im_subst.svg
	cp $< $@

.svg.png:
	if test -n "$(RSVG)"; then		\
	    $(RSVG) -w48 -h48 -fpng $< $@;	\
	fi

clean-svgpng:
	rm -f $(SVG_PNGS)

# rsvg(1) is required to make a distribution, but does not for build
# and install from a tarball distribution.
dist-hook:
	@test -n "$(RSVG)" || (echo 'rsvg(1) not found: install it'; exit 1)

if M17NLIB
install-data-hook:
	for f in $(m17n_db_dir)/icons/*.png; do \
	  if test -f "$$f"; then \
	    $(LN_S) -f "$$f" $(DESTDIR)$(pkgdatadir)/pixmaps/m17n-`basename "$$f"`; \
	  fi \
	done
endif
