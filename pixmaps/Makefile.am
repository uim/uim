pixmapsdir = @uim_pixmapsdir@

if M17NLIB
bin_SCRIPTS = uim-m17nlib-relink-icons
endif

SVGS = unknown.svg
# dark background
SVGS += unknown_dark_background.svg

# input methods
SVGS += direct.svg skk.svg tcode.svg tutcode.svg byeoru.svg wb86.svg zm.svg
# dark background
SVGS += direct_dark_background.svg \
        skk_dark_background.svg \
        wb86_dark_background.svg zm_dark_background.svg

# input modes
SVGS += direct_input.svg on.svg off.svg \
        ja_direct.svg ja_hiragana.svg ja_katakana.svg \
        ja_halfkana.svg \
        ja_halfwidth_alnum.svg ja_fullwidth_alnum.svg \
        prime_mode_application.svg \
        ko_direct.svg ko_hangulchar.svg ko_hangulword.svg
# dark background
SVGS += direct_input_dark_background.svg \
        on_dark_background.svg \
        off_dark_background.svg \
        ja_direct_dark_background.svg \
        ja_hiragana_dark_background.svg \
        ja_katakana_dark_background.svg \
        ja_halfkana_dark_background.svg \
        ja_halfwidth_alnum_dark_background.svg \
        ja_fullwidth_alnum_dark_background.svg \
        prime_mode_application_dark_background.svg \
        ko_direct_dark_background.svg \
        ko_hangulchar_dark_background.svg \
        ko_hangulword_dark_background.svg

# Japanese kana input methods
SVGS += ja_romaji.svg ja_kana.svg ja_azik.svg ja_act.svg ja_kzik.svg \
        ja_nicola.svg ja_pocketbell.svg
# dark background
SVGS += ja_romaji_dark_background.svg \
        ja_kana_dark_background.svg \
        ja_azik_dark_background.svg \
        ja_act_dark_background.svg \
        ja_kzik_dark_background.svg

# Tools
SVGS += im_switcher.svg uim-dict.svg

# input method substitutions
IM_SUBST_SVGS = py.svg pyunihan.svg pinyin-big5.svg \
                mana.svg \
                hangul2.svg hangul3.svg romaja.svg \
                viqr.svg ipa-x-sampa.svg latin.svg elatin.svg \
                look.svg scim.svg sj3.svg wnn.svg \
                social-ime.svg ajax-ime.svg yahoo-jp.svg
GENERATED_SVGS = $(IM_SUBST_SVGS) trycode.svg
SVGS += im_subst.svg $(GENERATED_SVGS)

IM_SUBST_PNGS = py.png pyunihan.png pinyin-big5.png \
                mana.png \
                hangul2.png hangul3.png romaja.png \
                viqr.png ipa-x-sampa.png latin.png elatin.png \
                look.png scim.png sj3.png wnn.png \
                social-ime.png ajax-ime.png yahoo-jp.png

SVG_PNGS = unknown.png \
           direct.png skk.png tcode.png tutcode.png trycode.png byeoru.png \
           direct_input.png on.png off.png \
           ja_direct.png ja_hiragana.png ja_katakana.png \
           ja_halfkana.png \
           ja_halfwidth_alnum.png ja_fullwidth_alnum.png \
           prime_mode_application.png \
           ko_direct.png ko_hangulchar.png ko_hangulword.png \
           ja_romaji.png ja_kana.png ja_azik.png ja_act.png ja_kzik.png \
           ja_nicola.png \
           ja_pocketbell.png \
           wb86.png zm.png \
           im_switcher.png uim-dict.png $(IM_SUBST_PNGS) \
           unknown_dark_background.png \
           direct_dark_background.png \
           skk_dark_background.png \
           wb86_dark_background.png \
           zm_dark_background.png \
           direct_input_dark_background.png \
           on_dark_background.png \
           off_dark_background.png \
           ja_direct_dark_background.png \
           ja_hiragana_dark_background.png \
           ja_katakana_dark_background.png \
           ja_halfkana_dark_background.png \
           ja_halfwidth_alnum_dark_background.png \
           ja_fullwidth_alnum_dark_background.png \
           prime_mode_application_dark_background.png \
           ko_direct_dark_background.png \
           ko_hangulchar_dark_background.png \
           ko_hangulword_dark_background.png \
           ja_romaji_dark_background.png \
           ja_kana_dark_background.png \
           ja_azik_dark_background.png \
           ja_act_dark_background.png \
           ja_kzik_dark_background.png

NATIVE_PNGS = null.png uim-icon64.png uim-icon48.png uim-gray.png

SCIM_PNGS = scim-anthy.png scim-canna.png scim-prime.png \
	    scim-skk.png scim-wnn.png
SCIM_ORIGINATED_PNGS = anthy.png canna.png prime.png
if ANTHY_UTF8
SCIM_ORIGINATED_PNGS += anthy-utf8.png
endif

GENERATED_PNGS = $(SVG_PNGS) $(SCIM_ORIGINATED_PNGS) uim-icon.png

PNGS = $(NATIVE_PNGS) $(GENERATED_PNGS)

EXTRA_DIST = COPYING.BSDL COPYING.LGPL LIST indication-id.txt $(SCIM_PNGS)

dist_pixmaps_DATA = $(SVGS) $(PNGS)

MAINTAINERCLEANFILES = $(GENERATED_PNGS) $(GENERATED_SVGS)

if MAINTAINER_MODE
anthy.png: scim-anthy.png
	cp $< $@
anthy-utf8.png: scim-anthy.png
	cp $< $@
canna.png: scim-canna.png
	cp $< $@
prime.png: scim-prime.png
	cp $< $@

uim-icon.png: uim-icon48.png
	cp $< $@

$(IM_SUBST_SVGS): im_subst.svg
	cp $< $@

trycode.svg: tcode.svg
	cp $< $@

.svg.png:
	if test -n "$(RSVG)"; then		\
	    $(RSVG) -w48 -h48 -fpng $< $@;	\
	fi
endif

clean-svgpng:
	rm -f $(SVG_PNGS)
clean-gensvg:
	rm -f $(GENERATED_SVGS)
clean-genpng:
	rm -f $(GENERATED_PNGS)

# rsvg(1) is required to make a distribution, but does not for build
# and install from a tarball distribution.
dist-hook:
	@test -n "$(RSVG)" || (echo 'rsvg(1) not found: install it'; exit 1)

install-data-hook:
if M17NLIB
	DESTDIR="$(DESTDIR)" $(SH) $(builddir)/uim-m17nlib-relink-icons
endif
