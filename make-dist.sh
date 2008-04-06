#!/bin/sh

MAKE=make

UIM_REPOSITORY="http://uim.googlecode.com/svn"
SSCM_REPOSITORY="${UIM_REPOSITORY}/sigscheme-trunk"
LIBGCROOTS_REPOSITORY="${UIM_REPOSITORY}/libgcroots-trunk"
TAGS_REPOSITORY="${UIM_REPOSITORY}/tags"
#SSCM_URL="${SSCM_REPOSITORY}"
SSCM_URL="${TAGS_REPOSITORY}/sigscheme-0.8.2"
#LIBGCROOTS_URL="${LIBGCROOTS_REPOSITORY}"
LIBGCROOTS_URL="${TAGS_REPOSITORY}/libgcroots-0.2.1"
RELEASE_SUFFIX="-beta"

CONF_COMMON="--enable-maintainer-mode --disable-warnings-into-error"
CONF_NONE="$CONF_COMMON --disable-debug --disable-fep --disable-emacs --disable-gnome-applet --disable-kde-applet --disable-pref --disable-dict --without-anthy --without-canna --without-mana --without-prime --without-m17nlib --without-scim --without-gtk2 --without-gnome2 --without-qt --without-qt-immodule --disable-compat-scm --without-eb --without-libedit"
CONF_DEFAULT="$CONF_COMMON"
# --without-scim since it is broken
# FIXME: hardcoded --with-eb-conf
CONF_FULL="$CONF_COMMON --enable-debug --enable-fep --enable-emacs --enable-gnome-applet --enable-kde-applet --enable-pref --enable-dict --enable-notify --with-anthy --with-canna --with-wnn --with-sj3 --with-mana --with-prime --with-m17nlib --without-scim --with-gtk2 --with-gnome2 --with-qt --with-qt-immodule --enable-compat-scm --with-eb --with-eb-conf=/etc/eb.conf --with-libedit"


svn export $SSCM_URL sigscheme
svn export $LIBGCROOTS_URL sigscheme/libgcroots
(cd sigscheme/libgcroots && ./autogen.sh) \
 && (cd sigscheme && ./autogen.sh) \
 && ./autogen.sh \
|| { echo 'autogen failed.' && exit 1; }

if test -n "$RELEASE_SUFFIX"; then
    ed Makefile.in <<EOT
/^distdir =
d
i
RELEASE_SUFFIX = ${RELEASE_SUFFIX}
#distdir = \$(PACKAGE)-\$(VERSION)
distdir = \$(PACKAGE)-\$(VERSION)\$(RELEASE_SUFFIX)
.
wq
EOT
fi

for conf_args in "$CONF_NONE" "$CONF_DEFAULT" "$CONF_FULL"; do
    echo "configure $conf_args"
    ./configure $conf_args && $MAKE distcheck sum \
      || { echo 'make distcheck failed'; exit 1; }
done
