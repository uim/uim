#!/bin/sh

MAKE=make

UIM_REPOSITORY="http://anonsvn.freedesktop.org/svn/uim"
SSCM_REPOSITORY="${UIM_REPOSITORY}/sigscheme-trunk"
LIBGCROOTS_REPOSITORY="${UIM_REPOSITORY}/libgcroots-trunk"
TAGS_REPOSITORY="${UIM_REPOSITORY}/tags"
SSCM_URL="${TAGS_REPOSITORY}/sigscheme-0.7.2"
#SSCM_URL="${SSCM_REPOSITORY}"
LIBGCROOTS_URL="${TAGS_REPOSITORY}/libgcroots-0.1.2"
#LIBGCROOTS_URL="${LIBGCROOTS_REPOSITORY}"

CONF_COMMON="--enable-maintainer-mode"
CONF_NONE="$CONF_COMMON --disable-debug --disable-fep --disable-emacs --disable-dict --disable-scim --disable-gnome-applet --without-anthy --without-canna --without-mana --without-prime --without-m17nlib --without-gtk2 --without-gnome2 --without-qt --without-qt-immodule --disable-compat-scm --without-eb --without-libedit"
CONF_DEFAULT="$CONF_COMMON"
CONF_FULL="$CONF_COMMON --enable-debug --enable-fep --enable-emacs --enable-dict --enable-scim --enable-gnome-applet --with-anthy --with-canna --with-mana --with-prime --with-m17nlib --with-gtk2 --with-gnome2 --with-qt --with-qt-immodule --enable-compat-scm --with-eb --with-libedit"


svn export $SSCM_URL sigscheme
svn export $LIBGCROOTS_URL sigscheme/libgcroots
(cd sigscheme/libgcroots && ./autogen.sh) \
 && (cd sigscheme && ./autogen.sh) \
 && ./autogen.sh

for conf_args in "$CONF_NONE" "$CONF_DEFAULT" "$CONF_FULL"; do
    echo "configure $conf_args"
    ./configure $conf_args && $MAKE distcheck sum \
      || { echo 'make distcheck failed'; exit 1; }
done
