#!/bin/sh

MAKE=make

SSCM_TAG="sigscheme-0.8.6"
LIBGCROOTS_TAG="libgcroots-0.2.3"

CONF_MAINT="--enable-maintainer-mode"
CONF_NOWERROR="--disable-warnings-into-error"
CONF_COMMON="$CONF_MAINT $CONF_NOWERROR"
CONF_NONE="$CONF_COMMON --disable-debug --disable-fep --disable-emacs --disable-gnome-applet --disable-kde-applet --disable-pref --disable-dict --without-anthy --without-canna --without-mana --without-prime --without-skk --without-x --without-xft --without-m17nlib --without-gtk2 --without-qt --without-qt-immodule --disable-compat-scm --without-eb --without-libedit"
CONF_DEFAULT="$CONF_COMMON"

for file in "/etc/eb.conf" "/usr/lib64/eb.conf" "/usr/lib/eb.conf" \
            "/usr/local/etc/eb.conf" "/usr/etc/eb.conf"
do
    if test -f "$file"; then
        EB_CONF="$file"
        break
    fi
done
if test -z "$EB_CONF"; then
    echo eb.conf not found
    exit 1
fi

# --without-qt
CONF_FULL_WO_MAINT="$CONF_NOWERROR --enable-debug --enable-fep --enable-emacs --enable-gnome-applet --enable-gnome3-applet --enable-kde-applet --enable-kde4-applet --enable-pref --enable-dict --enable-notify=libnotify,knotify4 --with-anthy --with-canna --with-wnn --with-sj3 --with-mana --with-prime --with-m17nlib --with-gtk2 --with-gtk3 --without-qt --without-qt-immodule --enable-compat-scm --with-eb --with-eb-conf=$EB_CONF --with-libedit --with-qt4 --with-qt4-immodule --with-qt5 --with-qt5-immodule"
CONF_FULL="$CONF_MAINT $CONF_FULL_WO_MAINT"

git submodule update --init --recursive
# Create a branch if a branch correspoinding to the tag doesn't exist.
# Then check out the branch.
(cd sigscheme/libgcroots \
    && (git branch | grep -q "\<$LIBGCROOTS_TAG\>"  \
        || git branch $LIBGCROOTS_TAG $LIBGCROOTS_TAG) \
    && git checkout $LIBGCROOTS_TAG && ./autogen.sh) \
 && (cd sigscheme \
    && (git branch | grep -q "\<$SSCM_TAG\>" \
        || git branch $SSCM_TAG $SSCM_TAG) \
    && git checkout $SSCM_TAG && ./autogen.sh) \
 && ./autogen.sh \
|| { echo 'autogen failed.' && exit 1; }

for conf_args in "$CONF_NONE" "$CONF_DEFAULT"; do
    echo "configure $conf_args"
    ./configure $conf_args && $MAKE dist \
      || { echo 'make dist failed'; exit 1; }
done

# do make check without --enable-maintainer-mode
export DISTCHECK_CONFIGURE_FLAGS="$CONF_FULL_WO_MAINT"
for conf_args in "$CONF_FULL"; do
    echo "configure $conf_args"
    ./configure $conf_args && $MAKE distcheck sum \
      || { echo 'make distcheck failed'; exit 1; }
done
