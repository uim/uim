#!/bin/sh

MAKE=make

CONF_MAINT="--enable-maintainer-mode"
CONF_NOWERROR="--disable-warnings-into-error"
CONF_COMMON="$CONF_MAINT $CONF_NOWERROR"
CONF_NONE="$CONF_COMMON --disable-debug --disable-fep --disable-emacs --disable-gnome-applet --disable-kde-applet --disable-pref --disable-dict --without-anthy --without-canna --without-mana --without-prime --without-skk --without-x --without-xft --without-m17nlib --without-gtk2 --without-qt --without-qt-immodule --disable-compat-scm --without-eb --without-libedit"
CONF_DEFAULT="$CONF_COMMON"

if type apt > /dev/null 2>&1; then
    sudo apt install -V \
	 libeb16-dev \
	 libqt4-dev \
	 qt5-qmake \
	 kdelibs-bin \
	 kdelibs5-dev
fi

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

CONF_FULL_BASE_NO_MAINT="
$CONF_NOWERROR
--enable-debug
--enable-fep
--enable-emacs
--enable-gnome-applet
--enable-gnome3-applet
--enable-pref
--enable-dict
--with-anthy
--with-canna
--with-wnn
--with-sj3
--with-mana
--with-prime
--with-m17nlib
--with-gtk2
--with-gtk3
--without-qt
--without-qt-immodule
--enable-compat-scm
--with-eb
--with-eb-conf=$EB_CONF
--with-libedit
"
CONF_FULL_QT4_WO_MAINT="
QT_SELECT=4
$CONF_FULL_BASE_NO_MAINT
--enable-kde4-applet
--enable-notify=libnotify,knotify4
--with-qt4
--with-qt4-immodule
--without-qt5
--without-qt5-immodule
"
CONF_FULL_QT5_WO_MAINT="
QT_SELECT=5
$CONF_FULL_BASE_NO_MAINT
--enable-kde-applet
--enable-notify=libnotify
--with-qt5
--with-qt5-immodule
"

git submodule update --init --recursive
# Create a branch if a branch correspoinding to the tag doesn't exist.
# Then check out the branch.
(cd sigscheme/libgcroots && ./autogen.sh) \
 && (cd sigscheme && ./autogen.sh) \
 && ./autogen.sh \
|| { echo 'autogen failed.' && exit 1; }

# for conf_args in "$CONF_NONE" "$CONF_DEFAULT"; do
#     echo "configure $conf_args"
#     ./configure $conf_args && $MAKE dist \
#       || { echo 'make dist failed'; exit 1; }
# done

for conf_args in "$CONF_FULL_QT4_NO_MAINT" "$CONF_FULL_QT5_NO_MAINT"; do
    echo "configure $conf_args"
    export DISTCHECK_CONFIGURE_FLAGS="$conf_args"
    ./configure $CONF_MAINT $conf_args && $MAKE distcheck sum \
      || { echo 'make distcheck failed'; exit 1; }
done
