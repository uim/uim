#!/bin/bash

set -eux

/source/configure \
  --enable-dict \
  --enable-gnome-applet \
  --enable-gnome3-applet \
  --enable-kde-applet \
  --enable-kde4-applet \
  --enable-maintainer-mode \
  --enable-openssl \
  --prefix=/tmp/local \
  --with-canna \
  --with-curl \
  --with-eb \
  --with-eb-conf=/etc/eb.conf \
  --with-ffi \
  --with-qt4 \
  --with-qt4-immodule \
  --with-qt4-qt3-support \
  --with-qt5 \
  --with-qt5-immodule \
  --with-gtk2 \
  --with-gtk3 \
  --with-sj3 \
  --with-wnn

sudo -H make distcheck
make sum

sudo -H mv *.tar.* *.sum /source/
