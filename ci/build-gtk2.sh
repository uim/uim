#!/bin/bash

set -eux

/source/configure \
  --disable-gnome3-applet \
  --enable-gnome-applet \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-gtk2 \
  --without-gtk3

make > /dev/null

sudo make install
