#!/bin/bash

set -eux

/source/configure \
  --enable-gnome2-applet \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-gtk3

make -j$(nproc) > /dev/null

sudo make install
