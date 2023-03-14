#!/bin/bash

set -eux

/source/configure \
  --enable-gnome3-applet \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-gtk3

make > /dev/null

sudo make install
