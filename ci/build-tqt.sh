#!/bin/bash

set -eux

export TDEDIR=/opt/trinity

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-tqt \
  --with-tqt-immodule \
  --enable-tde-applet

make > /dev/null

sudo make install
