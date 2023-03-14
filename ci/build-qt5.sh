#!/bin/bash

set -eux

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-qt5 \
  --with-qt5-immodule

make > /dev/null

sudo make install
