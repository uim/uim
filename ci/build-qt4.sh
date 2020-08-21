#!/bin/bash

set -eux

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-qt4 \
  --with-qt4-immodule \
  --with-qt4-qt3-support

make -j$(nproc) > /dev/null

sudo make install
