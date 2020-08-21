#!/bin/bash

set -eux

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-qt5 \
  --with-qt5-immodule

make -j$(nproc) > /dev/null

sudo make install
