#!/bin/bash

set -eux

/source/configure \
  --prefix=/tmp/local \
  --with-qt5 \
  --with-qt5-immodule

make > /dev/null

sudo make install
