#!/bin/bash

set -eux

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local

make -j$(nproc) # > /dev/null

sudo make install
