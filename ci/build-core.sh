#!/bin/bash

set -eux

/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/local

make # > /dev/null

sudo make install
