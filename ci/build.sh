#!/bin/bash

set -eux

/source/configure \
  --prefix=/tmp/local \

make -j$(nproc) > /dev/null

sudo make install
