#!/bin/bash

set -eux

/source/configure \
  --prefix=/tmp/local

make # > /dev/null

sudo make install
