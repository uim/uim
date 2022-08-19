#!/bin/bash

# Simplified make-dist.sh

set -exu

# "make distcheck" removes write permission
chmod -R u+w ../uim.release || :
rm -rf ../uim.release
mkdir -p ../uim.release
cd ../uim.release
../uim/configure \
  --enable-maintainer-mode \
  --disable-warnings-into-error
make distcheck sum
