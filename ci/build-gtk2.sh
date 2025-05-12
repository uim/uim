#!/bin/bash

set -eu

echo "::group::configure"
set -x
/source/configure \
  --disable-gnome3-applet \
  --enable-gnome-applet \
  --enable-maintainer-mode \
  --prefix=/tmp/local \
  --with-gtk2 \
  --without-gtk3
set +x
echo "::endgroup::"

echo "::group::make"
set -x
make > /dev/null
set +x
echo "::endgroup::"

echo "::group::install"
set -x
sudo make install
set +x
echo "::endgroup::"
