#!/bin/bash

set -eu

# Builds the Qt 6 immodule and helpers with Autotools, and the immodule
# and its candidate window with Meson.

echo "::group::Autotools: configure"
set -x
mkdir -p autotools
cd autotools
/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/autotools \
  --with-qt6 \
  --with-qt6-immodule
set +x
echo "::endgroup::"

echo "::group::Autotools: make"
set -x
make > /dev/null
set +x
echo "::endgroup::"

echo "::group::Autotools: install"
set -x
sudo make install
set +x
echo "::endgroup::"
cd -

/source/ci/build-meson.sh \
  -Dqt6=enabled
