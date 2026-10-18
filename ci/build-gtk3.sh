#!/bin/bash

set -eu

# Builds the GTK 3 frontends and uim-xim with both build systems.

echo "::group::Autotools: configure"
set -x
mkdir -p autotools
cd autotools
/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/autotools \
  --with-gtk3
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
  -Dgtk3=enabled \
  -Dpref=enabled \
  -Dxim=enabled
