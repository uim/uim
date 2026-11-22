#!/bin/bash

set -eu

mkdir -p autotools
cd autotools
echo "::group::configure"
set -x
/source/configure \
  --enable-maintainer-mode \
  --prefix=/tmp/autotools \
  --with-qt6 \
  --with-qt6-immodule
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

cd -
/source/ci/build-meson.sh \
	-Dqt6=enabled
