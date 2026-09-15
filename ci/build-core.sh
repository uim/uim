#!/bin/bash

set -eu

echo "::group::configure"
set -x
/source/configure \
  --enable-maintainer-mode \
  --enable-openssl \
  --prefix=/tmp/local
set +x
echo "::endgroup::"

echo "::group::make"
set -x
make # > /dev/null
set +x
echo "::endgroup::"

echo "::group::po"
set -x
# Regenerate uim.pot from the sources.  This fails when a gettext
# update stops accepting our sources, as happened with gettext 0.23
# (#221).
make -C po uim.pot
set +x
echo "::endgroup::"

echo "::group::install"
set -x
sudo make install
set +x
echo "::endgroup::"
