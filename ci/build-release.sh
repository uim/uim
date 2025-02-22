#!/bin/bash

set -eu

echo "::group::configure"
set -x
/source/configure \
  --enable-dict \
  --enable-gnome-applet \
  --enable-gnome3-applet \
  --enable-kde-applet \
  --enable-kde4-applet \
  --enable-maintainer-mode \
  --enable-openssl \
  --prefix=/tmp/local \
  --with-canna \
  --with-curl \
  --with-eb \
  --with-eb-conf=/etc/eb.conf \
  --with-ffi \
  --with-qt4 \
  --with-qt4-immodule \
  --with-qt4-qt3-support \
  --with-qt5 \
  --with-qt5-immodule \
  --with-gtk2 \
  --with-gtk3 \
  --with-sj3 \
  --with-wnn
set +x
echo "::endgroup::"

echo "::group::distcheck"
set -x
sudo -H make distcheck
set +x
echo "::endgroup::"

echo "::group::sum"
set -x
make sum
set +x
echo "::endgroup::"

echo "::group::dist"
set -x
sudo -H mv *.tar.* *.sum /source/
set +x
echo "::endgroup::"
