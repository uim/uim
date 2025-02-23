#!/bin/bash

set -eu

echo "::group::autogen"
set -x
rm -rf ~/source
cp -a /source ~/source
pushd ~/source
./autogen.sh
pushd sigscheme
./autogen.sh
pushd libgcroots
./autogen.sh
popd
popd
popd
set +x
echo "::endgroup::"

echo "::group::configure"
set -x
~/source/configure \
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
  --with-qt5 \
  --with-qt5-immodule \
  --with-qt6 \
  --with-qt6-immodule \
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
