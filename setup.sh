#!/bin/bash

set -eux

if type sudo > /dev/null 2>&1; then
  SUDO=sudo
else
  SUDO=
fi

function setup_with_apt () {
  ${SUDO} apt update
  ${SUDO} apt install -y -V \
    asciidoc \
    autoconf \
    autoconf-archive \
    autopoint \
    cmake \
    extra-cmake-modules \
    g++ \
    gcc \
    intltool \
    libanthy-dev \
    libcanna1g-dev \
    libcurl4-gnutls-dev \
    libeb16-dev \
    libedit-dev \
    libexpat1-dev \
    libffi-dev \
    libgtk-3-bin \
    libgtk-3-dev \
    libgtk-4-bin \
    libgtk-4-dev \
    libkf5plasma-dev \
    libm17n-dev \
    libncurses-dev \
    libnotify-dev \
    libqt5x11extras5-dev \
    librsvg2-bin \
    libsqlite3-dev \
    libssl-dev \
    libtool \
    libwnn-dev \
    libx11-dev \
    make \
    pkg-config \
    qt5-qmake \
    qt6-base-private-dev \
    qt6-declarative-dev \
    qtbase5-dev \
    qtbase5-private-dev \
    qtdeclarative5-dev \
    ruby \
    sudo \
    tzdata
}

if [ -f /etc/debian_version ]; then
  setup_with_apt
# elif type dnf > /dev/null 2>&1; then
#   setup_with_dnf
else
  echo "This OS setup is not supported."
  exit 1
fi
