FROM debian:12

RUN \
  echo "debconf debconf/frontend select Noninteractive" | \
    debconf-set-selections

RUN \
  echo 'APT::Install-Recommends "false";' > \
    /etc/apt/apt.conf.d/disable-install-recommends

RUN \
  apt update -qq && \
  apt install -y \
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
    tzdata && \
  apt clean && \
  rm -rf /var/lib/apt/lists/*

RUN \
  useradd --user-group --create-home uim

RUN \
  echo "uim ALL=(ALL:ALL) NOPASSWD:ALL" | \
    EDITOR=tee visudo -f /etc/sudoers.d/uim

USER uim

RUN mkdir -p /home/uim/build
WORKDIR /home/uim/build

CMD /source/ci/build-release.sh
