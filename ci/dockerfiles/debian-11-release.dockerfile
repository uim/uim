FROM debian:11

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
    cmake \
    extra-cmake-modules \
    g++ \
    gauche-dev \
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
    libgtk2.0-bin \
    libgtk2.0-dev \
    libkf5plasma-dev \
    libm17n-dev \
    libncurses-dev \
    libnotify-dev \
    libpanel-applet-dev \
    libqt4-dev \
    libqt5x11extras5-dev \
    librsvg2-bin \
    libsqlite3-dev \
    libssl-dev \
    libwnn-dev \
    libx11-dev \
    make \
    pkg-config \
    qt5-qmake \
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

ENV QT_SELECT=qt5

CMD /source/ci/build-release.sh
