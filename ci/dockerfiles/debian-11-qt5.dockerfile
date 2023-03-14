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
    cmake \
    extra-cmake-modules \
    g++ \
    gcc \
    intltool \
    libedit-dev \
    libkf5plasma-dev \
    libncurses-dev \
    libqt5x11extras5-dev \
    librsvg2-bin \
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

CMD /source/ci/build-qt5.sh
