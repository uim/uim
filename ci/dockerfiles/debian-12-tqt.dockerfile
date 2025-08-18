FROM debian:12

RUN \
  echo "debconf debconf/frontend select Noninteractive" | \
    debconf-set-selections

RUN \
  echo 'APT::Install-Recommends "false";' > \
    /etc/apt/apt.conf.d/disable-install-recommends

RUN \
  echo 'deb http://mirror.ppa.trinitydesktop.org/trinity/deb/trinity-r14.1.x bookworm main deps' >> /etc/apt/sources.list

ADD http://mirror.ppa.trinitydesktop.org/trinity/deb/trinity-keyring.deb /usr/local/

RUN dpkg -i /usr/local/trinity-keyring.deb

RUN \
  apt update -qq && \
  apt install -y \
    cmake \
    g++ \
    gcc \
    intltool \
    libedit-dev \
    libtqt3-mt-dev \
    libncurses-dev \
    libx11-dev \
    libxft-dev \
    librsvg2-bin \
    make \
    pkg-config \
    ruby \
    sudo \
    tdelibs14-trinity-dev \
    tqt3-dev-tools \
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

CMD /source/ci/build-tqt.sh
