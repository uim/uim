FROM debian:13

RUN \
  echo "debconf debconf/frontend select Noninteractive" | \
    debconf-set-selections

RUN \
  echo 'APT::Install-Recommends "false";' > \
    /etc/apt/apt.conf.d/disable-install-recommends

RUN \
  apt update -qq && \
  apt install -y \
    gcc \
    intltool \
    libedit-dev \
    libncurses-dev \
    librsvg2-bin \
    make \
    pkg-config \
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

CMD /source/ci/build-core.sh
