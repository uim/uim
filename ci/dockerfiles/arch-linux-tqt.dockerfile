FROM archlinux

RUN \
  echo '[trinity]' >> /etc/pacman.conf && \
  echo 'Server = https://mirror.ppa.trinitydesktop.org/trinity/archlinux/$arch' >> /etc/pacman.conf && \
  pacman-key --init && \
  pacman-key --recv-key  D6D6FAA25E9A3E4ECD9FBDBEC93AF1698685AD8B && \
  pacman-key --lsign-key D6D6FAA25E9A3E4ECD9FBDBEC93AF1698685AD8B

RUN \
  pacman --sync --noconfirm --refresh --sysupgrade && \
  pacman --sync --noconfirm \
    gcc \
    intltool \
    libedit \
    librsvg \
    make \
    ncurses \
    perl \
    pkgconf \
    ruby \
    sudo \
    tde-tdelibs \
    tde-tqtinterface \
    tzdata

RUN \
  useradd --user-group --create-home uim

RUN \
  echo "uim ALL=(ALL:ALL) NOPASSWD:ALL" | \
    EDITOR=tee visudo -f /etc/sudoers.d/uim

USER uim

RUN mkdir -p /home/uim/build
WORKDIR /home/uim/build

CMD /source/ci/build-tqt.sh
