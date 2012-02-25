#!/bin/sh

# make-wc.sh: Check out working copy of uim trunk and prepare to build.  Do
#             not use this script to make a release. Use make-dist.sh for it.
#
# Usage:
#
#     $ git clone https://code.google.com/p/uim/
#     $ cd uim
#     $ ./make-wc.sh
#     $ make
#
#   You can add arbitrary configure options to make-wc.sh as follows:
#
#     $ ./make-wc.sh --enable-debug --enable-backtrace
#
# Prerequisites to build uim trunk:
#
#   - autoconf 2.60b or later (2.61 is recommended)
#   - automake 1.10 or later
#   - libtool (1.5.22 is working)
#   - intltool 0.35.2 (later versions may need autogen.sh modification)
#   - GNU make
#   - perl
#   - ruby
#   - rsvg(1) distributed with librsvg
#   - AsciiDoc
#
# And to run tests:
#
#   - GNU sed (for 'make check' on SigScheme)
#   - Gauche 0.8.5 or later
#   - GaUnit 0.1.1 or later (see doc/UNIT-TEST)

LANG=C

MAKE=make

RELEASE_SUFFIX="-snapshot-"`date +%Y%m%d`

# --enable-maintainer-mode is required to build git HEAD
CONF_COMMON="--enable-maintainer-mode --disable-warnings-into-error"


git submodule update --init --recursive 
(cd sigscheme/libgcroots && ./autogen.sh) \
 && (cd sigscheme && ./autogen.sh) \
 && ./autogen.sh \
|| { echo 'autogen failed.' && exit 1; }

if test -n "$RELEASE_SUFFIX"; then
    ed Makefile.in <<EOT
/^distdir =
d
i
RELEASE_SUFFIX = ${RELEASE_SUFFIX}
#distdir = \$(PACKAGE)-\$(VERSION)
distdir = \$(PACKAGE)-\$(VERSION)\$(RELEASE_SUFFIX)
.
wq
EOT
fi

echo "configure $CONF_COMMON $@"
./configure $CONF_COMMON $@
