#!/bin/sh

# make-wc.sh: Check out working copy of uim trunk and prepare to build.  Do
#             not use this script to make a release. Use make-dist.sh for it.
#
# Usage:
#
#     $ svn co http://anonsvn.freedesktop.org/svn/uim/trunk uim-trunk
#     $ cd uim-trunk
#     $ ./make-wc.sh
#     $ make
#
#   Or if you are an uim committer,
#
#     $ svn co svn+ssh://svn.freedesktop.org/svn/uim/trunk uim-trunk
#     $ cd uim-trunk
#     $ ./make-wc.sh
#     $ make
#
#   You can add arbitrary configure options to make-wc.sh as follows:
#
#     $ ./make-wc.sh --enable-debug --enable-backtrace
#
# Prerequisites to build uim trunk:
#
#   - autoconf 2.60b or later
#   - automake 1.10 or later
#   - libtool (1.5.22 is working)
#   - intltool 0.35.2 (later versions may need autogen.sh modification)
#   - GNU make
#   - GNU sed
#   - perl
#   - ruby
#   - rsvg(1) distributed with librsvg
#   - AsciiDoc

MAKE=make

#UIM_REPOSITORY="http://anonsvn.freedesktop.org/svn/uim"
#UIM_REPOSITORY="svn+ssh://svn.freedesktop.org/svn/uim"
# autoselect
UIM_REPOSITORY=`svn info | grep 'Repository Root:' | sed 's/.*: //'`

SSCM_REPOSITORY="${UIM_REPOSITORY}/sigscheme-trunk"
LIBGCROOTS_REPOSITORY="${UIM_REPOSITORY}/libgcroots-trunk"
TAGS_REPOSITORY="${UIM_REPOSITORY}/tags"
SSCM_URL="${SSCM_REPOSITORY}"
#SSCM_URL="${TAGS_REPOSITORY}/sigscheme-0.7.3"
LIBGCROOTS_URL="${LIBGCROOTS_REPOSITORY}"
#LIBGCROOTS_URL="${TAGS_REPOSITORY}/libgcroots-0.1.4"
RELEASE_SUFFIX="-snapshot-"`date +%Y%m%d`

# --enable-maintainer-mode is required to build svn trunk
CONF_COMMON="--enable-maintainer-mode --disable-warnings-into-error"


svn co $SSCM_URL sigscheme
svn co $LIBGCROOTS_URL sigscheme/libgcroots
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
