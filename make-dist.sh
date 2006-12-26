#!/bin/sh

MAKE=make

UIM_REPOSITORY="http://anonsvn.freedesktop.org/svn/uim"
SSCM_REPOSITORY="${UIM_REPOSITORY}/sigscheme-trunk"
TAGS_REPOSITORY="${UIM_REPOSITORY}/tags"
#SSCM_URL="${TAGS_REPOSITORY}/sigscheme-0.7.2"
SSCM_URL="${UIM_REPOSITORY}/sigscheme-trunk"
LIBGCROOTS_URL="${TAGS_REPOSITORY}/libgcroots-0.1.2"

svn export $SSCM_URL sigscheme
svn export $LIBGCROOTS_URL sigscheme/libgcroots
(cd sigscheme/libgcroots && ./autogen.sh) \
 && (cd sigscheme && ./autogen.sh) \
 && ./autogen.sh
./configure --enable-maintainer-mode --enable-conf=full
$MAKE distcheck sum
