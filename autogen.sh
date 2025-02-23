#!/bin/sh

set -e

${AUTORECONF:-autoreconf} --force --install "$@"
# WORKAROUND: intltool-merge creates .intltool-merge-cache.lock but
# it's not listed in mostlyclean in po/Makefile.in.in. It causes a
# "make distcheck" failure.
sed -i.bak -e 's/ \.intltool-merge-cache$/ .intltool-merge-cache{,.lock}/g' \
  po/Makefile.in.in
rm -f po/Makefile.in.in.bak
