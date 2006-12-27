#!/bin/sh

# Notice: Automake 1.8.3 or later is required.

aclocal -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake --add-missing --foreign --copy \
  && autoconf \
  && intltoolize --copy --force --automake \
  && perl -i -pe 's/\bscm\b/scm-workaround/g' intltool-update.in
