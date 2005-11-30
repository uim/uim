#!/bin/sh

# Notice: Autotools' version must be 1.7 or later

aclocal -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake --add-missing --foreign --copy \
  && autoconf
