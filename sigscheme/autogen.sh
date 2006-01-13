#!/bin/sh

# Notice: Automake 1.8.3 or later is required.

aclocal \
  && libtoolize --force --copy \
  && autoheader \
  && automake --add-missing --foreign --copy \
  && autoconf
