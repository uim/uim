aclocal-1.7 -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake-1.7 --add-missing --foreign --copy \
  && autoconf \
  && ./configure $@
