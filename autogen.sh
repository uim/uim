#!/bin/sh

aclocal-1.11 -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake-1.11 --add-missing --foreign --copy \
  && autoconf \
  && intltoolize --copy --force --automake \
  && perl -i -pe "s/^DISTFILES\b/# Makevars gets inserted here. (Don't remove this line!)\n\nDISTFILES/" po/Makefile.in.in \
  && perl -i -pe 's/\bscm\b/scm-workaround/g' intltool-update.in \
  && perl -i -pe 's%"(POTFILES.(skip|ignore))"%"\$SRCDIR/$1"%g' intltool-update.in \
  && perl -i -pe 's%(-f "\$SRCDIR/../\$dummy")%$1 and ! -f "../\$dummy"%' intltool-update.in

# Since intltool 0.32 and later does not include po/Makevars into Makefile by
# default as we expected, I do the '# Makevars' workaround here. See
# config.status for further details.  -- YamaKen 2006-12-28

# To cancel the unwanted special treatment for *.scm by intltool-update, change
# the suffix here.  -- YamaKen 2006-12-29

# To fix the case intltool-update -m on (srcdir != builddir), the
# $SRCDIR-related lines are patched here.  -- YamaKen 2006-12-29
