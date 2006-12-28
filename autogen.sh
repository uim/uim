#!/bin/sh

# Notice: Automake 1.8.3 or later is required.

aclocal -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake --add-missing --foreign --copy \
  && autoconf \
  && intltoolize --copy --force --automake \
  && perl -i -pe 's/\bscm\b/scm-workaround/g' intltool-update.in \
  && perl -i -pe "s/^INSTALL\b/# Makevars gets inserted here. (Don't remove this line!)\n\nINSTALL/" po/Makefile.in.in \
  && cp po/Makefile.in.in qt/chardict/po

# Since intltool 0.32 and later does not include po/Makevars into Makefile by
# default as we expected, I do the '# Makevars' workaround here. See
# config.status for further details.  -- YamaKen 2006-12-28
