#!/bin/sh

generate_makefile_in_in() {
  cp po/Makefile.in.in $1 \
    && perl -i -pe 's/check: all.*$/check: all/' $1/Makefile.in.in \
    && ed $1/Makefile.in.in <<EOT
/^check:
+
.,/^$/d
i
# The test is removed for $1  -- YamaKen

.
wq
EOT
}

aclocal -I m4 \
  && libtoolize --force --copy \
  && autoheader \
  && automake --add-missing --foreign --copy \
  && autoconf \
  && intltoolize --copy --force --automake \
  && perl -i -pe "s/^DISTFILES\b/# Makevars gets inserted here. (Don't remove this line!)\n\nDISTFILES/" po/Makefile.in.in \
  && perl -i -pe 's/\bscm\b/scm-workaround/g' intltool-update.in \
  && perl -i -pe 's%"(POTFILES.(skip|ignore))"%"\$SRCDIR/$1"%g' intltool-update.in \
  && perl -i -pe 's%(-f "\$SRCDIR/../\$dummy")%$1 and ! -f "../\$dummy"%' intltool-update.in \
  && generate_makefile_in_in qt/chardict/po \
  && generate_makefile_in_in qt4/chardict/po

# Since intltool 0.32 and later does not include po/Makevars into Makefile by
# default as we expected, I do the '# Makevars' workaround here. See
# config.status for further details.  -- YamaKen 2006-12-28

# To cancel the unwanted special treatment for *.scm by intltool-update, change
# the suffix here.  -- YamaKen 2006-12-29

# To fix the case intltool-update -m on (srcdir != builddir), the
# $SRCDIR-related lines are patched here.  -- YamaKen 2006-12-29

# qt/chardict/po has separated gettext package with a dirty workaround.
#   -- YamaKen 2006-12-29
