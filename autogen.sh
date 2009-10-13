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

fix_aclocal_m4() {
  # need workaround if intltool >= 0.40.4
  if test $INTLTOOL_VERSION_MAJOR -eq 0 \
       -a $INTLTOOL_VERSION_MINOR -eq 40 \
       -a $INTLTOOL_VERSION_MICRO -gt 3 \
     -o \
          $INTLTOOL_VERSION_MAJOR -eq 0 \
       -a $INTLTOOL_VERSION_MINOR -gt 40; then
    echo "modify aclocal.m4"
    ed aclocal.m4 << EOT
/^    \[sed '\/\^POTFILES
i
    mv "\$1/Makefile" "\$1/Makefile.tmp"
.
/\$1\/Makefile.in
s/Makefile.in/Makefile.tmp/
wq
EOT
  fi
}

# check intltool version to examine whether fixes for aclocal.m4 is needed
INTLTOOL_VERSION=`intltoolize --version | sed -e "s/.* \([0-9]*\.[0-9]*\.[0-9]*\)/\1/"`
INTLTOOL_VERSION_MAJOR=`echo $INTLTOOL_VERSION | cut -d '.' -f 1`
INTLTOOL_VERSION_MINOR=`echo $INTLTOOL_VERSION | cut -d '.' -f 2`
INTLTOOL_VERSION_MICRO=`echo $INTLTOOL_VERSION | cut -d '.' -f 3`

aclocal -I m4 \
  && fix_aclocal_m4 \
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
