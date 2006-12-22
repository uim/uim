#!/bin/sh

ADDR="uim@freedesktop.org"
TMPDIR=/tmp
TMPFILE=$TMPDIR/sigscheme-report.$$
PERL=perl
MAKE=
OPTS="--enable-euccn --enable-eucjp --enable-euckr --enable-sjis"
#OPTS="$OPTS --enable-maintainer-mode"

for cmd in gmake gnumake make; do
  if $cmd --version 2>/dev/null | grep -q GNU; then
    MAKE=$cmd
    break
  fi
done

if test "x$MAKE" != x && $PERL -v >/dev/null; then
  (./configure $OPTS && $MAKE clean >/dev/null && $MAKE >/dev/null \
   && $MAKE check && echo 'all tests passed') 2>&1 | tee $TMPFILE
  if grep -q 'all tests passed' $TMPFILE; then
    result='PASS'
  else
    result='FAIL'
  fi
  pkg=`perl -ne 'print $1 if /^Package:\s+(.+)$/' $TMPFILE`
  host=`perl -ne 'print $1 if /^host \(compile for\):\s+(.+)$/' $TMPFILE`

  cat >report.mail <<EOT
To: ${ADDR}
Subject: ${result}: ${pkg} ${host}

EOT
  cat $TMPFILE >>report.mail
  rm -f $TMPFILE
  echo
  echo "File 'report.mail' has been created. Please send it to us."
  exit 0
else
  echo "GNU make or Perl not found. Install them."
  exit 1
fi
