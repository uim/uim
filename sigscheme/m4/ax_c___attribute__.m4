dnl Copyright (c) 1995, 1996, 1997, 1998
dnl The Regents of the University of California.  All rights reserved.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that: (1) source code distributions
dnl retain the above copyright notice and this paragraph in its entirety, (2)
dnl distributions including binary code include the above copyright notice and
dnl this paragraph in its entirety in the documentation or other materials
dnl provided with the distribution, and (3) all advertising materials mentioning
dnl features or use of this software display the following acknowledgement:
dnl ``This product includes software developed by the University of California,
dnl Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
dnl the University nor the names of its contributors may be used to endorse
dnl or promote products derived from this software without specific prior
dnl written permission.
dnl THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
dnl WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
dnl MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
dnl
dnl LBL autoconf macros
dnl
dnl Taken from libpcap -- Jun Inoue (May 7, 2006)
dnl


dnl
dnl Test for __attribute__
dnl

AC_DEFUN([AX_C___ATTRIBUTE__], [
AC_MSG_CHECKING(for __attribute__)
AC_CACHE_VAL(ac_cv___attribute__, [
AC_COMPILE_IFELSE(
  AC_LANG_SOURCE([[
#include <stdlib.h>

static void foo(void) __attribute__ ((noreturn));

static void
foo(void)
{
  exit(1);
}

int
main(int argc, char **argv)
{
  foo();
}
  ]]),
ac_cv___attribute__=yes,
ac_cv___attribute__=no)])
if test "$ac_cv___attribute__" = "yes"; then
  AC_DEFINE(HAVE___ATTRIBUTE__, 1, [define if your compiler has __attribute__])
  V_DEFS="$V_DEFS -D_U_=\"__attribute__((unused))\""
else
  V_DEFS="$V_DEFS -D_U_=\"\""
fi
AC_MSG_RESULT($ac_cv___attribute__)
])
