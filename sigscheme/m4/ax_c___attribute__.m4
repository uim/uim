dnl @synopsis AX_C___ATTRIBUTE__
dnl
dnl Provides a test for the compiler support of __attribute__
dnl extensions. defines HAVE___ATTRIBUTE__ if it is found.
dnl
dnl Originating from the 'pork' package by Ryan McCabe <ryan@numb.org>
dnl
dnl @category C
dnl @author Christian Haggstrom <chm@c00.info>
dnl @version 2005-01-21
dnl @license GPLWithACException

AC_DEFUN([AX_C___ATTRIBUTE__], [
  AC_MSG_CHECKING(for __attribute__)
  AC_CACHE_VAL(ac_cv___attribute__, [
    AC_TRY_COMPILE(
      [#include <stdlib.h>],
      [static void foo(void) __attribute__ ((unused));
      static void
      foo(void) {
          exit(1);
      }],
      ac_cv___attribute__=yes,
      ac_cv___attribute__=no
    )])
  if test "$ac_cv___attribute__" = "yes"; then
    AC_DEFINE(HAVE___ATTRIBUTE__, 1, [define if your compiler has __attribute__])
  fi
  AC_MSG_RESULT($ac_cv___attribute__)
])
