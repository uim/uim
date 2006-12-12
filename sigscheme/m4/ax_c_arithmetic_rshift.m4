dnl Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.
dnl
dnl
dnl @synopsis AX_C_ARITHMETIC_RSHIFT
dnl
dnl Checks if the right shift operation is arithmetic.
dnl
dnl This macro uses compile-time detection and so cross-compile ready.
dnl
dnl @category C
dnl @author YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl @version 2006-12-12
dnl @license AllPermissive

AC_DEFUN([AX_C_ARITHMETIC_RSHIFT], [
  AC_CACHE_CHECK([whether right shift operation is arithmetic],
                 [ax_cv_c_arithmetic_rshift],
                 [AC_COMPILE_IFELSE([[int dummy[((-1 >> 1) < 0) ? 1 : -1];]],
                                    [ax_cv_c_arithmetic_rshift=yes],
                                    [ax_cv_c_arithmetic_rshift=no])])
  if test "x$ax_cv_c_arithmetic_rshift" = xyes; then
    AC_DEFINE([HAVE_ARITHMETIC_RSHIFT], [1],
              [Define to 1 if the right shift operation is arithmetic.])
  fi
])
