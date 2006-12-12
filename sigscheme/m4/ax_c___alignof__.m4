dnl Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl
dnl Copying and distribution of this file, with or without modification,
dnl are permitted in any medium without royalty provided the copyright
dnl notice and this notice are preserved.
dnl
dnl
dnl @synopsis AX_C___ALIGNOF__
dnl
dnl Checks if __alignof__ operator is available.
dnl
dnl @category C
dnl @author YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl @version 2006-12-12
dnl @license AllPermissive

AC_DEFUN([AX_C___ALIGNOF__], [
  AC_CACHE_CHECK([for __alignof__],
    [ax_cv_c___alignof__],
    [AC_COMPILE_IFELSE([[
struct _st {
  int i;
  char c;
  void *p;
};

int  c[__alignof__(char)];
int  s[__alignof__(short)];
int  i[__alignof__(int)];
int  l[__alignof__(long)];
int vp[__alignof__(void *)];
int st[__alignof__(struct _st)];

/* test whether ((__alignof__(type)) != 0) */
int  c2[((__alignof__(char))       > 0) ? 1 : -1];
int  s2[((__alignof__(short))      > 0) ? 1 : -1];
int  i2[((__alignof__(int))        > 0) ? 1 : -1];
int  l2[((__alignof__(long))       > 0) ? 1 : -1];
int vp2[((__alignof__(void *))     > 0) ? 1 : -1];
int st2[((__alignof__(struct _st)) > 0) ? 1 : -1];

int
f(void)
{
  short sv;

  return (__alignof__(sv) >= __alignof__(char));
}
      ]],
      [ax_cv_c___alignof__=yes],
      [ax_cv_c___alignof__=no])])
  if test "x$ax_cv_c___alignof__" = xyes; then
    AC_DEFINE([HAVE___ALIGNOF__], [1],
              [Define to 1 if __alignof__ operator is available.])
  fi
])
