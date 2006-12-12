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

int c[__alignof__(char)];
int s[__alignof__(short)];
int i[__alignof__(int)];
int l[__alignof__(long)];
int vp[__alignof__(void *)];
int st[__alignof__(struct _st)];

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
