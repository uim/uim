dnl @synopsis AX_C_REFERENCEABLE_PASSED_VA_LIST
dnl
dnl Checks whether f(va_list va){ &va; } works as expected.
dnl
dnl C99 mentioned passing a pointer to va_list to other functions (footnote
dnl 212 of "7.15 Variable arguments <stdarg.h>"). However, f(va_list va) {
dnl &va; } produces broken pointer on some environments such as gcc on
dnl x86_64, although { va_list va; &va; } works as expected. See the
dnl detection code of this file and the links below for further information.
dnl
dnl   http://www.gnu.org/software/autoconf/manual/html_node/Function-Portability.html
dnl   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=14557
dnl   http://gcc.gnu.org/bugzilla/show_bug.cgi?id=20951
dnl
dnl Although C99 does not define the operations f(va_list va) { &va; } and
dnl &va itself as standard (footnotes are declared as "normative part,
dnl information only"), certain situations need it. This macro provides a
dnl type detection about va_list implementation to deal with the operation.
dnl
dnl Following workaround will probably work on such environments although it
dnl does not ensure to be safe and portable. At least it is working on
dnl x86_64-unknown-linux-gnu.
dnl 
dnl f(va_list va)
dnl {
dnl   va_list *vap;
dnl 
dnl #if HAVE_REFERENCEABLE_PASSED_VA_LIST
dnl     vap = &va;
dnl #else
dnl     vap = (va_list *)va;
dnl #endif
dnl }
dnl
dnl @category C
dnl @author YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl @version 2006-12-12
dnl @license AllPermissive

AC_DEFUN([AX_C_REFERENCEABLE_PASSED_VA_LIST], [
  AC_CACHE_CHECK([whether f(va_list va){ &va; } works as expected],
    [ax_cv_c_referenceable_passed_va_list],
    [AC_LINK_IFELSE([
#include <stdarg.h>

volatile va_list g_va;

void
vf(va_list callee_va)
{
  /* typeof(callee_va) differs from typeof(caller_va) by a compiler trick, if
   * va_list is implemented as an array. On such environment, this copy
   * operation fails. */
  g_va = callee_va;
}

void
f(int last, ...)
{
  va_list caller_va;

  va_start(caller_va, last);
  vf(caller_va);  /* passed as &caller_va[0] if va_list is an array type */
  va_end(caller_va);
}

int
main(int argc, char *argv[])
{
  f(0xdeadbeef, 0xfedbeef, 0xfeedee);

  return 0;
}
      ],
      [ax_cv_c_referenceable_passed_va_list=yes],
      [ax_cv_c_referenceable_passed_va_list=no])])
  if test "x$ax_cv_c_referenceable_passed_va_list" = xyes; then
    AC_DEFINE([HAVE_REFERENCEABLE_PASSED_VA_LIST], [1],
              [Define to 1 if f(va_list va){ &va; } works as expected.])
  fi
])
