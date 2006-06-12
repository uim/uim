dnl @synopsis AX_LIB_GLIBC
dnl
dnl Defines HAVE_GLIBC if libc is the GNU libc.
dnl
dnl This macro only performs the glibc detection, and not responsible for
dnl defining any extension specifiers such as _GNU_SOURCE. Do it by
dnl separate code as needed.
dnl 
dnl Although __GLIBC__ is provided by glibc, it is available only after
dnl including feature.h. But some extensions of glibc requires that
dnl corresponding macro is defined by user before feature.h is included,
dnl to be enabled. For example, _GNU_SOURCE is required to enable
dnl asprintf(3). So this macro provide HAVE_GLIBC to define such extension
dnl specifier macros only if the libc is glibc, to avoid that unneeded
dnl macros that may cause an incompatible effect are defined in non-glibc
dnl environment.
dnl
dnl @category C
dnl @author YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
dnl @version 2006-01-22
dnl @license AllPermissive

AC_DEFUN([AX_LIB_GLIBC],
  [AC_CACHE_CHECK([if libc is the GNU libc],
    ax_cv_lib_glibc,
    [AC_EGREP_CPP([^ax_cv_lib_glibc_yes$], [
/* To avoid being affected from possible header reorganization, this macro
 * does not include features.h directly. */
#include <stdlib.h>
#if (defined(__GLIBC__) || defined(__GNU_LIBRARY__))
ax_cv_lib_glibc_yes
#endif
      ],
      ax_cv_lib_glibc=yes,
      ax_cv_lib_glibc=no)
    ])
  if test "x$ax_cv_lib_glibc" = xyes; then
    AC_DEFINE(HAVE_GLIBC, 1, [Define to 1 if you have the GNU libc.])
  fi
])
