##### 
#
# SYNOPSIS
#
#   AX_FUNC_SIGSETJMP
#
# DESCRIPTION
#
#   Check whether sigsetjmp(3) and siglongjmp(3) is available.
#
#   Since sigsetjmp(3) and siglongjmp(3) may be a macro,
#   AC_CHECK_FUNC() is not appropriate to detect them. This macro can
#   properly detect them although having '_FUNC_' prefix for the naming
#   convention. This macro uses compile-time detection and so is
#   cross-compile ready.
#
# LAST MODIFICATION
#
#   2007-08-11
#
# COPYLEFT
#
#   Copyright (c) 2007 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

AC_DEFUN([AX_FUNC_SIGSETJMP], [
  AC_CACHE_CHECK([for sigsetjmp],
                 [ax_cv_func_sigsetjmp],
                 [AC_LINK_IFELSE(
                    AC_LANG_PROGRAM([[@%:@include <setjmp.h>]],
                                    [[sigjmp_buf env;
				      while (!sigsetjmp(env, 1))
				        siglongjmp(env, 1);
				      return 0;]]),
                    [ax_cv_func_sigsetjmp=yes],
                    [ax_cv_func_sigsetjmp=no])])
  if test "x$ax_cv_func_sigsetjmp" = xyes; then
    AC_DEFINE([HAVE_SIGSETJMP], [1],
              [Define to 1 if you have the `sigsetjmp' (and 'siglongjmp') function or macro.])
  fi
])
