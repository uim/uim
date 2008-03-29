# wnn.m4
# Configure paths for libwnn

dnl AM_PATH_WNN([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Test for WNN, and define WNNLIBDIR, LIBWNN_CPPFLAGS and LIBWNN_LIBS
dnl
AC_DEFUN([AM_PATH_WNN],
[dnl
dnl Get the cflags and libraries
dnl
  AC_ARG_WITH(wnn-libraries,[  --with-wnn-libraries=DIR Directory where wnn library is installed (optional)], wnn_libraries="$withval", wnn_libraries="")
  AC_ARG_WITH(wnn-includes,[  --with-wnn-includes=DIR  Directory where wnn header files are installed (optional)], wnn_includes="$withval", wnn_includes="")

  if test "x$wnn_libraries" != "x" ; then
      WNN_LIBS="-L$wnn_libraries"
      wnnlibdir="$wnn_libraries"
  elif test "x$prefix" != "xNONE"; then
      WNN_LIBS="-L$prefix/lib"
      wnnlibdir="$prefix/lib"
  fi

  if test "x$wnn_includes" != "x" ; then
      WNN_CPPFLAGS="-I$wnn_includes"
  elif test "x$prefix" != "xNONE"; then
      WNN_CPPFLAGS="-I$prefix/include"
  fi

  ac_save_CPPFLAGS="$CPPFLAGS"
  ac_save_LIBS="$LIBS"

  CPPFLAGS="$CPPFLAGS $WNN_CPPFLAGS"
  LIBS="$LIBS $WNN_LIBS"

  AC_CHECK_HEADERS([jllib.h],
   [AC_CHECK_LIB(wnn, jl_open_lang, use_wnn=yes, use_wnn=no)
   ], [use_wnn="no"], [])
  if test x"$use_wnn" = "xno"; then
     AC_CHECKING(libwnn requires libcrypt)
     AC_CHECK_HEADERS([jllib.h],
     [AC_CHECK_LIB(wnn, jl_connect_lang, [use_wnn=yes WNN_LIBADD="-lcrypt"], use_wnn=no, -lcrypt)
     ], [use_wnn="no"], [])
  fi

  if test x"$use_wnn" = "xyes"; then
    ifelse([$1], , :, [$1])
  else
    ifelse([$2], , :, [$2])
  fi

  CPPFLAGS="$ac_save_CPPFLAGS"
  LIBS="$ac_save_LIBS"

  if test "x$wnnlibdir" != "x" ; then
    AC_DEFINE_UNQUOTED(WNNLIBDIR, "$wnnlibdir", [Wnn library directory])
  else
    AC_MSG_WARN([****** Cannot found path of Wnn library. Please set this option with --with-wnn-libraries])
    AC_DEFINE_UNQUOTED(WNNLIBDIR, "$ac_default_prefix/lib", [Wnn library directory])
  fi

  AC_SUBST(WNN_CPPFLAGS)
  AC_SUBST(WNN_LIBS)
  AC_SUBST(WNN_LIBADD)
])
