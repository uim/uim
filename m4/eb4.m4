dnl *
dnl * Make ready to link EB Library 3.x or 4.x.
dnl *
dnl * Copyright (c) 2000-2006  Motoyuki Kasahara
dnl *
dnl * Redistribution and use in source and binary forms, with or without
dnl * modification, are permitted provided that the following conditions
dnl * are met:
dnl * 1. Redistributions of source code must retain the above copyright
dnl *    notice, this list of conditions and the following disclaimer.
dnl * 2. Redistributions in binary form must reproduce the above copyright
dnl *    notice, this list of conditions and the following disclaimer in the
dnl *    documentation and/or other materials provided with the distribution.
dnl * 3. Neither the name of the project nor the names of its contributors
dnl *    may be used to endorse or promote products derived from this software
dnl *    without specific prior written permission.
dnl * 
dnl * THIS SOFTWARE IS PROVIDED BY THE PROJECT AND CONTRIBUTORS ``AS IS'' AND
dnl * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
dnl * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
dnl * ARE DISCLAIMED.  IN NO EVENT SHALL THE PROJECT OR CONTRIBUTORS BE LIABLE
dnl * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
dnl * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
dnl * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
dnl * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
dnl * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
dnl * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
dnl * SUCH DAMAGE.
dnl *

AC_DEFUN([eb_LIB_EB4],
[dnl
dnl *
dnl * Requirements.
dnl *
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_LIBTOOL])
AC_REQUIRE([AC_C_CONST])
AC_REQUIRE([AC_C_PROTOTYPES])
AC_REQUIRE([AC_TYPE_OFF_T])
AC_REQUIRE([AC_TYPE_SIZE_T])
AC_REQUIRE([AC_HEADER_TIME])

AC_CHECK_HEADERS(limits.h)
AC_CHECK_TYPE(ssize_t, int)

dnl *
dnl * --with-eb-conf option.
dnl *
AC_ARG_WITH(eb-conf,
AC_HELP_STRING([--with-eb-conf=FILE],
    [eb.conf file is FILE [[SYSCONFDIR/eb.conf]]]),
[ebconf="${withval}"], [ebconf=$sysconfdir/eb.conf])
if test X$prefix = XNONE; then
   PREFIX=$ac_default_prefix
else
   PREFIX=$prefix
fi
ebconf=`echo X$ebconf | sed -e 's/^X//' -e 's;\${prefix};'"$PREFIX;g" \
   -e 's;\$(prefix);'"$PREFIX;g"`

dnl *
dnl * Read eb.conf
dnl *
AC_MSG_CHECKING(for eb.conf)
AC_MSG_RESULT($ebconf)
if test -f ${ebconf}; then
   . ${ebconf}
else
   AC_MSG_ERROR($ebconf not found)
fi

if test X$EBCONF_ENABLE_PTHREAD = Xyes; then
   AC_DEFINE(EBCONF_ENABLE_PTHREAD, 1,
      [Define if EB Library supports pthread.])
fi
if test X$EBCONF_ENABLE_NLS = Xyes; then
   AC_DEFINE(EBCONF_ENABLE_NLS, 1,
      [Define if EB Library supports native language.])
fi
if test X$EBCONF_ENABLE_EBNET = Xyes; then
   AC_DEFINE(EBCONF_ENABLE_EBNET, 1,
      [Define if EB Library supports remote access.])
fi

AC_SUBST(EBCONF_EBINCS)
AC_SUBST(EBCONF_EBLIBS)
AC_SUBST(EBCONF_ZLIBINCS)
AC_SUBST(EBCONF_ZLIBLIBS)
AC_SUBST(EBCONF_PTHREAD_CPPFLAGS)
AC_SUBST(EBCONF_PTHREAD_CFLAGS)
AC_SUBST(EBCONF_PTHREAD_LDFLAGS)
AC_SUBST(EBCONF_INTLINCS)
AC_SUBST(EBCONF_INTLLIBS)

dnl *
dnl * Check for EB Library.
dnl *
AC_MSG_CHECKING(for EB Library)
save_CPPFLAGS=$CPPFLAGS
save_CFLAGS=$CFLAGS
save_LDFLAGS=$LDFLAGS
save_LIBS=$LIBS
CPPFLAGS="$CPPFLAGS $EBCONF_PTHREAD_CPPFLAGS $EBCONF_EBINCS $EBCONF_ZLIBINCS $EBCONF_INTLINCS"
CFLAGS="$CFLAGS $EBCONF_PTHREAD_CFLAGS"
LDFLAGS="$LDFAGS $EBCONF_PTHREAD_LDFLAGS"
LIBS="$LIBS $EBCONF_EBLIBS $EBCONF_ZLIBLIBS $EBCONF_INTLLIBS"
AC_TRY_LINK([#include <eb/eb.h>],
[eb_initialize_library(); return 0;],
try_eb=yes, try_eb=no)
CPPFLAGS=$save_CPPFLAGS
CFLAGS=$save_CFLAGS
LDFLAGS=$save_LDFLAGS
LIBS=$save_LIBS
AC_MSG_RESULT($try_eb)
if test ${try_eb} != yes; then
   AC_MSG_ERROR(EB Library not available)
fi
])
