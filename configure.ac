m4_define([UIM_MAJOR_VERSION],      [1])
m4_define([UIM_MINOR_VERSION],      [9])
m4_define([UIM_PATCHLEVEL_VERSION], [0])
m4_define([UIM_VERSION],
    UIM_MAJOR_VERSION.UIM_MINOR_VERSION.UIM_PATCHLEVEL_VERSION)

AC_PREREQ(2.60b)
AC_INIT([uim], [UIM_VERSION], [uim-en@googlegroups.com])
AC_CONFIG_SRCDIR([uim/uim.c])
AC_CONFIG_HEADERS([uim/config.h])
AM_INIT_AUTOMAKE([1.10 dist-bzip2])

AC_SUBST(UIM_VERSION_MAJOR, UIM_MAJOR_VERSION)
AC_SUBST(UIM_VERSION_MINOR, UIM_MINOR_VERSION)
AC_SUBST(UIM_VERSION_PATCHLEVEL, UIM_PATCHLEVEL_VERSION)

# Enable GNU extensions such as asprintf(3), BSD-originated functions,
# POSIX features and more on glibc (and some other
# implementations). See features.h. This macro should be placed here.
AC_GNU_SOURCE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_OBJC
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_DISABLE_STATIC
AC_PROG_LIBTOOL
AC_PATH_PROG(SH, sh)
AC_PATH_PROG(RSVG, rsvg-convert)
AC_PATH_PROG(CSI, csi)
AC_PATH_PROGS(MD5, md5 md5sum)
AC_PATH_PROGS(SHA1, sha1 sha1sum)
AC_PATH_PROGS(SED, sed gsed)
AX_PATH_QMAKE4
AX_PATH_QMAKE5

AM_MAINTAINER_MODE

# Checks for libraries
AC_CHECK_LIB(dl,dlopen)
AC_CHECK_LIB(socket,socket)

AX_LIB_GLIBC

dnl ***********************
dnl *** Tests for iconv ***
dnl ***********************
dnl
dnl We do this before the gettext checks, to avoid distortion

AM_ICONV
if test "$am_cv_func_iconv" != yes; then
  AC_MSG_ERROR([Could not find iconv(3) in libc or libiconv (required for uim)])
fi

AC_SUBST(LIBICONV)

dnl *************************
dnl *** Tests for m17nlib ***
dnl *************************

AC_ARG_WITH(m17nlib,
  AS_HELP_STRING([--without-m17nlib],
    [do not build against m17nlib]),
  [],
  [with_m17nlib=yes])

if test "x$with_m17nlib" != xno; then
  PKG_CHECK_MODULES(M17NLIB, m17n-shell >= 1.3.1,
                    with_m17nlib=yes, with_m17nlib=no)
  AC_CHECK_PROG(M17NDB, m17n-db, m17n-db)
  if test -n "$M17NDB"; then
    m17n_db_dir=`$M17NDB`
  fi
fi

AC_SUBST(M17NLIB_LIBS)
AC_SUBST(m17n_db_dir)
AM_CONDITIONAL(M17NLIB, test "x$with_m17nlib" = xyes)

# ***********************
# *** Tests for Anthy ***
# ***********************
AC_ARG_WITH(anthy,
  AC_HELP_STRING([--without-anthy],
    [do not build with libanthy]),
  [],
  [with_anthy=yes])

if test "x$with_anthy" != xno; then
  AC_CHECK_HEADERS([anthy/anthy.h],
    [
      saved_LIBS=$LIBS
      LIBS="$LIBS -lanthy -lanthydic"
      AC_CHECK_LIB([anthy], [anthy_init], [with_anthy=yes], [with_anthy=no])
      LIBS=$saved_LIBS
    ],
    [with_anthy=no])
fi

if test "x$with_anthy" = xyes; then
  ANTHY_LIBS="-lanthy -lanthydic"
  ANTHY_CFLAGS=""
  PKG_CHECK_EXISTS(anthy >= 8622,
                   [AC_DEFINE(LIBANTHY_UTF8_CAPABLE, 1,
                    "libanthy can handle UTF-8")])
fi
AC_SUBST(ANTHY_LIBS)
AC_SUBST(ANTHY_CFLAGS)
AM_CONDITIONAL(ANTHY, test "x$with_anthy" = xyes)

# *******************************
# *** Tests for Anthy (UTF-8) ***
# *******************************
AC_ARG_WITH(anthy-utf8,
  AC_HELP_STRING([--with-anthy-utf8],
    [build with libanthy (UTF-8)]),
  [],
  [with_anthy_utf8=no])

if test "x$with_anthy_utf8" != xno; then
  PKG_CHECK_MODULES(ANTHY_UTF8, anthy >= 8622,
                    [with_anthy_utf8=yes
                     AC_DEFINE(LIBANTHY_UTF8_CAPABLE, 1,
                     [libanthy can handle UTF-8])],
                     with_anthy_utf8=no)
fi

AM_CONDITIONAL(ANTHY_UTF8, test "x$with_anthy_utf8" = xyes)

dnl for uim-dict.xml.in
if test "x$with_anthy" = xyes || test "x$with_anthy_utf8" = xyes; then
  UI_XML_ANTHY_START=""
  UI_XML_ANTHY_END=""
else
  UI_XML_ANTHY_START="<!--"
  UI_XML_ANTHY_END="-->"
fi
AC_SUBST(UI_XML_ANTHY_START)
AC_SUBST(UI_XML_ANTHY_END)

# ***********************
# *** Tests for Canna ***
# ***********************
AC_ARG_WITH(canna,
  AS_HELP_STRING([--with-canna],
    [use Canna]),
    [],
    [with_canna=no])

AM_CONDITIONAL(CANNA, test "x$with_canna" = xyes)

# *********************
# *** Tests for Wnn ***
# *********************
AC_ARG_WITH(wnn,
  AS_HELP_STRING([--with-wnn],
    [build with libwnn]),
  [],
  [with_wnn=no])

if test "x$with_wnn" != xno; then
  AM_PATH_WNN(with_wnn=yes, with_wnn=no)
fi

AM_CONDITIONAL(WNN, test "x$with_wnn" = xyes)

# ***********************
# *** Tests for Mana ***
# ***********************
AC_ARG_WITH(mana,
  AS_HELP_STRING([--without-mana],
    [do not build a plugin for Mana]),
  [],
  [with_mana=yes])

if test "x$with_mana" != xno; then
  AC_CHECK_PROG(MANA, mana, mana)
  if test -z "$MANA"; then
    with_mana=no
  fi
fi

AM_CONDITIONAL(MANA, test "x$with_mana" = xyes)

# ***********************
# *** Tests for PRIME ***
# ***********************
AC_ARG_WITH(prime,
  AS_HELP_STRING([--without-prime],
    [do not build a plugin for PRIME]),
  [],
  [with_prime=yes])

if test "x$with_prime" != xno; then
  PKG_CHECK_MODULES(PRIME, prime >= 0.8.5.2, with_prime=yes, with_prime=no)
fi

AM_CONDITIONAL(PRIME, test "x$with_prime" = xyes)

# *********************
# *** Tests for SJ3 ***
# *********************
AC_ARG_WITH(sj3,
  AS_HELP_STRING([--with-sj3],
    [use SJ3]),
  [],
  [with_sj3=no])

AM_CONDITIONAL(SJ3, test "x$with_sj3" = xyes)

# *********************
# *** Tests for SKK ***
# *********************
AC_ARG_WITH(skk,
  AS_HELP_STRING([--without-skk],
    [do not build a plugin for SKK]),
  [],
  [with_skk=yes])

AM_CONDITIONAL(SKK, test "x$with_skk" = xyes)

# **********************
# *** Tests for cURL ***
# **********************
AC_ARG_WITH(curl,
  AS_HELP_STRING([--with-curl],
    [build with libcurl]),
  [],
  [with_curl=no])

if test "x$with_curl" != xno; then
  PKG_CHECK_MODULES(CURL, libcurl >= 7.16.4, with_curl=yes, with_curl=no)
fi

AM_CONDITIONAL(CURL, test "x$with_curl" = xyes)

# ***********************
# *** Tests for expat ***
# ***********************
expat_found=no
AM_WITH_EXPAT
AM_CONDITIONAL(EXPAT, test "x$expat_found" = xyes)

# *************************
# *** Tests for OpenSSL ***
# *************************
AC_ARG_ENABLE(openssl,
  AC_HELP_STRING([--enable-openssl],
    [enable OpenSSL]),
  [],
  [enable_openssl=no])

if test "x$enable_openssl" != xno; then
  AM_WITH_OPENSSL
fi

AM_CONDITIONAL(OPENSSL, test "x$enable_openssl" = xyes)

# *************************
# *** Tests for SQLite3 ***
# *************************
AC_ARG_WITH(sqlite3,
  AS_HELP_STRING([--with-sqlite3],
    [build with SQLite3]),
  [],
  [with_sqlite3=yes])

if test "x$with_sqlite3" = xyes; then
  PKG_CHECK_MODULES(SQLITE3, sqlite3 >= 3.0.0,
                    with_sqlite3=yes, with_sqlite3=no)
fi

AM_CONDITIONAL(SQLITE3, test "x$with_sqlite3" = xyes)

# ************************
# *** Tests for libffi ***
# ************************
AC_ARG_WITH(ffi,
  AS_HELP_STRING([--with-ffi],
    [build with libffi]),
  [],
  [with_ffi=no])

if test "x$with_ffi" = xyes; then
  PKG_CHECK_MODULES(FFI, libffi >= 3.0.0, with_ffi=yes, with_ffi=no)
fi

AM_CONDITIONAL(FFI, test "x$with_ffi" = xyes)

PKG_CHECK_MODULES(X11, x11, x11_use_new_dir=yes, x11_use_new_dir=no)
AC_PATH_XTRA

use_xim=no
if test x"$have_x" != xdisabled && test x"$have_x" != xno; then
  PKG_CHECK_MODULES(XIM, xext, use_xim=yes, use_xim=no)
  if test "x$use_xim" = xyes; then
    AC_CXX_NAMESPACES
    AC_CXX_HAVE_STL
    if test $ac_cv_cxx_have_stl = no; then
      use_xim=no
    fi
  fi
  AC_MSG_CHECKING([whether to have Xft support])
  AM_PATH_XFT(yes, XFT=true, XFT=false)
  if test x"$XFT" = xtrue; then
    saved_CFLAGS=$CFLAGS
    saved_LIBS=$LIBS
    CFLAGS="$CFLAGS $XFT_CFLAGS"
    LIBS="$LIBS $XFT_LIBS"
    AC_TRY_LINK([
#include <X11/Xft/Xft.h>], [ XftFontClose(0, 0); return 1; ], 
      [
        AC_DEFINE(WITH_XFT, 1, [font antialiasing support])
        AC_MSG_CHECKING([Xft UTF-8 support])
        AC_TRY_LINK([
#include <X11/Xft/Xft.h>
          ], [ 
XftDrawStringUtf8(0, 0, 0, 0, 0, 0, 0); return 0; 
          ],
            AC_DEFINE(HAVE_XFT_UTF8_STRING, 1, "Xft UTF8 support")
            AC_MSG_RESULT(yes),
            AC_MSG_RESULT(no)) 
      ],
      [
        AC_MSG_RESULT([***Could not link with Xft. Install Xft if you want support for it***])
        XFT=false
        XFT_CFLAGS=
        XFT_LIBS=
      ])
    CFLAGS=$saved_CFLAGS
    LIBS=$saved_LIBS
  fi
fi

AC_XKB

AM_CONDITIONAL(XIM, test x"$use_xim" = xyes)
AM_CONDITIONAL(WITH_XFT, test x"$XFT" = xtrue)

# Checks for header files
AC_HEADER_STDC
AC_HEADER_STDBOOL
AC_CHECK_HEADERS([stdint.h inttypes.h sys/inttypes.h sys/types.h])
AC_CHECK_HEADERS([fcntl.h locale.h stdlib.h unistd.h errno.h])
AC_CHECK_HEADERS([string.h sys/ioctl.h sys/socket.h termios.h sys/termios.h wchar.h])
AC_CHECK_HEADERS([sys/time.h sys/stat.h sys/un.h getopt.h assert.h signal.h term.h ncurses/term.h ctype.h pwd.h stdarg.h])
AC_CHECK_HEADERS([pty.h utmp.h util.h libutil.h])
AC_CHECK_HEADERS([curses.h stropts.h])
AC_CHECK_HEADERS([sys/param.h strings.h netdb.h sysexits.h])
AC_CHECK_HEADERS([poll.h sys/poll.h])

# Check for types
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INTMAX_T
AC_TYPE_INTPTR_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINTMAX_T
AC_TYPE_UINTPTR_T

AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_OFF_T

AC_TYPE_PID_T
AC_TYPE_SIGNAL
AC_CHECK_TYPES(sig_atomic_t, , ,
  [ #include <signal.h> ])
AC_CHECK_TYPES(sig_t, , ,
  [ #include <signal.h> ])

# Checks for structures

# Checks for compiler characteristics
AC_C_CONST

# FIXME: depending on the internal variable name
AC_DEFUN([AX_LANG_WNOERROR], [ac_[]_AC_LANG_ABBREV[]_werror_flag=])

AC_LANG_WERROR  # Turn warning-only unknown options into error.

AX_CFLAGS_GCC_OPTION([-pedantic])
AX_CFLAGS_GCC_OPTION([-pipe])
AX_CFLAGS_GCC_OPTION([-Wall])
AX_CFLAGS_GCC_OPTION([-Wchar-subscripts])
AX_CFLAGS_GCC_OPTION([-Wmissing-declarations])
AX_CFLAGS_GCC_OPTION([-Wredundant-decls])
AX_CFLAGS_GCC_OPTION([-Wmissing-prototypes])
AX_CFLAGS_GCC_OPTION([-Wnested-externs])
AX_CFLAGS_GCC_OPTION([-Wpointer-arith])
AX_CFLAGS_GCC_OPTION([-Wcast-align])
AX_CFLAGS_GCC_OPTION([-Wsign-compare])
AX_CFLAGS_GCC_OPTION([-Wno-long-long])

# Suppress warnings about strings longer than ISO C 89 maximum length (509).
AX_CFLAGS_GCC_OPTION([-Wno-overlength-strings])

# Disable the problematic preprocessing on Mac OS X
AX_CFLAGS_GCC_OPTION([-no-cpp-precomp])

# Deprecated autoconf macro used in AX_CXXFLAGS_GCC_OPTION.
AC_DEFUN([AC_LANG_CXX], [AC_LANG([C++])])

AX_CXXFLAGS_GCC_OPTION([-pipe])
AX_CXXFLAGS_GCC_OPTION([-W])
AX_CXXFLAGS_GCC_OPTION([-Wall])
AX_CXXFLAGS_GCC_OPTION([-Wchar-subscripts])
AX_CXXFLAGS_GCC_OPTION([-Wnon-virtual-dtor])
AX_CXXFLAGS_GCC_OPTION([-Wno-long-long])
AX_CXXFLAGS_GCC_OPTION([-Wcast-align])
AX_CXXFLAGS_GCC_OPTION([-Wconversion])
AX_CXXFLAGS_GCC_OPTION([-Wchar-subscripts])
AX_CXXFLAGS_GCC_OPTION([-Wpointer-arith])
AX_CXXFLAGS_GCC_OPTION([-Wwrite-strings])
AX_CXXFLAGS_GCC_OPTION([-Wformat-security])

AX_LANG_WNOERROR  # end AC_LANG_WERROR

AC_CHECK_DECL(LLONG_MAX, have_llong_max=1, , [#include <limits.h>])
# compute LLONG_MIN and LLONG_MAX if we don't know them.
if test -z "$have_llong_max"; then
	AC_MSG_CHECKING([for max value of long long])
	AC_RUN_IFELSE(
		[AC_LANG_SOURCE([[
#include <stdio.h>
/* Why is this so damn hard? */
#ifdef __GNUC__
# undef __GNUC__
#endif
#define __USE_ISOC99
#include <limits.h>
#define DATA "conftest.llminmax"
#define my_abs(a) ((a) < 0 ? ((a) * -1) : (a))

/*
 * printf in libc on some platforms (eg old Tru64) does not understand %lld so
 * we do this the hard way.
 */
static int
fprint_ll(FILE *f, long long n)
{
	unsigned int i;
	int l[sizeof(long long) * 8];

	if (n < 0)
		if (fprintf(f, "-") < 0)
			return -1;
	for (i = 0; n != 0; i++) {
		l[i] = my_abs(n % 10);
		n /= 10;
	}
	do {
		if (fprintf(f, "%d", l[--i]) < 0)
			return -1;
	} while (i != 0);
	if (fprintf(f, " ") < 0)
		return -1;
	return 0;
}

int main(void) {
	FILE *f;
	long long i, llmin, llmax = 0, llmin1, llmax1;

	if((f = fopen(DATA,"w")) == NULL)
		exit(1);

#if defined(LLONG_MIN) && defined(LLONG_MAX)
	fprintf(stderr, "Using system header for LLONG_MIN and LLONG_MAX\n");
	llmin = LLONG_MIN;
	llmax = LLONG_MAX;
#else
	fprintf(stderr, "Calculating  LLONG_MIN and LLONG_MAX\n");
	/* This will work on one's complement and two's complement */
	for (i = 1; i > llmax; i <<= 1, i++)
		llmax = i;
	llmin = llmax + 1LL;    /* wrap */
#endif

	/* Sanity check */
	llmin1 = llmin - 1;
	llmax1 = llmax + 1;
	if (llmin + 1 < llmin || llmin1 < llmin || llmax1 > llmax
	    || llmax - 1 > llmax || llmin == llmax || llmin == 0
	    || llmax == 0 || llmax < LONG_MAX || llmin > LONG_MIN) {
		fprintf(f, "unknown unknown\n");
		exit(2);
	}

	if (fprint_ll(f, llmin) < 0)
		exit(3);
	if (fprint_ll(f, llmax) < 0)
		exit(4);
	if (fclose(f) < 0)
		exit(5);
	exit(0);
}
		]])],
		[
			llong_min=`$AWK '{print $1}' conftest.llminmax`
			llong_max=`$AWK '{print $2}' conftest.llminmax`

			AC_MSG_RESULT($llong_max)
			AC_DEFINE_UNQUOTED(LLONG_MAX, [${llong_max}LL],
			    [max value of long long calculated by configure])
			AC_MSG_CHECKING([for min value of long long])
			AC_MSG_RESULT($llong_min)
			AC_DEFINE_UNQUOTED(LLONG_MIN, [${llong_min}LL],
			    [min value of long long calculated by configure])
		],
		[
			AC_MSG_RESULT(not found)
		],
		[
			AC_MSG_WARN([cross compiling: not checking])
		]
	)
fi

# Checks for library functions
AC_FUNC_REALLOC
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AX_FUNC_SIGSETJMP
AC_FUNC_FORK
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([cfmakeraw])
AC_CHECK_FUNCS([wcswidth])
# BSD functions
AC_CHECK_FUNCS([strdup])
# GNU functions
AC_CHECK_FUNCS([asprintf vasprintf])
# C99 functions
AC_CHECK_FUNCS([snprintf vsnprintf])
# FIXME: replace non-standard perror(3)
AC_REPLACE_FUNCS([getpeereid strsep setenv unsetenv strlcpy strlcat strtonum strtoll])
AC_CHECK_FUNCS([getpid stat mkdir chmod])
AC_CHECK_FUNCS([execv execvp])
AC_CHECK_FUNCS([isascii])
AC_CHECK_FUNCS([getaddrinfo freeaddrinfo getnameinfo])
AC_CHECK_FUNCS([issetugid])
AC_CHECK_FUNCS([poll])
AC_CHECK_FUNCS([waitpid])
AC_CHECK_FUNCS([daemon])

# IRIX has a const char return value for gai_strerror()
AC_CHECK_FUNCS(gai_strerror,[
        AC_DEFINE(HAVE_GAI_STRERROR)
        AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

const char *gai_strerror(int);],[
char *str;

str = gai_strerror(0);],[
                AC_DEFINE(HAVE_CONST_GAI_STRERROR_PROTO, 1,
                [Define if gai_strerror() returns const char *])])])

# Check for broken snprintf
if test "x$ac_cv_func_snprintf" = xyes; then
	AC_MSG_CHECKING([whether snprintf correctly terminates long strings])
	AC_RUN_IFELSE(
		[AC_LANG_SOURCE([[
#include <stdio.h>
int main(void){char b[5];snprintf(b,5,"123456789");exit(b[4]!='\0');}
		]])],
		[AC_MSG_RESULT(yes)],
		[
			AC_MSG_RESULT(no)
			AC_DEFINE(BROKEN_SNPRINTF, 1,
				[Define if your snprintf is busted])
			AC_MSG_WARN([****** Your snprintf() function is broken, complain to your vendor])
		],
		[ AC_MSG_WARN([cross compiling: Assuming working snprintf()]) ]
	)
fi

# If we don't have a working asprintf, then we strongly depend on vsnprintf
# returning the right thing on overflow: the number of characters it tried to
# create (as per SUSv3)
if test "x$ac_cv_func_asprintf" != xyes && \
   test "x$ac_cv_func_vsnprintf" = xyes; then
	AC_MSG_CHECKING([whether vsnprintf returns correct values on overflow])
	AC_RUN_IFELSE(
		[AC_LANG_SOURCE([[
#include <sys/types.h>
#include <stdio.h>
#include <stdarg.h>

int x_snprintf(char *str,size_t count,const char *fmt,...)
{
	size_t ret; va_list ap;
	va_start(ap, fmt); ret = vsnprintf(str, count, fmt, ap); va_end(ap);
	return ret;
}
int main(void)
{
	char x[1];
	exit(x_snprintf(x, 1, "%s %d", "hello", 12345) == 11 ? 0 : 1);
} ]])],
		[AC_MSG_RESULT(yes)],
		[
			AC_MSG_RESULT(no)
			AC_DEFINE(BROKEN_SNPRINTF, 1,
				[Define if your snprintf is busted])
			AC_MSG_WARN([****** Your vsnprintf() function is broken, complain to your vendor])
		],
		[ AC_MSG_WARN([cross compiling: Assuming working vsnprintf()]) ]
	)
fi

# On systems where [v]snprintf is broken, but is declared in stdio,
# check that the fmt argument is const char * or just char *.
# This is only useful for when BROKEN_SNPRINTF
AC_MSG_CHECKING([whether snprintf can declare const char *fmt])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
	   int snprintf(char *a, size_t b, const char *c, ...) { return 0; }
	   int main(void) { snprintf(0, 0, 0); } 
    ]])],
   [AC_MSG_RESULT(yes)
    AC_DEFINE(SNPRINTF_CONST, [const],
              [Define as const if snprintf() can declare const char *fmt])],
   [AC_MSG_RESULT(no)
    AC_DEFINE(SNPRINTF_CONST, [/* not const */])])

AC_SEARCH_LIBS(dlfunc,dl,
  [
    AC_DEFINE(HAVE_DLFUNC, 1,
              [Define to 1 if you have the dlfunc function.])

  ])

# for uim-fep
AM_LANGINFO_CODESET

dnl Solaris 9 needs -lresolv for inet_aton
NETLIBS=""
AC_CHECK_FUNC(inet_aton, ,
  [AC_CHECK_LIB(resolv, inet_aton, [NETLIBS="-lresolv"])])
AC_SUBST(NETLIBS)

dnl socket related
AC_CACHE_CHECK([for struct sockaddr_storage], ac_cv_have_struct_sockaddr_storage, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <sys/socket.h>
		],
		[ struct sockaddr_storage s; ],
		[ ac_cv_have_struct_sockaddr_storage=yes ],
		[ ac_cv_have_struct_sockaddr_storage=no ]
	)
])
if test "x$ac_cv_have_struct_sockaddr_storage" = xyes; then
	AC_DEFINE(HAVE_STRUCT_SOCKADDR_STORAGE, 1,
		[define if you have struct sockaddr_storage data type])
fi

dnl cred related
AC_CACHE_CHECK([for struct cmsgcred], ac_cv_have_struct_cmsgcred, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <sys/socket.h>
		],
		[ struct cmsgcred s; ],
		[ ac_cv_have_struct_cmsgcred=yes ],
		[ ac_cv_have_struct_cmsgcred=no ]
	)
])
if test "x$ac_cv_have_struct_cmsgcred" = xyes; then
	AC_DEFINE(HAVE_STRUCT_CMSGCRED, 1,
		[define if you have struct cmsgcred data type])
fi

AC_CACHE_CHECK([for struct sockaddr_in6], ac_cv_have_struct_sockaddr_in6, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <netinet/in.h>
		],
		[ struct sockaddr_in6 s; s.sin6_family = 0; ],
		[ ac_cv_have_struct_sockaddr_in6=yes ],
		[ ac_cv_have_struct_sockaddr_in6=no ]
	)
])
if test "x$ac_cv_have_struct_sockaddr_in6" = xyes; then
	AC_DEFINE(HAVE_STRUCT_SOCKADDR_IN6, 1,
		[define if you have struct sockaddr_in6 data type])
fi

AC_CACHE_CHECK([for struct in6_addr], ac_cv_have_struct_in6_addr, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <netinet/in.h>
		],
		[ struct in6_addr s; s.s6_addr[0] = 0; ],
		[ ac_cv_have_struct_in6_addr=yes ],
		[ ac_cv_have_struct_in6_addr=no ]
	)
])
if test "x$ac_cv_have_struct_in6_addr" = xyes; then
	AC_DEFINE(HAVE_STRUCT_IN6_ADDR, 1,
		[define if you have struct in6_addr data type])
fi

AC_CACHE_CHECK([for struct addrinfo], ac_cv_have_struct_addrinfo, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
		],
		[ struct addrinfo s; s.ai_flags = AI_PASSIVE; ],
		[ ac_cv_have_struct_addrinfo=yes ],
		[ ac_cv_have_struct_addrinfo=no ]
	)
])
if test "x$ac_cv_have_struct_addrinfo" = xyes; then
	AC_DEFINE(HAVE_STRUCT_ADDRINFO, 1,
		[define if you have struct addrinfo data type])
fi

GETTEXT_PACKAGE=uim
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE",[gettext package name])

AM_GNU_GETTEXT([external], [need-ngettext])
AM_GNU_GETTEXT_VERSION([0.17])

# FIXME: Probably voilent way  -- YamaKen 2005-01-07
if test "x$prefix" = xNONE; then
  prefix=$ac_default_prefix
fi
if test "x$exec_prefix" = xNONE; then
  exec_prefix=$prefix
fi

# FIXME: complicated directory handlings  -- YamaKen 2006-07-08
# FIXME: double eval workaround for datarootdir
LIBDIR=`eval eval echo ${libdir}`
AC_DEFINE_UNQUOTED(LIBDIR, "$LIBDIR", [libraries dir])
DATADIR=`eval eval echo ${datadir}`
AC_DEFINE_UNQUOTED(DATADIR, "$DATADIR", [read-only architecture-independent data dir])
UIM_LIBEXECDIR=`eval echo "${libexecdir}"`
AC_DEFINE_UNQUOTED(UIM_LIBEXECDIR, "$UIM_LIBEXECDIR", [libexec dir])
AC_SUBST(UIM_LIBEXECDIR)
# we should use more safe macro such as AC_EXPAND_DIR
LOCALEDIR=`eval eval echo ${datadir}/locale`
# backward compatibility
localedir=$LOCALEDIR
AC_DEFINE_UNQUOTED(LOCALEDIR, "$LOCALEDIR", [locale dir])
AC_DEFINE_UNQUOTED(GNOMELOCALEDIR, "$LOCALEDIR", [locale dir for GNOME])
AC_DEFINE_UNQUOTED(GNOME3LOCALEDIR, "$LOCALEDIR", [locale dir for GNOME3])
# define XLIB directory for Compose file
if test "x$x11_use_new_dir" = xyes; then
  AC_DEFINE_UNQUOTED(XLIB_DIR, "`$PKG_CONFIG x11 --variable=prefix`/share", [X11 Library Directory])
else
  AC_DEFINE_UNQUOTED(XLIB_DIR, "$x_libraries", [X11 Library Directory])
fi
uim_pixmapsdir=`eval eval echo ${datadir}/${PACKAGE_TARNAME}/pixmaps`
AC_DEFINE_UNQUOTED(UIM_PIXMAPSDIR, "${uim_pixmapsdir}", [pixmaps directory])
AC_SUBST(uim_pixmapsdir)

dnl ******************************
dnl *** Check for GTK+ Library ***
dnl ******************************
AC_ARG_WITH(gtk2,
  AC_HELP_STRING([--without-gtk2],
    [do not build against GTK+2]),
  [],
  [with_gtk2=yes])

if test "x$with_gtk2" != xno; then
  PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.2.0 gdk-x11-2.0,
                    with_gtk2=yes, with_gtk2=no)
  PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0 gdk-x11-2.0,
                    with_gtk2_4=yes, with_gtk2_4=no)
fi

AM_CONDITIONAL(GTK2, test "x$with_gtk2" = xyes)
AM_CONDITIONAL(GTK2_4, test "x$with_gtk2_4" = xyes)

if test "x$with_gtk2" = xyes; then
  GTK_BINARY_VERSION=`$PKG_CONFIG gtk+-2.0 --variable=gtk_binary_version`
  GTK_LIBDIR=`$PKG_CONFIG gtk+-2.0 --variable=libdir`
  AC_SUBST(GTK_BINARY_VERSION)
  AC_SUBST(GTK_LIBDIR)
  AC_DEFINE(USE_GTK2, 1, [use GTK+2])
fi

AC_ARG_ENABLE(gnome-applet,
  AC_HELP_STRING([--disable-gnome-applet],
    [disable uim applet for GNOME2 panel]),
  [],
  [enable_gnome_applet=yes])

if test "x$enable_gnome_applet" != xno; then
  PKG_CHECK_MODULES(GNOME_APPLET, libpanelapplet-2.0,
                    enable_gnome_applet=yes, enable_gnome_applet=no)
  PKG_CHECK_MODULES(APPLET_2_14, libpanelapplet-2.0 >= 2.14.0,
                    panelapplet_have_set_background_api=yes,
                    panelapplet_have_set_background_api=no)
fi

if test x"$panelapplet_have_set_background_api" = xyes; then
  AC_DEFINE(LIBPANEL_APPLET_HAVE_SET_BACKGROUND_WIDGET, 1 , [libpanel-applet have set_background api])
fi

AM_CONDITIONAL(GNOME_APPLET, test "x$enable_gnome_applet" = xyes)

dnl *******************************
dnl *** Check for GTK+3 Library ***
dnl *******************************
AC_ARG_WITH(gtk3,
  AC_HELP_STRING([--without-gtk3],
    [do not build GTK+3 immodule and helper applications]),
  [],
  [with_gtk3=yes])

if test "x$with_gtk3" != xno; then
  PKG_CHECK_MODULES(GTK3, gtk+-3.0, with_gtk3=yes, with_gtk3=no)
fi

AM_CONDITIONAL(GTK3, test "x$with_gtk3" = xyes)

if test "x$with_gtk3" = xyes; then
  AC_DEFINE(USE_GTK3, 1, [use GTK+3])
  GTK3_BINARY_VERSION=`$PKG_CONFIG gtk+-3.0 --variable=gtk_binary_version`
  GTK3_LIBDIR=`$PKG_CONFIG gtk+-3.0 --variable=libdir`
  AC_SUBST(GTK3_BINARY_VERSION)
  AC_SUBST(GTK3_LIBDIR)
fi

AC_ARG_ENABLE(gnome3-applet,
  AC_HELP_STRING([--disable-gnome3-applet],
    [disable uim applet for GNOME3 panel]),
  [],
  [enable_gnome3_applet=yes])

if test "x$enable_gnome3_applet" != xno; then
  PKG_CHECK_MODULES(GNOME3_APPLET, libpanelapplet-4.0,
                    enable_gnome3_applet=yes, enable_gnome3_applet=no)
fi

AM_CONDITIONAL(GNOME3_APPLET, test "x$enable_gnome3_applet" = xyes)

dnl ****************************
dnl *** Check for Qt Library ***
dnl ****************************
AC_ARG_WITH(qt,
  AC_HELP_STRING([--with-qt],
    [build Qt3 helper applications]),
  [],
  [with_qt=no])

if test "x$with_qt" = xyes; then
  # minimum version
  QT_VERSION=3.2.0
  QT_VERSION_NUM=0x030200

  # GUESS $QTDIR
  if test ! -d "$QTDIR"; then
    QTDIR="/usr/lib/qt3"
  fi
  if test ! -d "$QTDIR"; then
    QTDIR="/usr/lib/qt"
  fi
  if test ! -d "$QTDIR"; then
    QTDIR="/usr"
  fi
  AC_MSG_CHECKING(for Qt3 library dir)
  if test ! -d "$QTDIR"; then
    AC_MSG_RESULT(no)
  else
    AC_MSG_RESULT($QTDIR)
  fi

  # GUESS $QTINCDIR from $QTDIR
  QTINCDIR="$QTDIR/include/qt"
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="$QTDIR/include/X11/qt"
  fi
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="$QTDIR/include/qt3"
  fi
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="$QTDIR/include"
  fi
  # GUESS $QTINCDIR independently
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="/usr/include/X11/qt"
  fi
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="/usr/include/qt3"
  fi
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="/usr/include/qt"
  fi
  if test ! -d "$QTINCDIR"; then
    QTINCDIR="/usr/include"
  fi

  # GUESS $QTLIBDIR
  QTLIBDIR="$QTDIR/lib64"
  if test ! -d "$QTLIBDIR"; then
    QTLIBDIR="$QTDIR/lib"
  fi
  if test ! -d "$QTLIBDIR"; then
    QTLIBDIR="/usr/lib/qt3"
  fi
  if test ! -d "$QTLIBDIR"; then
    QTLIBDIR="/usr/lib/qt"
  fi
  if test ! -d "$QTLIBDIR"; then
    QTLIBDIR="/usr/lib"
  fi

  # Check For MOC
  if test -x "$QTDIR/bin/moc"; then
    HOST_MOC="$QTDIR/bin/moc"
  else
    AC_CHECK_PROGS(HOST_MOC, moc-qt3 moc, "")
  fi
  if test -z "$HOST_MOC"; then
    AC_MSG_ERROR([no acceptable moc( meta object compiler ) found])
  fi
  MOC=$HOST_MOC

  # Check For UIC
  if test -x "$QTDIR/bin/uic"; then
    HOST_UIC="$QTDIR/bin/uic"
  else
    AC_CHECK_PROGS(HOST_UIC, uic-qt3 uic, "")
  fi
  if test -z "$HOST_UIC"; then
    AC_MSG_ERROR([no acceptable uic( user interface compiler ) found])
  fi
  UIC=$HOST_UIC

  # GUESS plugins dir for immodule installation
  AC_MSG_CHECKING(for qt-immodule plugins dir)
  if test ! -d "$QTDIR/plugins"; then
    AC_MSG_RESULT(no)
  else
    QT_PLUGINSDIR=$QTDIR/plugins
    AC_MSG_RESULT($QT_PLUGINSDIR)
    AC_SUBST(QT_PLUGINSDIR)
  fi

  # Process for compiler & linker flags
  QT_CXXFLAGS="-I${QTINCDIR} -DQT_GENUINE_STR -DQT_NO_STL"
  if test -z "$enable_debug"; then
    QT_CXXFLAGS="$QT_CXXFLAGS -DQT_NO_DEBUG -DNO_DEBUG"
  fi
  _SAVE_LDFLAGS=$LDFLAGS
  QT_LDFLAGS=-L${QTLIBDIR}
  LDFLAGS="$LDFLAGS $QT_LDFLAGS"
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_CHECK_LIB(qt, main, QT_LIB=-lqt,
      AC_CHECK_LIB(qt-mt, main, QT_LIB=-lqt-mt,
          AC_MSG_ERROR([Cannot find QT libraries.])))

  if test "$QT_LIB" = "-lqt-mt"; then
    QT_CXXFLAGS="$QT_CXXFLAGS -DQT_THREAD_SUPPORT"
  fi

  LDFLAGS=$_SAVE_LDFLAGS
  QT_LIBS="$X_LIBS $QT_LDFLAGS $QT_LIB -lXext -lX11"

  _SAVE_CXXFLAGS=$CXXFLAGS
  _SAVE_LIBS=$LIBS

  CXXFLAGS="$CXXFLAGS $QT_CXXFLAGS"
  LIBS="$LIBS $QT_LIBS"

  AC_MSG_CHECKING(Qt - version >= $QT_VERSION)
  AC_TRY_COMPILE([#include <qglobal.h>],
  [
      #if (QT_VERSION < $QT_VERSION_NUM)
          #error  "QT_VERSION too old"
      #endif
  ],result=yes,result=no)

  AC_MSG_RESULT("$result")
  if test "x$result" = xno; then
    with_qt=no
    AC_MSG_WARN([Qt Helper requires at least version $QT_VERSION of Qt])
  fi
  CXXFLAGS=$_SAVE_CXXFLAGS
  LIBS=$_SAVE_LIBS

  AC_LANG_RESTORE

  UIM_QT_LDFLAGS=$QT_LIBS
  UIM_QT_CXXFLAGS=$QT_CXXFLAGS

  AC_SUBST(MOC)
  AC_SUBST(UIC)
  AC_SUBST(UIM_QT_CXXFLAGS)
  AC_SUBST(UIM_QT_LDFLAGS)
fi

AC_PATH_PROG(KDE_CONFIG, [kde-config], [no])
if test "x$with_qt" = xyes; then
  if test "x$KDE_CONFIG" != xno; then
    KDE_ICONDIR=`$KDE_CONFIG --expandvars --install icon`
  fi
  if test -z "$KDE_ICONDIR"; then
    KDE_ICONDIR="$prefix/share/icons"
  fi
  AC_DEFINE_UNQUOTED(KDE_ICONDIR, "$KDE_ICONDIR", [kde icon directory])
fi

AM_CONDITIONAL(QT, test "x$with_qt" = xyes)

AC_ARG_WITH(qt-immodule,
  AC_HELP_STRING([--with-qt-immodule],
    [build Qt3-immodule extension]),
  [],
  [with_qt_immodule=no])

if test "x$with_qt_immodule" = xyes; then
  AC_MSG_CHECKING(for qt-immodule patch)
  # Check for immodule for Qt patch
  if test ! -f $QTINCDIR/qinputcontext.h \
      || test ! -f $QTINCDIR/qinputcontextplugin.h; then
    AC_MSG_RESULT(no)
    with_qt_immodule=no
  else
    AC_MSG_RESULT(yes)
  fi
fi

AM_CONDITIONAL(QT_IMMODULE, test "x$with_qt_immodule" = xyes)

dnl *****************************
dnl *** Check for Qt4 Library ***
dnl *****************************
AC_ARG_WITH(qt4,
  AC_HELP_STRING([--with-qt4],
    [build Qt4 helper applications]),
  [],
  [with_qt4=no])

AM_CONDITIONAL(QT4, test "x$with_qt4" = xyes)

AC_ARG_WITH(qt4-immodule,
  AC_HELP_STRING([--with-qt4-immodule],
    [build Qt4-immodule extension]),
  [],
  [with_qt4_immodule=no])

AM_CONDITIONAL(QT4_IMMODULE, test "x$with_qt4_immodule" = xyes)

if test "x$with_qt4" = xyes || test "x$with_qt4_immodule" = xyes; then
  if test -z "$QMAKE4"; then
    AC_MSG_ERROR([no qmake for Qt4 found])
  fi
fi

dnl *****************************************
dnl *** test for Qt3Support module in Qt4 ***
dnl *****************************************
AC_ARG_ENABLE(qt4-qt3support,
  AC_HELP_STRING([--disable-qt4-qt3support],
    [disable support for Qt3Support module in Qt4]),
  [],
  [enable_qt4_qt3support=yes])

if test "x$enable_qt4_qt3support" != xno; then
  if test "x$with_qt4_immodule" = xyes; then
    if test -f `${QMAKE4} -query QT_INSTALL_LIBS`/libQt3Support.so; then
      enable_qt4_qt3support=yes
    else
      enable_qt4_qt3support=no
      AC_MSG_WARN([No Qt3Support found])
    fi
  else
    enable_qt4_qt3support=no
  fi
fi

dnl *****************************
dnl *** Check for Qt5 Library ***
dnl *****************************
AC_ARG_WITH(qt5,
  AC_HELP_STRING([--with-qt5],
    [build Qt5 helper applications]),
  [],
  [with_qt5=no])

AM_CONDITIONAL(QT5, test "x$with_qt5" = xyes)

AC_ARG_WITH(qt5-immodule,
  AC_HELP_STRING([--with-qt5-immodule],
    [build Qt5-immodule extension]),
  [],
  [with_qt5_immodule=no])

if test "x$with_qt5" = xyes || test "x$with_qt5_immodule" = xyes; then
  if test -z "$QMAKE5"; then
    AC_MSG_ERROR([no qmake for Qt5 found])
  fi
fi

AM_CONDITIONAL(QT5_IMMODULE, test "x$with_qt5_immodule" = xyes)

AC_ARG_ENABLE(default-toolkit,
  AC_HELP_STRING([--enable-default-toolkit],
    [determine default toolkit (gtk, gtk3, qt, qt4 or qt5) @<:@default=gtk@:>@]),
  [],
  [enable_default_toolkit=no])

default_toolkit=gtk
if test "x$enable_default_toolkit" != xno; then
  if test "x$enable_default_toolkit" = xgtk && \
     test "x$with_gtk2" = xyes; then
    default_toolkit=gtk
  elif test "x$enable_default_toolkit" = xgtk3 && \
       test "x$with_gtk3" = xyes; then
    default_toolkit=gtk3
  elif test "x$enable_default_toolkit" = xqt && \
       test "x$with_qt" = xyes; then
    default_toolkit=qt
  elif test "x$enable_default_toolkit" = xqt4 && \
       test "x$with_qt4" = xyes; then
    default_toolkit=qt4
  elif test "x$enable_default_toolkit" = xqt5 && \
       test "x$with_qt5" = xyes; then
    default_toolkit=qt5
  fi
fi

AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK, test "x$default_toolkit" = xgtk)
AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK3, test "x$default_toolkit" = xgtk3)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT,  test "x$default_toolkit" = xqt)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT4, test "x$default_toolkit" = xqt4)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT5, test "x$default_toolkit" = xqt5)

AC_ARG_ENABLE(debug,
  AC_HELP_STRING([--enable-debug],
    [enable debugging]),
  [],
  [enable_debug=no])

AM_CONDITIONAL(DEBUG, test "x$enable_debug" = xyes)

AC_ARG_ENABLE(fep,
  AC_HELP_STRING([--disable-fep],
    [disable uim-fep]),
  [],
  [enable_fep=yes])

if test "x$enable_fep" != xno; then
  enable_fep=yes
  AC_CHECK_LIB(curses, setupterm, FEP_LIBADD="-lcurses $FEP_LIBADD",
    [AC_CHECK_LIB(ncurses, setupterm, FEP_LIBADD="-lncurses $FEP_LIBADD",
      AC_MSG_WARN([fep needs setupterm in libcurses or libncurses. disabled...]);enable_fep=no)])
  AC_CHECK_FUNCS(forkpty)
  if test "x$ac_cv_func_forkpty" = xno; then
    AC_CHECK_LIB(util, forkpty,
      [AC_DEFINE(HAVE_FORKPTY) FEP_LIBADD="-lutil $FEP_LIBADD"])
  fi
  AC_SUBST(FEP_LIBADD)
fi

AM_CONDITIONAL(UIM_FEP, test "x$enable_fep" = xyes)

AC_ARG_ENABLE(emacs,
  AC_HELP_STRING([--disable-emacs],
    [disable uim.el]),
  [],
  [enable_emacs=yes])

if test "x$enable_emacs" != xno; then
  AM_PATH_LISPDIR
  AC_SUBST(UIMEL_LISP_DIR, $lispdir/uim-el)
fi

AM_CONDITIONAL(UIM_EL, test "x$enable_emacs" = xyes)

AC_ARG_ENABLE(dict,
  AC_HELP_STRING([--enable-dict],
    [build dictionary utility for uim (Anthy and Canna)]),
  [],
  [enable_dict=no])

if test "x$enable_dict" != xno; then
  PKG_CHECK_MODULES(DICT, gtk+-2.0 >= 2.4.0,
    enable_dict=yes, enable_dict=no)

  if test "x$enable_dict" = xno; then
    PKG_CHECK_MODULES(DICT, gtk+-3.0, enable_dict=yes, enable_dict=no)
  fi

  if test "x$enable_dict" = xno; then
    AC_MSG_WARN([dict needs GTK+ 2 (> 2.4) or GTK+ 3. dict disabled...])
  fi

  dnl Tests for -lcanna for dict
  if test "x$enable_dict" = xyes; then
    AC_CHECK_HEADERS([canna/RK.h],
      [AC_CHECK_LIB(canna, RkInitialize,
        enable_dict_canna=yes, enable_dict_canna=no)],
      [enable_dict_canna=no],
      [ #undef _WCHAR_T ])
  else
    enable_dict_canna=no
  fi

  if test "x$enable_dict_canna" = xno && \
     test "x$with_anthy" = xno && \
     test "x$with_anthy_utf8" = xno; then
    AC_MSG_WARN([no dictionary backend is usable.  disabled...])
    enable_dict=no
  fi
fi

AM_CONDITIONAL(DICT, test "x$enable_dict" = xyes)
AM_CONDITIONAL(DICT_CANNA, test "x$enable_dict_canna" = xyes)

if test "x$enable_dict_canna" = xyes; then
  UI_XML_CANNA_START=""
  UI_XML_CANNA_END=""
else
  UI_XML_CANNA_START="<!--"
  UI_XML_CANNA_END="-->"
fi
AC_SUBST(UI_XML_CANNA_START)
AC_SUBST(UI_XML_CANNA_END)

dnl ****************************
dnl *** Tests for EB Library ***
dnl ****************************

AC_ARG_WITH(eb,
  AS_HELP_STRING([--with-eb],
    [build with EB]),
  [],
  [with_eb=no])

if test "x$with_eb" = xyes; then
  eb_LIB_EB4()
  with_eb=$try_eb
fi

if test "x$with_eb" = xyes; then
  AC_DEFINE(HAVE_EBLIB, 1,
    [Define to 1 if you have EB library and header file])
fi

AM_CONDITIONAL(EB, test "x$with_eb" = xyes)

dnl test for OS X
AC_ARG_WITH(osx-dcs,
  AS_HELP_STRING([--with-osx-dcs],
    [build with OS X Dictionary Services]),
  [],
  [with_osx_dcs=no])

if test "x$with_osx_dcs" = xyes; then
  case $host_os in
  darwin*)
    with_osx_dcs=yes
    ;;
  *)
    with_osx_dcs=no
    ;;
  esac
fi

AM_CONDITIONAL(OSX_DCS, test "x$with_osx_dcs" = xyes)

# Check whether user wants libedit support
# This code was based on openssh-4.1p1

AC_ARG_WITH(libedit,
  AC_HELP_STRING([--with-libedit[=DIR]],
    [enable libedit support @<:@default=yes@:>@]),
  [],
  [with_libedit=yes])

if test "x$with_libedit" != xno; then
  if test "x$with_libedit" = xyes; then
    libedit_path=/usr
  else
    with_libedit=yes
    libedit_path="$with_libedit"
  fi

  saved_CPPFLAGS=$CPPFLAGS
  saved_LDFLAGS=$LDFLAGS
  CPPFLAGS="${CPPFLAGS} -I$libedit_path/include"
  LDFLAGS="${LDFLAGS} -L$libedit_path/lib"
  AC_CHECK_LIB(curses, tgetent, LIBEDIT_LIBS="-lcurses",
    [AC_CHECK_LIB(ncurses, tgetent, LIBEDIT_LIBS="-lncurses",
      AC_MSG_WARN("libedit needs libcurses or libncurses. disabled...")
        with_libedit=no)])

  if test "x$with_libedit" != xno; then
      AC_CHECK_LIB(edit, el_init,
        [LIBEDIT_LIBS="-ledit $LIBEDIT_LIBS -L$libedit_path/lib"
          AC_SUBST(LIBEDIT_LIBS)],
        [AC_MSG_WARN("libedit not found. Disabled...")
          with_libedit=no])
  fi
  CPPFLAGS=$saved_CPPFLAGS
  LDFLAGS=$saved_LDFLAGS
fi

AM_CONDITIONAL(LIBEDIT, test "x$with_libedit" != xno)

AM_CONDITIONAL(LIBUIM_X_UTIL, test "x$have_x" = xyes)
AM_CONDITIONAL(NEED_SETENV_C,
              test "x$ac_cv_func_setenv" = xno -o "x$ac_cv_func_unsetenv" = xno)
AM_CONDITIONAL(NEED_STRSEP_C, test $ac_cv_func_strsep = no)
AM_CONDITIONAL(GCC, test "x$ac_cv_c_compiler_gnu" = xyes)
AM_CONDITIONAL(GXX, test "x$ac_cv_cxx_compiler_gnu" = xyes)

IT_PROG_INTLTOOL([0.36.3], [no-xml])

dnl **********************************
dnl *** test for KDE3 applet.. XXX ***
dnl **********************************
AC_ARG_ENABLE(kde-applet,
  AC_HELP_STRING([--disable-kde-applet],
    [do not build uim applet for KDE3 panel]),
  [],
  [enable_kde_applet=yes])

if test "x$enable_kde_applet" != xno; then
  if test "x$with_qt" = xyes; then
    if test "x$KDE_CONFIG" != xno; then
      KDE_PREFIX=`kde-config --prefix`
      KDE_LIB_DIR=`kde-config --expandvars --install lib`
      KDE_DATA_DIR=`kde-config --expandvars --install data`

      saved_cppflags="${CPPFLAGS}"
      CPPFLAGS="-I${KDE_PREFIX}/include/kde ${CPPFLAGS} ${QT_CXXFLAGS}"
      AC_LANG_PUSH(C++)
      AC_CHECK_HEADER([${KDE_PREFIX}/include/kde/kpanelapplet.h],
        [enable_kde_applet=yes KDE_INCLUDE_DIR="${KDE_PREFIX}/include/kde"],
        [enable_kde_applet=no])
      if test "x$enable_kde_applet" = xno; then
        CPPFLAGS="-I${KDE_PREFIX}/include ${saved_cppflags} ${QT_CXXFLAGS}"
        AC_CHECK_HEADER([${KDE_PREFIX}/include/kglobal.h],
          [enable_kde_applet=yes KDE_INCLUDE_DIR="${KDE_PREFIX}/include"],
          [enable_kde_applet=no])
      fi
      CPPFLAGS="${saved_cppflags}"
      AC_LANG_POP(C++)
    else
      enable_kde_applet=no
    fi
  else
    enable_kde_applet=no
  fi
fi

AC_SUBST(KDE_PREFIX)
AC_SUBST(KDE_LIB_DIR)
AC_SUBST(KDE_DATA_DIR)
AC_SUBST(KDE_INCLUDE_DIR)

AM_CONDITIONAL(KDE_APPLET, test "x$enable_kde_applet" = xyes)

dnl ****************************
dnl *** test for KDE4 applet ***
dnl ****************************
AC_ARG_ENABLE(kde4-applet,
  AC_HELP_STRING([--disable-kde4-applet],
    [do not build uim KDE4 Plasma widget]),
  [],
  [enable_kde4_applet=yes])

if test "x$enable_kde4_applet" != xno; then
  if test "x$with_qt4" != xyes; then
    enable_kde4_applet=no
  fi
fi

AC_PATH_PROG(CMAKE, [cmake], [no])
if test "x$enable_kde4_applet" = xyes && test "x$CMAKE" = xno; then
  AC_MSG_WARN([no CMake found])
  enable_kde4_applet=no
fi

AC_PATH_PROG(KDE4_CONFIG, [kde4-config], [no])
if test "x$with_qt4" = xyes || test "x$with_qt5" = xyes; then
  if test "x$KDE4_CONFIG" != xno; then
    KDE4_ICONDIR=`$KDE4_CONFIG --expandvars --install icon`
  fi
  if test -z "$KDE4_ICONDIR"; then
    KDE4_ICONDIR="$prefix/share/icons"
  fi
  AC_DEFINE_UNQUOTED(KDE4_ICONDIR, "$KDE4_ICONDIR", [kde4 icon directory])
fi

if test "x$enable_kde4_applet" = xyes && test "x$KDE4_CONFIG" = xno; then
  AC_MSG_WARN([no kde4-config found])
  enable_kde4_applet=no
fi

AM_CONDITIONAL(KDE4_APPLET, test "x$enable_kde4_applet" = xyes)

AC_ARG_ENABLE(pref,
  AC_HELP_STRING([--disable-pref],
    [do not build graphical utility to edit user settings]),
  [],
  [enable_pref=yes])

if test "x$enable_pref" != xno; then
  if test x"$default_toolkit" = xgtk && \
     test x"$with_gtk2_4" = xyes; then
    enable_pref=yes
  elif test x"$default_toolkit" = xgtk3 && \
       test x"$with_gtk3" = xyes; then
    enable_pref=yes
  elif test x"$default_toolkit" = xqt && \
       test x"$with_qt" = xyes; then
    enable_pref=yes
  elif test x"$default_toolkit" = xqt4 && \
       test x"$with_qt4" = xyes; then
    enable_pref=yes
  elif test x"$default_toolkit" = xqt5 && \
       test x"$with_qt5" = xyes; then
    enable_pref=yes
  else
    enable_pref=no
    AC_MSG_WARN([uim-pref needs GTK+ or Qt toolkit, disabled...])
  fi
fi

AM_CONDITIONAL(PREF, test x"$enable_pref" = xyes)

AC_ARG_ENABLE(anthy-static,
  AC_HELP_STRING([--enable-anthy-static],
    [enable static Anthy]),
  [],
  [enable_anthy_static=no])

AM_CONDITIONAL(ENABLE_ANTHY_STATIC, test "x$enable_anthy_static" = xyes)

AC_ARG_ENABLE(anthy-utf8-static,
  AC_HELP_STRING([--enable-anthy-utf8-static],
    [enable static Anthy-UTF8]),
  [],
  [enable_anthy_utf8_static=no])

AM_CONDITIONAL(ENABLE_ANTHY_UTF8_STATIC,
               test "x$enable_anthy_utf8_static" = xyes)

# ****************************
# *** Tests for uim-notify ***
# ****************************
notify_targets='stderr libnotify knotify3 knotify4 growl'

AC_ARG_ENABLE(notify,
  AS_HELP_STRING([--enable-notify@<:@=agent-list@:>@ (HIGHLY EXPERIMENTAL)],
    [enable system message notification for users
     (specify comma separated notification agent list from
      stderr, libnotify, knotify3, knotify4 and growl)]),
  [],
  [enable_notify=yes])

if test "x$enable_notify" != xno; then
  AC_DEFINE(UIM_USE_NOTIFY_PLUGINS, 1,
            [Define to 1 if you want to use uim-notify plugins])
  use_notify="stderr "

  for i in `echo $enable_notify | ${SED} 's/,/ /g'`; do
    eval "au_enable_$i=yes"
  done
fi

AC_MSG_CHECKING([whether libnotify is requested])
if test "x$au_enable_libnotify" = xyes; then
  AC_MSG_RESULT([yes])
  PKG_CHECK_MODULES(LIBNOTIFY, libnotify >= 0.4,
                    use_libnotify=yes, use_libnotify=no)
  if test "x$use_libnotify" = xyes; then
    AC_DEFINE(ENABLE_LIBNOTIFY, 1, [set notify interface with libnotify])
    AC_SUBST(LIBNOTIFY_LIBS)
    AC_SUBST(LIBNOTIFY_CFLAGS)
    use_notify="${use_notify}libnotify "
  fi
else
  AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING([whether knotify3 is requested])
if test "x$au_enable_knotify3" = xyes; then
  AC_MSG_RESULT([yes])
  if test "x$with_qt4_immodule" = xyes; then
    AC_MSG_WARN([knotify3 could not be used with qt4-immodule enabled])
    use_knotify3=no
  elif test "x$with_qt" = xyes; then
    if test "x$KDE_CONFIG" != xno; then
      KNOTIFY3_PREFIX=`kde-config --prefix`
      KNOTIFY3_LIB_DIR=`kde-config --expandvars --install lib`
      KNOTIFY3_DATA_DIR=`kde-config --expandvars --install data`
      saved_cppflags="${CPPFLAGS}"
      CPPFLAGS="-I${KNOTIFY3_PREFIX}/include/kde ${CPPFLAGS} ${QT_CXXFLAGS}"
      AC_LANG_PUSH(C++)
      AC_CHECK_HEADER([${KNOTIFY3_PREFIX}/include/kde/knotifyclient.h],
                      [use_knotify3=yes KNOTIFY3_INCLUDE_DIR="${KNOTIFY3_PREFIX}/include/kde"], [use_knotify3=no])
      if test "x$use_knotify3" = xno; then
        CPPFLAGS="-I${KNOTIFY3_PREFIX}/include ${saved_cppflags} ${QT_CXXFLAGS}"
        AC_CHECK_HEADER([${KNOTIFY3_PREFIX}/include/knotifyclient.h],
                        [use_knotify3=yes KNOTIFY3_INCLUDE_DIR="${KNOTIFY3_PREFIX}/include"], [use_knotify3=no])
      fi
      CPPFLAGS="${saved_cppflags}"
      AC_LANG_POP(C++)
    else
      use_knotify3=no
    fi
  else
    use_knotify3=no
  fi
else
  AC_MSG_RESULT(no)
fi

if test "x$use_knotify3" = xyes; then
  AC_DEFINE(ENABLE_KNOTIFY3, 1, [set notify interface with knotify3])
  AC_SUBST(KNOTIFY3_PREFIX)
  AC_SUBST(KNOTIFY3_LIB_DIR)
  AC_SUBST(KNOTIFY3_DATA_DIR)
  AC_SUBST(KNOTIFY3_INCLUDE_DIR)
  use_notify="${use_notify}knotify3 "
fi

AC_MSG_CHECKING([whether knotify4 is requested])
if test "x$au_enable_knotify4" = xyes; then
  if test "x$CMAKE" != xno; then
    AC_MSG_RESULT([yes])
    use_knotify4=yes
    use_notify="${use_notify}knotify4 "
  else
    AC_MSG_WARN([no CMake found])
    use_knotify4=no
  fi
else
  AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING([whether growl is requested])
if test "x$au_enable_growl" = xyes; then
  AC_MSG_RESULT([yes])
  use_growl=yes
  use_notify="${use_notify}growl "
else
  AC_MSG_RESULT(no)
fi

if test "x$use_notify" = x; then
  use_notify=none
fi

AM_CONDITIONAL(NOTIFY, test "x$enable_notify" != xno)
AM_CONDITIONAL(LIBNOTIFY, test "x$use_libnotify" = xyes)
AM_CONDITIONAL(KNOTIFY3, test "x$use_knotify3" = xyes)
AM_CONDITIONAL(KNOTIFY4, test "x$use_knotify4" = xyes)
AM_CONDITIONAL(GROWL, test "x$use_growl" = xyes)

AC_DEFINE(UIM_USE_ERROR_GUARD, 1, [Define to 1 if you want to use longjmp-based error handlings])

#
# Compiler options
#

# Debugging-related flags
if test "x$enable_debug" = xyes; then
  # add -g flag to CFLAGS regardless of GCC=yes or not
  if test "x$ac_cv_prog_cc_g" = xyes; then
    CFLAGS="$CFLAGS -g"
    CXXFLAGS="$CXXFLAGS -g"
  fi

  # Restricting the C dialect tends to cause system library disfunction such as
  # on glibc and Darwin libc. So this option is only specified at debugging
  # mode.
  #
  # -std=gnu89 is required only for (old?) glibc. -std=c89 is sufficient for
  # other environments.
  AC_LANG_WERROR  # Turn warning-only unknown options into error.
  if test "x$ax_cv_lib_glibc" = xyes; then
    AX_CFLAGS_GCC_OPTION([-std=gnu89])
  else
    AX_CFLAGS_GCC_OPTION([-std=c89])
  fi
  AX_LANG_WNOERROR  # end AC_LANG_WERROR

  QT_CONFIG_OPTS="debug"
else
  CFLAGS="$CFLAGS -DNDEBUG"
  CXXFLAGS="$CXXFLAGS -DNDEBUG"

  # Suppress the warnings of combined-mode SigScheme
  AC_LANG_WERROR  # Turn warning-only unknown options into error.
  AX_CFLAGS_GCC_OPTION([-Wno-redundant-decls], [UIM_SCM_CFLAGS])
  AX_CFLAGS_GCC_OPTION([-Wno-unused-function], [UIM_SCM_CFLAGS])
  AX_LANG_WNOERROR  # end AC_LANG_WERROR

  QT_CONFIG_OPTS="release"
fi

AC_ARG_ENABLE(warnings-into-error,
  AC_HELP_STRING([--enable-warnings-into-error],
    [treat compiler warnings as error @<:@default=no if !debug@:>@]),
    [
        case $enable_warnings_into_error in
	no)
	    ;;
	yes|*)
	    AX_CFLAGS_GCC_OPTION([-Werror])
	    AX_CXXFLAGS_GCC_OPTION([-Werror])
	    ;;
	esac
    ], [
	    # Enable -Werror by default if --enable-debug. But since
	    # the C89 string length limit always cause the warnings, I
	    # disable it for uim-scm.c.  -- YamaKen 2007-07-19
	    if test "x$enable_debug" = xyes; then
	      AX_CFLAGS_GCC_OPTION([-Werror])
	      AX_CXXFLAGS_GCC_OPTION([-Werror])
  	      AX_CFLAGS_GCC_OPTION([-Wno-error], [UIM_SCM_CFLAGS])
	    fi
    ])
AC_SUBST(UIM_SCM_CFLAGS)
AC_SUBST(QT_CONFIG_OPTS)

dnl *************************
dnl *** Tests for gosh and GaUnit ***
dnl *************************
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_MSG_CHECKING([whether to have gosh and GaUnit])
if test -n "$GOSH" && test -n "$GAUCHE_CONFIG"; then
  gauche_version=`$GAUCHE_CONFIG -V`
  gauche_major_version=`echo $gauche_version | \
          sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
  gauche_minor_version=`echo $gauche_version | \
          sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
  gauche_micro_version=`echo $gauche_version | \
          sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
  dnl require 0.8.13
  if test "$gauche_major_version" -gt 0 || \
     test "$gauche_major_version" -eq 0 -a "$gauche_minor_version" -gt 8 || \
     test "$gauche_major_version" -eq 0 -a "$gauche_minor_version" -eq 8 -a \
          "$gauche_micro_version" -gt 13; then
    echo '(use test.unit)' | $GOSH -b
    gosh_result=$?
    if test "$gosh_result" = 0; then
      gaunit_version=`echo '(use test.unit) (print *gaunit-version*)' | $GOSH -b`
      gosh_result=$?
      if test "$gosh_result" = 0; then
        gaunit_major_version=`echo $gaunit_version | \
            sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
        gaunit_minor_version=`echo $gaunit_version | \
            sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
        gaunit_micro_version=`echo $gaunit_version | \
            sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
        dnl require 0.1.6
        if test "$gaunit_major_version" -gt 0 || \
           test "$gaunit_major_version" -eq 0 -a \
                "$gaunit_minor_version" -gt 6 || \
           test "$gaunit_major_version" -eq 0 -a \
                "$gaunit_minor_version" -eq 1 -a \
                "$gaunit_micro_version" -ge 6; then
          have_gosh_and_gaunit=yes
        fi
      fi
    fi
  fi
fi
if test "x$have_gosh_and_gaunit" = xyes; then
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
AM_CONDITIONAL(DO_CHECK_IN_TEST,
               test "x$have_gosh_and_gaunit" = xyes -a "x$enable_debug" = xyes)

# Checks for system services

AH_BOTTOM([/* Include os specific header. */
#include "os_dep.h"])

# Add include path
INCLUDES='-I$(top_srcdir)/replace -I$(top_srcdir)/uim'
AC_SUBST(INCLUDES)

SRCDIR=$srcdir
AC_SUBST(SRCDIR)
AC_SUBST(DESTDIR)

AC_SUBST(abs_srcdir)
AC_SUBST(abs_builddir)
AC_SUBST(abs_top_srcdir)
AC_SUBST(abs_top_builddir)

# configure sigscheme/ subdir
#
# --with-master-pkg=uim instructs that pkgdatadir for Scheme libraries.
# See also sigscheme/lib/Makefile.am
ac_configure_args="$ac_configure_args --with-master-pkg=uim --enable-conf=uim --disable-libsscm --disable-shell"
AC_CONFIG_SUBDIRS([sigscheme])

AC_CONFIG_FILES([Makefile
                 po/Makefile.in
                 m4/Makefile
                 doc/Makefile
                 uim/Makefile
                 uim/version.h
                 scm/Makefile
                 gtk2/Makefile
                 gtk2/candwin/Makefile
                 gtk2/dict/Makefile
                 gtk2/dict/uim-dict-ui.xml
                 gtk2/immodule/Makefile
                 gtk2/pad/Makefile
                 gtk2/pref/Makefile
                 gtk2/switcher/Makefile
                 gtk2/test/Makefile
                 gtk2/test/test.sh
                 gtk2/toolbar/Makefile
                 gtk3/Makefile
                 gtk3/candwin/Makefile
                 gtk3/dict/Makefile
                 gtk3/immodule/Makefile
                 gtk3/pad/Makefile
                 gtk3/pref/Makefile
                 gtk3/switcher/Makefile
                 gtk3/test/Makefile
                 gtk3/toolbar/Makefile
                 qt3/Makefile
                 qt3/candwin/Makefile
                 qt3/chardict/Makefile
                 qt3/immodule/Makefile
                 qt3/pref/Makefile
                 qt3/switcher/Makefile
                 qt3/test/Makefile
                 qt3/toolbar/Makefile
                 qt3/toolbar/uimapplet.desktop
                 qt4/Makefile
                 qt4/common.pro
                 qt4/candwin/Makefile
                 qt4/candwin/uim-candwin-qt4.pro
                 qt4/chardict/Makefile
                 qt4/chardict/uim-chardict-qt4.pro
                 qt4/edittest/Makefile
                 qt4/immodule/quiminputcontextplugin.pro
                 qt4/immodule/Makefile
                 qt4/pref/uim-pref-qt4.pro
                 qt4/pref/Makefile
                 qt4/switcher/uim-im-switcher-qt4.pro
                 qt4/switcher/Makefile
                 qt4/toolbar/plasma-applet-uim.desktop
                 qt4/toolbar/uim-toolbar-qt4.pro
                 qt4/toolbar/Makefile
                 qt5/Makefile
                 qt5/candwin/Makefile
                 qt5/candwin/uim-candwin-qt5.pro
                 qt5/chardict/Makefile
                 qt5/chardict/uim-chardict-qt5.pro
                 qt5/edittest/Makefile
                 qt5/immodule/quimplatforminputcontextplugin.pro
                 qt5/immodule/Makefile
                 qt5/pref/uim-pref-qt5.pro
                 qt5/pref/Makefile
                 qt5/switcher/uim-im-switcher-qt5.pro
                 qt5/switcher/Makefile
                 qt5/toolbar/uim-toolbar-qt5.pro
                 qt5/toolbar/Makefile
                 xim/Makefile
                 fep/Makefile
                 emacs/Makefile
                 emacs/uim-version.el
                 tables/Makefile
                 byeoru-data/Makefile
                 test/Makefile
                 test2/Makefile
                 test2/run-singletest.sh
                 examples/Makefile
                 examples/uim-custom/Makefile
                 pixmaps/Makefile
                 pixmaps/uim-m17nlib-relink-icons
                 replace/Makefile
                 notify/Makefile
                 uim.pc
                 uim.desktop.in
])

AC_OUTPUT

if test "x$CMAKE" != xno; then
  if test "x$enable_debug" = xyes; then
    cmake_build_type=Debug
  else
    cmake_build_type=Release
  fi
  # FIXME: move CMAKE_CXX_FLAGS into cmake_option
  cmake_option="-DCMAKE_BUILD_TYPE=${cmake_build_type}"
  cmake_option="${cmake_option} -DCMAKE_INSTALL_PREFIX=${prefix}"
  cmake_option="${cmake_option} -DCMAKE_SKIP_RPATH=true"
  cmake_option="${cmake_option} -DKDE4_USE_ALWAYS_FULL_RPATH=false"
fi

if test "x$with_qt4" = xyes; then
  if test "x$with_qt4_immodule" = xyes; then
    qt4_immodule_option="DEFINES+=QT4_IMMODULE"
  fi
  # Generate Makefiles for Qt4 by qmake
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/candwin/Makefile.qmake \
    ${ac_abs_top_builddir}/qt4/candwin/uim-candwin-qt4.pro
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/chardict/Makefile.qmake \
    ${ac_abs_top_builddir}/qt4/chardict/uim-chardict-qt4.pro
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/pref/Makefile.qmake \
    ${qt4_immodule_option} \
    ${ac_abs_top_builddir}/qt4/pref/uim-pref-qt4.pro
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/switcher/Makefile.qmake \
    ${ac_abs_top_builddir}/qt4/switcher/uim-im-switcher-qt4.pro
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/toolbar/Makefile.qmake \
    ${ac_abs_top_builddir}/qt4/toolbar/uim-toolbar-qt4.pro

  # Generate a Makefile for KDE4 applet by cmake
  if test "x$enable_kde4_applet" = xyes; then
    mkdir -p ${ac_abs_top_builddir}/qt4/toolbar/build
    cd ${ac_abs_top_builddir}/qt4/toolbar/build
    # FIXME: move CMAKE_CXX_FLAGS into cmake_option
    ${CMAKE} -DCMAKE_CXX_FLAGS="${CXXFLAGS} ${X_CFLAGS}" \
      ${cmake_option} ${ac_abs_top_srcdir}/qt4/toolbar
    cd -
  fi
fi

if test "x$with_qt4_immodule" = xyes; then
  if test "x$enable_qt4_qt3support" = xyes; then
    qt4_qt3support_option="QT+=qt3support DEFINES+=ENABLE_QT4_QT3SUPPORT"
  fi
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/immodule/Makefile.qmake \
    ${qt4_qt3support_option} \
    ${ac_abs_top_builddir}/qt4/immodule/quiminputcontextplugin.pro
  ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/edittest/Makefile.qmake \
    ${ac_abs_top_srcdir}/qt4/edittest/edittest.pro
fi

if test "x$use_knotify4" = xyes; then
  # Generate a Makefile for KNotify plugin by cmake
  mkdir -p ${ac_abs_top_builddir}/notify/build
  cd ${ac_abs_top_builddir}/notify/build
  # FIXME: move CMAKE_CXX_FLAGS into cmake_option
  ${CMAKE} -DCMAKE_CXX_FLAGS="${CXXFLAGS} ${X_CFLAGS}" \
    -DUIMNOTIFY_PLUGINDIR="${LIBDIR}/${PACKAGE}"/notify \
    ${cmake_option} ${ac_abs_top_srcdir}/notify
  cd -
fi

# Generate Makefiles for Qt5 by qmake
if test "x$with_qt5" = xyes; then
  if test "x$with_qt5_immodule" = xyes; then
    qt5_immodule_option="DEFINES+=QT5_IMMODULE"
  fi
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/candwin/Makefile.qmake \
    ${ac_abs_top_builddir}/qt5/candwin/uim-candwin-qt5.pro
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/chardict/Makefile.qmake \
    ${ac_abs_top_builddir}/qt5/chardict/uim-chardict-qt5.pro
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/pref/Makefile.qmake \
    ${qt5_immodule_option} \
    ${ac_abs_top_builddir}/qt5/pref/uim-pref-qt5.pro
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/switcher/Makefile.qmake \
    ${ac_abs_top_builddir}/qt5/switcher/uim-im-switcher-qt5.pro
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/toolbar/Makefile.qmake \
    ${ac_abs_top_builddir}/qt5/toolbar/uim-toolbar-qt5.pro
fi

if test "x$with_qt5_immodule" = xyes; then
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/immodule/Makefile.qmake \
    ${ac_abs_top_builddir}/qt5/immodule/quimplatforminputcontextplugin.pro
  ${QMAKE5} -o ${ac_abs_top_builddir}/qt5/edittest/Makefile.qmake \
    ${ac_abs_top_srcdir}/qt4/edittest/edittest.pro
fi

AC_MSG_RESULT([
Configure Result :

   Anthy           : ${with_anthy}
   Anthy (UTF-8)   : ${with_anthy_utf8}
   Canna           : ${with_canna}
   Mana            : ${with_mana}
   PRIME           : ${with_prime}
   SJ3             : ${with_sj3}
   SKK             : ${with_skk}
   Wnn             : ${with_wnn}
   m17n-lib        : ${with_m17nlib}
   cURL            : ${with_curl}
   expat           : ${expat_found}
   OpenSSL         : ${enable_openssl}
   SQLite3         : ${with_sqlite3}
   ffi             : ${with_ffi}
   GTK+2           : ${with_gtk2}
   GNOME2 Applet   : ${enable_gnome_applet}
   GTK+3           : ${with_gtk3}
   GNOME3 Applet   : ${enable_gnome3_applet}
   Qt3             : ${with_qt}
   Qt3 immodule    : ${with_qt_immodule}
   Qt4             : ${with_qt4}
   Qt4 immodule    : ${with_qt4_immodule}
   Qt4 Qt3Support  : ${enable_qt4_qt3support}
   Qt5             : ${with_qt5}
   Qt5 immodule    : ${with_qt5_immodule}
   KDE3 Applet     : ${enable_kde_applet}
   KDE4 Applet     : ${enable_kde4_applet}
   FEP             : ${enable_fep}
   Emacs           : ${enable_emacs}
   XIM             : ${use_xim}
   Pref            : ${enable_pref}
   DICT            : ${enable_dict}
   EB              : ${with_eb}
   OSX dictionary  : ${with_osx_dcs}
   libedit         : ${with_libedit}
   notify          : ${use_notify}
   Default toolkit : ${default_toolkit}
])

if test "x$enable_debug" = xyes; then
AC_MSG_RESULT([
Configure Result for developers:

   DEBUG           : ${enable_debug}
])
fi
