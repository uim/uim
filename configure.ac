AC_PREREQ(2.60b)
AC_INIT([uim], [1.6.0], [uim-en@googlegroups.com])
AC_CONFIG_SRCDIR([uim/uim.c])
AC_CONFIG_HEADERS([uim/config.h])
AM_INIT_AUTOMAKE([1.10 dist-bzip2])

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
AC_PATH_PROG(RSVG, rsvg)
AC_PATH_PROGS(MD5, md5 md5sum)
AC_PATH_PROGS(SHA1, sha1 sha1sum)
AC_PATH_PROGS(SED, sed gsed)
AX_PATH_QMAKE4

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

use_m17nlib="no"
AC_ARG_WITH(m17nlib,
  AS_HELP_STRING([--without-m17nlib],
                 [Don't build against m17nlib]),
  [
  if test "x$with_m17nlib" = "xyes"; then
    PKG_CHECK_MODULES(M17NLIB, m17n-shell >= 1.3.1, use_m17nlib="yes",use_m17nlib="no")
    AC_CHECK_PROG(M17NDB, m17n-db, m17n-db)
    if test -n "$M17NDB"; then
      m17n_db_dir=`$M17NDB`
    fi
  fi
  ],
  [
    PKG_CHECK_MODULES(M17NLIB, m17n-shell >= 1.3.1, use_m17nlib="yes",use_m17nlib="no")
    AC_CHECK_PROG(M17NDB, m17n-db, m17n-db)
    if test -n "$M17NDB"; then
      m17n_db_dir=`$M17NDB`
    fi
])

AC_SUBST(M17NLIB_LIBS)
AC_SUBST(m17n_db_dir)


AC_ARG_WITH(scim,
  AC_HELP_STRING([--with-scim],
                 [build against SCIM (broken)
		  @<:@default=no@:>@]),
  [
    case $with_scim in
      no)
        use_scim="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(SCIM, scim >= 1.3.0, use_scim="yes",use_scim="no")
      ;;
    esac
  ],
  [use_scim="no"] )

# ***********************
# *** Tests for Anthy ***
# ***********************
AC_ARG_WITH(anthy,
  AC_HELP_STRING([--without-anthy], [Don't build with libanthy]),
  [
    if test "x$with_anthy" = "xyes"; then
      AC_CHECK_HEADERS([anthy/anthy.h],
        [
          saved_LIBS=$LIBS
          LIBS="$LIBS -lanthy -lanthydic"
          AC_CHECK_LIB([anthy], [anthy_init], [use_anthy="yes"], [use_anthy="no"])
          LIBS=$saved_LIBS
        ],
        [use_anthy="no"])
    else
      use_anthy="no"
    fi
  ],
  [
    AC_CHECK_HEADERS([anthy/anthy.h],
     [
       saved_LIBS=$LIBS
       LIBS="$LIBS -lanthy -lanthydic"
       AC_CHECK_LIB([anthy], [anthy_init], [use_anthy="yes"], [use_anthy="no"])
       LIBS=$saved_LIBS
     ],
     [use_anthy="no"])
  ])

if test "x$use_anthy" = "xyes"; then
  ANTHY_LIBS="-lanthy -lanthydic"
  ANTHY_CFLAGS=""
  PKG_CHECK_EXISTS(anthy >= 8622,
		   [AC_DEFINE(LIBANTHY_UTF8_CAPABLE, 1,
		    "libanthy can handle UTF-8")])
fi
AC_SUBST(ANTHY_LIBS)
AC_SUBST(ANTHY_CFLAGS)

# *******************************
# *** Tests for Anthy (UTF-8) ***
# *******************************
AC_ARG_WITH(anthy-utf8,
  AC_HELP_STRING([--with-anthy-utf8],
		 [build with libanthy (UTF-8)
		 @<:@default=no@:>@]),
  [
    case $with_anthy_utf8 in
      no)
        use_anthy_utf8="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(ANTHY_UTF8, anthy >= 8622,
			  [use_anthy_utf8="yes"
			   AC_DEFINE(LIBANTHY_UTF8_CAPABLE, 1,
				     [libanthy can handle UTF-8])],
			   use_anthy_utf8="no")
      ;;
    esac
  ],
  [use_anthy_utf8="no"])

# ***********************
# *** Tests for Canna ***
# ***********************
AC_ARG_WITH(canna,
	AS_HELP_STRING([--with-canna], [Build with libcanna
					@<:@default=no@:>@]),
	[
	if test "x$with_canna" = "xyes"; then
		AC_CHECK_HEADERS([canna/RK.h],
    		 [AC_CHECK_LIB(canna, RkInitialize, use_canna=yes, use_canna=no)
		 ], [use_canna="no"], [ #undef _WCHAR_T ])
	else
		use_canna="no"
	fi
	],[
	use_canna="no"
])

# *********************
# *** Tests for Wnn ***
# *********************
AC_ARG_WITH(wnn,
	AS_HELP_STRING([--with-wnn], [Build with libwnn
					@<:@default=no@:>@]),
	[
	if test "x$with_wnn" = "xyes"; then
	   	AM_PATH_WNN(use_wnn=yes, use_wnn=no)
	fi
	],[
	use_wnn="no"
])

# *********************
# *** Tests for SJ3 ***
# *********************
AC_ARG_WITH(sj3,
	AS_HELP_STRING([--with-sj3], [Build with sj3lib
					    @<:@default=no@:>@]),
	[
	if test "x$with_sj3" = "xyes"; then
	        PKG_CHECK_MODULES(SJ3, sj3lib >= 2.0.1.21, use_sj3="yes",use_sj3="no")
	else
		use_sj3="no"
	fi
	],[
	        use_sj3="no"
])

# ***********************
# *** Tests for Mana ***
# ***********************
AC_ARG_WITH(mana,
	AS_HELP_STRING([--with-mana], [Build a plugin for Mana
				       @<:@default=yes@:>@]),
	[
	if test "x$with_mana" = "xyes"; then
	        AC_CHECK_PROG(MANA, mana, mana)
		if test -n "$MANA"; then
			use_mana="yes"
		else
			use_mana="no"
		fi
	else
		use_mana="no"
	fi
	],[
	        AC_CHECK_PROG(MANA, mana, mana)
		if test -n "$MANA"; then
			use_mana="yes"
		else
			use_mana="no"
		fi
])

# ***********************
# *** Tests for PRIME ***
# ***********************
AC_ARG_WITH(prime,
	AS_HELP_STRING([--with-prime], [Build a plugin for PRIME
					@<:@default=yes@:>@]),
	[
	if test "x$with_prime" = "xyes"; then
	        PKG_CHECK_MODULES(PRIME, prime >= 0.8.5.2, use_prime="yes",use_prime="no")
	else
		use_prime="no"
	fi
	],[
	        PKG_CHECK_MODULES(PRIME, prime >= 0.8.5.2, use_prime="yes",use_prime="no")
])

# ***********************
# *** Tests for cURL ***
# ***********************
AC_ARG_WITH(curl,
	AS_HELP_STRING([--with-curl], [Build with libcurl
					@<:@default=no@:>@]),
	[
	if test "x$with_curl" = "xyes"; then
		PKG_CHECK_MODULES(CURL, libcurl >= 7.16.4, use_curl="yes", use_curl="no")
	else
		use_curl="no"
	fi
	],[
	use_curl="no"
])

# ***********************
# *** Tests for expat ***
# ***********************
expat_found=no
AM_WITH_EXPAT

# ***********************
# *** Tests for OpenSSL ***
# ***********************
AC_ARG_ENABLE(openssl,
  AC_HELP_STRING([--enable-openssl],
                 [enable openssl @<:@default=no@:>@]),
  [
    use_openssl="yes"
    AM_WITH_OPENSSL
  ],
  [
    use_openssl="no"
  ])


# *************************
# *** Tests for SQLite3 ***
# *************************
AC_ARG_WITH(sqlite3,
	AS_HELP_STRING([--with-sqlite3], [Build with SQLite3
					@<:@default=no@:>@]),
	[
	if test "x$with_sqlite3" = "xyes"; then
		PKG_CHECK_MODULES(SQLITE3, sqlite3 >= 3.0.0, use_sqlite3="yes", use_sqlite3="no")
	else
		use_sqlite3="no"
	fi
	],[
	use_sqlite3="no"
])

PKG_CHECK_MODULES(X11, x11, x11_use_new_dir="yes", x11_use_new_dir="no")
AC_PATH_XTRA

use_xim="no"
if test x"$have_x" != "xdisabled" && test x"$have_x" != "xno"; then
   use_xim="yes"
   AC_CXX_NAMESPACES
   AC_CXX_HAVE_STL
   if test $ac_cv_cxx_have_stl = no; then
     use_xim="no"
   fi
   AC_MSG_CHECKING([whether to have Xft support])
   AM_PATH_XFT(yes, XFT=true, XFT=false)
   if test x"$XFT" = "xtrue" ; then
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
AM_CONDITIONAL(WITH_XFT, test x"$XFT" = "xtrue")

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
if test "x$ac_cv_func_snprintf" = "xyes" ; then
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
if test "x$ac_cv_func_asprintf" != "xyes" && \
   test "x$ac_cv_func_vsnprintf" = "xyes" ; then
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
		[ ac_cv_have_struct_sockaddr_storage="yes" ],
		[ ac_cv_have_struct_sockaddr_storage="no" ]
	)
])
if test "x$ac_cv_have_struct_sockaddr_storage" = "xyes" ; then
	AC_DEFINE(HAVE_STRUCT_SOCKADDR_STORAGE, 1,
		[define if you have struct sockaddr_storage data type])
fi

AC_CACHE_CHECK([for struct sockaddr_in6], ac_cv_have_struct_sockaddr_in6, [
	AC_TRY_COMPILE(
		[
#include <sys/types.h>
#include <netinet/in.h>
		],
		[ struct sockaddr_in6 s; s.sin6_family = 0; ],
		[ ac_cv_have_struct_sockaddr_in6="yes" ],
		[ ac_cv_have_struct_sockaddr_in6="no" ]
	)
])
if test "x$ac_cv_have_struct_sockaddr_in6" = "xyes" ; then
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
		[ ac_cv_have_struct_in6_addr="yes" ],
		[ ac_cv_have_struct_in6_addr="no" ]
	)
])
if test "x$ac_cv_have_struct_in6_addr" = "xyes" ; then
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
		[ ac_cv_have_struct_addrinfo="yes" ],
		[ ac_cv_have_struct_addrinfo="no" ]
	)
])
if test "x$ac_cv_have_struct_addrinfo" = "xyes" ; then
	AC_DEFINE(HAVE_STRUCT_ADDRINFO, 1,
		[define if you have struct addrinfo data type])
fi


GETTEXT_PACKAGE=uim
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE",[gettext package name])

AM_GNU_GETTEXT([external], [need-ngettext])
AM_GNU_GETTEXT_VERSION([0.17])

# FIXME: Probably voilent way  -- YamaKen 2005-01-07
if test x$prefix = xNONE; then
   prefix=$ac_default_prefix
fi
if test x$exec_prefix = xNONE; then
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
# we should use more safe macro such as AC_EXPAND_DIR
LOCALEDIR=`eval eval echo ${datadir}/locale`
# backward compatibility
localedir=$LOCALEDIR
AC_DEFINE_UNQUOTED(LOCALEDIR, "$LOCALEDIR", [locale dir])
AC_DEFINE_UNQUOTED(GNOMELOCALEDIR, "$LOCALEDIR", [locale dir for gnome])
# define XLIB directory for Compose file
if test "x$x11_use_new_dir" = "xyes"; then
  AC_DEFINE_UNQUOTED(XLIB_DIR, "`$PKG_CONFIG x11 --variable=prefix`/share", [X11 Library Directory])
else
  AC_DEFINE_UNQUOTED(XLIB_DIR, "$x_libraries", [X11 Library Directory])
fi
uim_pixmapsdir=`eval eval echo ${datadir}/${PACKAGE_TARNAME}/pixmaps`
AC_DEFINE_UNQUOTED(UIM_PIXMAPSDIR, "${uim_pixmapsdir}", [pixmaps directory])
AC_SUBST(uim_pixmapsdir)

dnl *****************************
dnl *** Check for Gtk Library ***
dnl *****************************
AC_ARG_WITH(gtk2,
  AC_HELP_STRING([--without-gtk2],
                 [don't build against Gtk+2]),
  [
    case $with_gtk2 in
      no)
        use_gtk2="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.2.0, use_gtk2="yes",use_gtk2="no")
        PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0, use_gtk2_4="yes",use_gtk2_4="no")
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.2.0, use_gtk2="yes",use_gtk2="no")
    PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0, use_gtk2_4="yes",use_gtk2_4="no") ])

AC_ARG_ENABLE(gnome-applet,
  AC_HELP_STRING([--disable-gnome-applet],
                 [disable uim applet for Gnome panel]),
  [
    case $enable_gnome_applet in
      no)
        use_applet_gnome="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(APPLET_GNOME, libpanelapplet-2.0 libgnomeui-2.0, use_applet_gnome="yes",use_applet_gnome="no")
        PKG_CHECK_MODULES(APPLET_2_14, libpanelapplet-2.0 >= 2.14.0, panelapplet_have_set_background_api="yes",panelapplet_have_set_background_api="no")
      ;;
    esac
  ],
  [
    PKG_CHECK_MODULES(APPLET_GNOME, libpanelapplet-2.0 libgnomeui-2.0, use_applet_gnome="yes",use_applet_gnome="no")
    PKG_CHECK_MODULES(APPLET_2_14, libpanelapplet-2.0 >= 2.14.0, panelapplet_have_set_background_api="yes",panelapplet_have_set_background_api="no")
  ])

if test x"$panelapplet_have_set_background_api" = xyes; then
  AC_DEFINE(LIBPANEL_APPLET_HAVE_SET_BACKGROUND_WIDGET, 1 , [libpanel-applet have set_background api])
fi

dnl ****************************
dnl *** Check for Qt Library ***
dnl ****************************
AC_ARG_WITH(qt,
  AC_HELP_STRING([--with-qt],
                 [build qt helper applications
		  @<:@default=no@:>@]),
  [
    case $with_qt in
      no)
        use_qt="no"
      ;;
      yes|*)
        use_qt="yes"
      ;;
    esac
  ],
  [
    use_qt="no"
  ])

AC_ARG_WITH(qt-immodule,
  AC_HELP_STRING([--with-qt-immodule],
                 [Build qt-immodule extension
		  @<:@default=no@:>@]),
  [
    case $with_qtimmodule in
      no)
        use_qtimmodule="no"
      ;;
      yes|*)
        use_qtimmodule="yes"
      ;;
    esac

  ],
  [
    use_qtimmodule="no"
  ])

dnl *****************************
dnl *** Check for Qt4 Library ***
dnl *****************************
AC_ARG_WITH(qt4,
  AC_HELP_STRING([--with-qt4],
                 [build qt4 helper applications (experimental)
		  @<:@default=no@:>@]),
  [
    case $with_qt4 in
      no)
        use_qt4="no"
      ;;
      yes|*)
        use_qt4="yes"
      ;;
    esac
  ],
  [
    use_qt4="no"
  ])


AC_ARG_WITH(qt4-immodule,
  AC_HELP_STRING([--with-qt4-immodule],
                 [Build qt4-immodule extension (experimental)
		  @<:@default=no@:>@]),
  [
    case $with_qt4_immodule in
      no)
        use_qt4_immodule="no"
      ;;
      yes|*)
        use_qt4_immodule="yes"
      ;;
    esac

  ],
  [
    use_qt4_immodule="no"
  ])
if test x$use_qt4 = xyes || test x$use_qt4_immodule = xyes; then
    if test -z "$QMAKE4"; then
        AC_MSG_ERROR([no qmake for Qt4 found])
    fi
fi

default_toolkit="gtk"
AC_ARG_ENABLE(default-toolkit,
  AC_HELP_STRING([--enable-default-toolkit],
                 [Determine default toolkit (gtk, qt, or qt4)
		  @<:@default=gtk@:>@]),
  [
  if test x"$enable_default_toolkit" = "xgtk" && test x"$use_gtk2" = "xyes"; then
     default_toolkit="gtk"
  fi
  if test x"$enable_default_toolkit" = "xqt" && test x"$use_qt" = "xyes"; then
     default_toolkit="qt"
  fi
  if test x"$enable_default_toolkit" = "xqt4" && test x"$use_qt4" = "xyes"; then
     default_toolkit="qt4"
  fi
  ],
  [])

AC_ARG_ENABLE(debug,
  AC_HELP_STRING([--enable-debug],
                 [enable debugging @<:@default=no@:>@]),
  [],
  [
    enable_debug="no"
  ])

AC_ARG_ENABLE(fep,
  AC_HELP_STRING([--disable-fep],
                 [disable uim-fep]),
  [
    case $enable_fep in
      no)
        use_uim_fep="no"
      ;;
      yes|*)
        use_uim_fep="yes"
	AC_CHECK_LIB(curses, setupterm, FEP_LIBADD="-lcurses $FEP_LIBADD",
	  [AC_CHECK_LIB(ncurses, setupterm, FEP_LIBADD="-lncurses $FEP_LIBADD",
	    AC_MSG_WARN([fep needs setupterm in libcurses or libncurses. disabled...]);use_uim_fep="no")])
	AC_CHECK_FUNCS(forkpty)
	if test $ac_cv_func_forkpty = no; then
	  AC_CHECK_LIB(util, forkpty, [AC_DEFINE(HAVE_FORKPTY) FEP_LIBADD="-lutil $FEP_LIBADD"])
	fi
	AC_SUBST(FEP_LIBADD)
      ;;
    esac
  ],
  [ use_uim_fep="yes"
    AC_CHECK_LIB(curses, setupterm, FEP_LIBADD="-lcurses $FEP_LIBADD",
      [AC_CHECK_LIB(ncurses, setupterm, FEP_LIBADD="-lncurses $FEP_LIBADD",
        AC_MSG_WARN([fep needs setupterm in libcurses or libncurses. disabled...]);use_uim_fep="no")])
    AC_CHECK_FUNCS(forkpty)
    if test $ac_cv_func_forkpty = no; then
      AC_CHECK_LIB(util, forkpty, [AC_DEFINE(HAVE_FORKPTY) FEP_LIBADD="-lutil $FEP_LIBADD"])
    fi
    AC_SUBST(FEP_LIBADD)
  ])

AC_ARG_ENABLE(emacs,
  AC_HELP_STRING([--disable-emacs],
                 [disable uim.el]),
  [
    case $enable_emacs in
      no)
        use_uim_el="no"
      ;;
      yes|*)
        use_uim_el="yes"
        AM_PATH_LISPDIR
        AC_SUBST(UIMEL_LISP_DIR, $lispdir/uim-el)
      ;;
    esac
  ],
  [ use_uim_el="yes"
    AM_PATH_LISPDIR
    AC_SUBST(UIMEL_LISP_DIR, $lispdir/uim-el)
  ])

AC_ARG_ENABLE(dict,
  AC_HELP_STRING([--enable-dict],
                 [build dictionary utility for uim (Anthy and Canna)
		  @<:@default=no@:>@]),
  [
    case $enable_dict in
      no)
        use_dict="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(DICT, gtk+-2.0 >= 2.4.0,
                           use_dict="yes",
                          AC_MSG_WARN([dict needs Gtk+ 2.4 or higher. dict disabled...]);use_dict="no")
      ;;
    esac
  ],
  [ use_dict="no" ])

dnl ****************************
dnl *** Tests for EB Library ***
dnl ****************************

AC_ARG_WITH(eb,
  AS_HELP_STRING([--with-eb],
                 [Build with EB @<:@default=no@:>@]),
  [
  if test "x$with_eb" = "xyes"; then
    eb_LIB_EB4()
    use_eb="$try_eb"
  else
    use_eb="no"
  fi
  ],
  [
  use_eb="no"
])
if test "x$use_eb" = "xyes"; then
  AC_DEFINE(HAVE_EBLIB, 1, [Define to 1 if you have EB library and header file])
fi

# Check whether user wants libedit support
# This code was based on openssh-4.1p1

AC_ARG_WITH(libedit,
AC_HELP_STRING([--with-libedit[=DIR], Enable libedit support
				      @<:@default=yes@:>@]),
  [
   if test "x$with_libedit" != "xno"; then
        use_libedit="yes"
	libedit_path="$withval"
   else
        use_libedit="no"
   fi
  ],
  [use_libedit="yes"])

 if test "x$use_libedit" != "xno"; then
     saved_CPPFLAGS=$CPPFLAGS
     saved_LDFLAGS=$LDFLAGS
     CPPFLAGS="${CPPFLAGS} -I$libedit_path/include"
     LDFLAGS="${LDFLAGS} -L$libedit_path/lib"
     AC_CHECK_LIB(edit, el_init,	
     		[
		   LIBEDIT_LIBS="-ledit -lcurses -L$libedit_path/lib"
		   AC_SUBST(LIBEDIT_LIBS)
		], [ 
                   AC_MSG_WARN("libedit not found. Disabled...")
                   use_libedit="no"
                ])
     CPPFLAGS=$saved_CPPFLAGS
     LDFLAGS=$saved_LDFLAGS
fi

dnl for uim-dict.xml.in
if test "x$use_anthy" = "xyes" || test "x$use_anthy_utf8" = "xyes"; then
  UI_XML_ANTHY_START=""
  UI_XML_ANTHY_END=""
else
  UI_XML_ANTHY_START="<!--"
  UI_XML_ANTHY_END="-->"
fi
if test "x$use_canna" = "xyes"; then
  UI_XML_CANNA_START=""
  UI_XML_CANNA_END=""
else
  UI_XML_CANNA_START="<!--"
  UI_XML_CANNA_END="-->"
fi
AC_SUBST(UI_XML_ANTHY_START)
AC_SUBST(UI_XML_ANTHY_END)
AC_SUBST(UI_XML_CANNA_START)
AC_SUBST(UI_XML_CANNA_END)


AM_CONDITIONAL(M17NLIB, test x$use_m17nlib = xyes)
AM_CONDITIONAL(SCIM, test x$use_scim = xyes)
AM_CONDITIONAL(ANTHY, test x$use_anthy = xyes)
AM_CONDITIONAL(ANTHY_UTF8, test x$use_anthy_utf8 = xyes)
AM_CONDITIONAL(CANNA, test x$use_canna = xyes)
AM_CONDITIONAL(WNN, test x$use_wnn = xyes)
AM_CONDITIONAL(SJ3, test x$use_sj3 = xyes)
AM_CONDITIONAL(MANA, test x$use_mana = xyes)
AM_CONDITIONAL(PRIME, test x$use_prime = xyes)
AM_CONDITIONAL(SKK, true)
AM_CONDITIONAL(CURL, test x$use_curl = xyes)
AM_CONDITIONAL(EXPAT, test x$expat_found = xyes)
AM_CONDITIONAL(OPENSSL, test x$use_openssl = xyes)
AM_CONDITIONAL(SQLITE3, test x$use_sqlite3 = xyes)
AM_CONDITIONAL(GTK2, test x$use_gtk2 = xyes)
AM_CONDITIONAL(GTK2_4, test x$use_gtk2_4 = xyes)
AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK, test x$default_toolkit = xgtk)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT,  test x$default_toolkit = xqt)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT4, test x$default_toolkit = xqt4)
AM_CONDITIONAL(APPLET_GNOME, test x$use_applet_gnome = xyes)
AM_CONDITIONAL(UIM_FEP, test x$use_uim_fep = xyes)
AM_CONDITIONAL(UIM_EL, test x$use_uim_el = xyes)
AM_CONDITIONAL(XIM, test x$use_xim = xyes)
AM_CONDITIONAL(DICT, test x$use_dict = xyes)
AM_CONDITIONAL(EB, test x$use_eb = xyes)
AM_CONDITIONAL(LIBEDIT, test x$use_libedit = xyes)
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)
AM_CONDITIONAL(LIBUIM_X_UTIL, test x$have_x = xyes)
AM_CONDITIONAL(NEED_SETENV_C, test $ac_cv_func_setenv = no -o $ac_cv_func_unsetenv = no)
AM_CONDITIONAL(NEED_STRSEP_C, test $ac_cv_func_strsep = no)
AM_CONDITIONAL(GCC, test x$ac_cv_c_compiler_gnu = xyes)
AM_CONDITIONAL(GXX, test x$ac_cv_cxx_compiler_gnu = xyes)

# To get qt/chardict/po working, 0.35.2 is required. 0.35.[01] has a hard-coded
# $(top_builddir) bug.  -- YamaKen 2006-12-28
#
# 0.36.3 or later is required to resolve a 'make check' problem ([uim-ja 87]).
IT_PROG_INTLTOOL([0.36.3], [no-xml])
IT_PO_SUBDIR([qt/chardict/po])
IT_PO_SUBDIR([qt4/chardict/po])


GTK_BINARY_VERSION=`$PKG_CONFIG gtk+-2.0 --variable=gtk_binary_version`
GTK_LIBDIR=`$PKG_CONFIG gtk+-2.0 --variable=libdir`
AC_SUBST(GTK_BINARY_VERSION)
AC_SUBST(GTK_LIBDIR)

if test x$use_gtk2 = xyes ; then
AC_DEFINE(USE_GTK2, 1, [use gtk2])
fi

if test x$use_qt = xyes ; then
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
    QTLIBDIR="$QTDIR/lib"
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
    ],result="yes",result="no")

    AC_MSG_RESULT("$result")
    if test "$result" = "no"; then
       use_qt="no"
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

AC_PATH_PROGS(KDE_CONFIG, [kde-config kde4-config], [no])
if test "x$use_qt" = "xyes" || test "x$use_qt4" = "xyes"; then
    if test "x$KDE_CONFIG" != "xno"; then
	KDE_ICONDIR=`$KDE_CONFIG --expandvars --install icon`
    fi
    if test -z "$KDE_ICONDIR"; then
      KDE_ICONDIR="$prefix/share/icons"
    fi
    AC_DEFINE_UNQUOTED(KDE_ICONDIR, "$KDE_ICONDIR", [kde icon directory])
fi

dnl **********************************
dnl *** test for KDE3 applet.. XXX ***
dnl **********************************
AC_ARG_ENABLE(kde-applet,
  AC_HELP_STRING([--enable-kde-applet],
  		 [build uim applet for KDE panel
		  @<:@default=yes@:>@]),
		 enable_kde_applet=$enableval,
		 enable_kde_applet=yes)
case "$enable_kde_applet" in
  no)
    use_applet_kde="no"
    ;;
  yes|*)
    if test "x$use_qt" = "xyes"; then
      if test "x$KDE_CONFIG" != "xno"; then
	KDE_PREFIX=`kde-config --prefix`
	KDE_LIB_DIR=`kde-config --expandvars --install lib`
	KDE_DATA_DIR=`kde-config --expandvars --install data`
	saved_cppflags="${CPPFLAGS}"
	CPPFLAGS="-I${KDE_PREFIX}/include/kde ${CPPFLAGS} ${QT_CXXFLAGS}"
	AC_LANG_PUSH(C++)
	AC_CHECK_HEADER([${KDE_PREFIX}/include/kde/kpanelapplet.h],
			[use_applet_kde="yes" KDE_INCLUDE_DIR="${KDE_PREFIX}/include/kde"], [use_applet_kde="no"])
	if test "x$use_applet_kde" = "xno"; then
	  CPPFLAGS="-I${KDE_PREFIX}/include ${saved_cppflags} ${QT_CXXFLAGS}"
	  AC_CHECK_HEADER([${KDE_PREFIX}/include/kglobal.h],
	  		  [use_applet_kde="yes" KDE_INCLUDE_DIR="${KDE_PREFIX}/include"], [use_applet_kde="no"])
	fi
	CPPFLAGS="${saved_cppflags}"
	AC_LANG_POP(C++)
      else
	use_applet_kde="no"
      fi
    else
      use_applet_kde="no"
    fi
    ;;
esac
AC_SUBST(KDE_PREFIX)
AC_SUBST(KDE_LIB_DIR)
AC_SUBST(KDE_DATA_DIR)
AC_SUBST(KDE_INCLUDE_DIR)

if test x$use_qtimmodule = xyes ; then
    AC_MSG_CHECKING(for qt-immodule patch)
    # Check for immodule for Qt patch
    if test ! -f $QTINCDIR/qinputcontext.h  || test ! -f $QTINCDIR/qinputcontextplugin.h; then
      AC_MSG_RESULT(no)
      use_qtimmodule="cannot"
    else
      AC_MSG_RESULT(yes)
    fi
fi


AM_CONDITIONAL(QT, test x$use_qt = xyes)
AM_CONDITIONAL(QT_IMMODULE, test x$use_qtimmodule = xyes)
AM_CONDITIONAL(APPLET_KDE, test x$use_applet_kde = xyes)
AM_CONDITIONAL(QT4, test x$use_qt4 = xyes)
AM_CONDITIONAL(QT4_IMMODULE, test x$use_qt4_immodule = xyes)

AC_ARG_ENABLE(pref,
  AC_HELP_STRING([--enable-pref],
		 [build graphical utility to edit user settings
		  @<:@default=yes@:>@]),
[
    case $enable_pref in
      no)
        use_pref="no"
	;;
      yes|*)
	if test x"$default_toolkit" = "xgtk" && \
	   test x"$use_gtk2_4" = "xyes"; then
	  use_pref="yes"
	elif test x"$default_toolkit" = "xqt" && \
	     test x"$use_qt" = "xyes"; then
	  use_pref="yes"
	elif test x"$default_toolkit" = "xqt4" && \
	     test x"$use_qt4" = "xyes"; then
	  use_pref="yes"
	else
	  use_pref="no"
	  AC_MSG_WARN([uim-pref needs Gtk+ or Qt toolkit, disabled...])
	fi
	;;
    esac ],
  [
    if test x"$default_toolkit" = "xgtk" && test x"$use_gtk2_4" = "xyes"; then
      use_pref="yes"
    else
      if test x"$default_toolkit" = "xqt" && \
         test x"$use_qt" = "xyes"; then
	use_pref="yes"
      else
	use_pref="no"
	AC_MSG_WARN([uim pref needs Gtk+ or Qt toolkit, disabled...])
      fi
    fi
  ])
AM_CONDITIONAL(PREF, test x"$use_pref" = "xyes")

AC_ARG_ENABLE(anthy-static,
    AC_HELP_STRING([--enable-anthy-static],
		   [@<:@default=no@:>@]),
    [
        case $enable_anthy_static in
	yes)
	    build_anthy_static="yes"
	    ;;
	no|*)
	    build_anthy_static="no"
	    ;;
	esac
    ], [
	    build_anthy_static="no"
    ])

AM_CONDITIONAL(ENABLE_ANTHY_STATIC, test x$build_anthy_static = xyes)

AC_ARG_ENABLE(anthy-utf8-static,
    AC_HELP_STRING([--enable-anthy-utf8-static],
		   [@<:@default=no@:>@]),
    [
        case $enable_anthy_utf8_static in
	yes)
	    build_anthy_utf8_static="yes"
	    ;;
	no|*)
	    build_anthy_utf8_static="no"
	    ;;
	esac
    ], [
	    build_anthy_utf8_static="no"
    ])

AM_CONDITIONAL(ENABLE_ANTHY_UTF8_STATIC, test x$build_anthy_utf8_static = xyes)

# ****************************
# *** Tests for uim-notify ***
# ****************************
notify_targets='stderr libnotify knotify3 growl'

AC_ARG_ENABLE(notify,
  AS_HELP_STRING([--enable-notify@<:@=agent-list@:>@ (HIGHLY EXPERIMENTAL)],
                 [enable system message notification for users
		  (specify comma separated notification agent list
		  from stderr, libnotify, knotify3 and growl)]),
  [ enable_notify=$enableval
    have_notify_opt=yes ],
  [ enable_notify=yes
    have_notify_opt=no ])

if test "x$enable_notify" != xno; then
  AC_DEFINE(UIM_USE_NOTIFY, 1, [Define to 1 if you want to use uim-notify])
  use_notify="${use_notify}stderr "

  for i in `echo $enable_notify | ${SED} 's/,/ /g'`; do
    eval "au_enable_$i=yes"
  done
fi

AC_MSG_CHECKING([whether libnotify is requested])
if test "x$au_enable_libnotify" = xyes; then
  AC_MSG_RESULT([yes])
  PKG_CHECK_MODULES(LIBNOTIFY, libnotify >= 0.4, use_libnotify="yes",
					       use_libnotify="no")
  if test "x$use_libnotify" = "xyes"; then
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
  if test "x$use_qt4_immodule" = "xyes"; then
    AC_MSG_WARN([knotify3 couldn't be used with qt4-immodule enabled])
    use_knotify3="no"
  elif test "x$use_qt" = "xyes"; then
    if test "x$KDE_CONFIG" != "xno"; then
      KNOTIFY3_PREFIX=`kde-config --prefix`
      KNOTIFY3_LIB_DIR=`kde-config --expandvars --install lib`
      KNOTIFY3_DATA_DIR=`kde-config --expandvars --install data`
      saved_cppflags="${CPPFLAGS}"
      CPPFLAGS="-I${KNOTIFY3_PREFIX}/include/kde ${CPPFLAGS} ${QT_CXXFLAGS}"
      AC_LANG_PUSH(C++)
      AC_CHECK_HEADER([${KNOTIFY3_PREFIX}/include/kde/knotifyclient.h],
                      [use_knotify3="yes" KNOTIFY3_INCLUDE_DIR="${KNOTIFY3_PREFIX}/include/kde"], [use_knotify3="no"])
      if test "x$use_knotify3" = "xno"; then
        CPPFLAGS="-I${KNOTIFY3_PREFIX}/include ${saved_cppflags} ${QT_CXXFLAGS}"
        AC_CHECK_HEADER([${KNOTIFY3_PREFIX}/include/knotifyclient.h],
                        [use_knotify3="yes" KNOTIFY3_INCLUDE_DIR="${KNOTIFY3_PREFIX}/include"], [use_knotify3="no"])
      fi
      CPPFLAGS="${saved_cppflags}"
      AC_LANG_POP(C++)
    else
      use_knotify3="no"
    fi
  else
    use_knotify3="no"
  fi
else
  AC_MSG_RESULT(no)
fi
if test "x$use_knotify3" = "xyes"; then
  AC_DEFINE(ENABLE_KNOTIFY3, 1, [set notify interface with knotify3])
  AC_SUBST(KNOTIFY3_PREFIX)
  AC_SUBST(KNOTIFY3_LIB_DIR)
  AC_SUBST(KNOTIFY3_DATA_DIR)
  AC_SUBST(KNOTIFY3_INCLUDE_DIR)
  use_notify="${use_notify}knotify3 "
fi

AC_MSG_CHECKING([whether growl is requested])
if test "x$au_enable_growl" = xyes; then
  AC_MSG_RESULT([yes])
  use_growl="yes"
  use_notify="${use_notify}growl "
else
  AC_MSG_RESULT(no)
fi

if test "x$use_notify" = "x"; then
  use_notify=none
fi

AM_CONDITIONAL(NOTIFY, test "x$enable_notify" != xno)
AM_CONDITIONAL(LIBNOTIFY, test x$use_libnotify = xyes)
AM_CONDITIONAL(KNOTIFY3, test x$use_knotify3 = xyes)
AM_CONDITIONAL(GROWL, test x$use_growl = xyes)


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

  # Suppress the warnings of libuim-scim
  AC_LANG_WERROR  # Turn warning-only unknown options into error.
  AX_CFLAGS_GCC_OPTION([-Wno-unused-function],  [SCIM_CFLAGS])
  AX_CFLAGS_GCC_OPTION([-Wno-unused-parameter], [SCIM_CFLAGS])
  AX_CFLAGS_GCC_OPTION([-Wno-unused-variable],  [SCIM_CFLAGS])
  AX_LANG_WNOERROR  # end AC_LANG_WERROR

  QT_CONFIG_OPTS="release"
fi

AC_ARG_ENABLE(warnings-into-error,
    AC_HELP_STRING([--enable-warnings-into-error],
		   [Treat compiler warnings as error @<:@default=no if !debug@:>@]),
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
	    if test x$enable_debug = xyes; then
	      AX_CFLAGS_GCC_OPTION([-Werror])
	      AX_CXXFLAGS_GCC_OPTION([-Werror])
  	      AX_CFLAGS_GCC_OPTION([-Wno-error], [UIM_SCM_CFLAGS])
  	      AX_CFLAGS_GCC_OPTION([-Wno-error], [SCIM_CFLAGS])
	    fi
    ])
AC_SUBST(UIM_SCM_CFLAGS)
AC_SUBST(SCIM_CFLAGS)
AC_SUBST(QT_CONFIG_OPTS)


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
		 helper/Makefile
		 helper/uim-dict-ui.xml
		 doc/Makefile
		 uim/Makefile
		 scm/Makefile
		 gtk/Makefile
		 gtk/test/Makefile
		 gtk/test/test.sh
		 qt/Makefile
		 qt/chardict/Makefile
		 qt/chardict/po/Makefile.in
		 qt/test/Makefile
		 qt/uimapplet.desktop
		 qt4/Makefile
		 qt4/candwin/Makefile
		 qt4/candwin/uim-candwin-qt4.pro
		 qt4/chardict/Makefile
		 qt4/chardict/uim-chardict-qt4.pro
		 qt4/chardict/po/Makefile.in
		 qt4/edittest/Makefile
		 qt4/immodule/quiminputcontextplugin.pro
		 qt4/immodule/Makefile
		 qt4/pref/uim-pref-qt4.pro
		 qt4/pref/Makefile
		 qt4/switcher/uim-im-switcher-qt4.pro
		 qt4/switcher/Makefile
		 qt4/toolbar/uim-toolbar-qt4.pro
		 qt4/toolbar/Makefile
		 xim/Makefile
		 fep/Makefile
		 emacs/Makefile
		 emacs/uim-version.el
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
		 uim.spec
		 ])

AC_OUTPUT

# Generate Makefiles for Qt4 by qmake
if test x$use_qt4 = xyes; then
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/candwin/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/candwin/uim-candwin-qt4.pro
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/chardict/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/chardict/uim-chardict-qt4.pro
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/pref/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/pref/uim-pref-qt4.pro
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/switcher/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/switcher/uim-im-switcher-qt4.pro
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/toolbar/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/toolbar/uim-toolbar-qt4.pro
fi
if test x$use_qt4_immodule = xyes; then
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/immodule/Makefile.qmake \
		 ${ac_abs_top_srcdir}/qt4/immodule/quiminputcontextplugin.pro
    ${QMAKE4} -o ${ac_abs_top_builddir}/qt4/edittest/Makefile.qmake \
                 ${ac_abs_top_srcdir}/qt4/edittest/edittest.pro
fi

AC_MSG_RESULT([
Configure Result :

   Anthy           : ${use_anthy}
   Anthy (UTF-8)   : ${use_anthy_utf8}
   Canna           : ${use_canna}
   Wnn             : ${use_wnn}
   SJ3             : ${use_sj3}
   Mana            : ${use_mana}
   PRIME           : ${use_prime}
   m17n-lib        : ${use_m17nlib}
   SCIM            : ${use_scim}
   cURL            : ${use_curl}
   expat           : ${expat_found}
   OpenSSL         : ${use_openssl}
   SQLite3         : ${use_sqlite3}
   Gtk+            : ${use_gtk2}
   Gnome Applet    : ${use_applet_gnome}
   Qt3             : ${use_qt}
   Qt3 immodule    : ${use_qtimmodule}
   Qt4             : ${use_qt4}
   Qt4 immodule    : ${use_qt4_immodule}
   KDE3 Applet     : ${use_applet_kde}
   FEP             : ${use_uim_fep}
   Emacs           : ${use_uim_el}
   XIM             : ${use_xim}
   Pref            : ${use_pref}
   DICT            : ${use_dict}
   EB              : ${use_eb}
   libedit         : ${use_libedit}
   notify          : ${use_notify}
   Default toolkit : ${default_toolkit}
])

if test x$enable_debug = xyes; then
AC_MSG_RESULT([
Configure Result for developers:

   DEBUG           : ${enable_debug}
])
fi
