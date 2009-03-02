# openssl.m4
# Configure paths for libwnn

dnl AM_PATH_OPENSSL([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl Test for OpenSSL, and define OPENSSL_CPPFLAGS and OEPNSSL_LIBS
dnl
AC_DEFUN([AM_WITH_OPENSSL],
[dnl
dnl Get the cflags and libraries
dnl

saved_CPPFLAGS="$CPPFLAGS"
saved_LDFLAGS="$LDFLAGS"
saved_saved_LIBS="$LIBS"
AC_ARG_WITH(openssl-dir,
	[  --with-openssl-dir=PATH     Specify path to OpenSSL installation ],
	[
		if test "x$withval" != "xno" ; then
			case "$withval" in
				# Relative paths
				./*|../*)	withval="`pwd`/$withval"
			esac
			if test -d "$withval/lib"; then
				if test -n "${need_dash_r}"; then
					LDFLAGS="-L${withval}/lib -R${withval}/lib ${LDFLAGS}"
				else
					LDFLAGS="-L${withval}/lib ${LDFLAGS}"
				fi
			else
				if test -n "${need_dash_r}"; then
					LDFLAGS="-L${withval} -R${withval} ${LDFLAGS}"
				else
					LDFLAGS="-L${withval} ${LDFLAGS}"
				fi
			fi
			if test -d "$withval/include"; then
				CPPFLAGS="-I${withval}/include ${CPPFLAGS}"
			else
				CPPFLAGS="-I${withval} ${CPPFLAGS}"
			fi
		fi
	]
	[use_openssl="no"]
)

LIBS="-lcrypto $LIBS"
AC_CHECK_LIB(ssl, SSL_CTX_new,
	[
		LIBS="-lssl $LIBS"
	], [
	AC_MSG_WARN("libssl not found. OpenSSL Disabled...")
		use_openssl="no"
	])

AC_TRY_LINK_FUNC(RAND_add, AC_DEFINE(HAVE_OPENSSL, 1,
	[Define if your ssl headers are included
	with #include <openssl/header.h>]),
	[
		dnl Check default openssl install dir
		if test -n "${need_dash_r}"; then
			LDFLAGS="-L/usr/local/ssl/lib -R/usr/local/ssl/lib ${saved_LDFLAGS}"
		else
			LDFLAGS="-L/usr/local/ssl/lib ${saved_LDFLAGS}"
		fi
		CPPFLAGS="-I/usr/local/ssl/include ${saved_CPPFLAGS}"
		AC_TRY_LINK_FUNC(RAND_add, AC_DEFINE(HAVE_OPENSSL),
			[
				AC_MSG_WARN([*** Can't find recent OpenSSL libcrypto (see config.log for details) ***])
				use_openssl="no"
			]
		)
	]
)

# Determine OpenSSL header version
AC_MSG_CHECKING([OpenSSL header version])
AC_RUN_IFELSE(
	[AC_LANG_SOURCE([[
#include <stdio.h>
#include <string.h>
#include <openssl/opensslv.h>
#define DATA "conftest.sslincver"
int main(void) {
	FILE *fd;
	int rc;

	fd = fopen(DATA,"w");
	if(fd == NULL)
		exit(1);

	if ((rc = fprintf(fd ,"%x (%s)\n", OPENSSL_VERSION_NUMBER, OPENSSL_VERSION_TEXT)) <0)
		exit(1);

	exit(0);
}
	]])],
	[
		ssl_header_ver=`cat conftest.sslincver`
		AC_MSG_RESULT($ssl_header_ver)
	],
	[
		AC_MSG_RESULT(not found)
		AC_MSG_WARN(OpenSSL version header not found.)
		use_openssl="no"
	],
	[
		AC_MSG_WARN([cross compiling: not checking])
	]
)

# Determine OpenSSL library version
AC_MSG_CHECKING([OpenSSL library version])
AC_RUN_IFELSE(
	[AC_LANG_SOURCE([[
#include <stdio.h>
#include <string.h>
#include <openssl/opensslv.h>
#include <openssl/crypto.h>
#define DATA "conftest.ssllibver"
int main(void) {
	FILE *fd;
	int rc;

	fd = fopen(DATA,"w");
	if(fd == NULL)
		exit(1);

	if ((rc = fprintf(fd ,"%x (%s)\n", SSLeay(), SSLeay_version(SSLEAY_VERSION))) <0)
		exit(1);

	exit(0);
}
	]])],
	[
		ssl_library_ver=`cat conftest.ssllibver`
		AC_MSG_RESULT($ssl_library_ver)
	],
	[
		AC_MSG_RESULT(not found)
		AC_MSG_WARN(OpenSSL library not found.)
		use_openssl="no"
	],
	[
		AC_MSG_WARN([cross compiling: not checking])
	]
)

AC_ARG_WITH(openssl-header-check,
	[  --without-openssl-header-check Disable OpenSSL version consistency check],
	[  if test "x$withval" = "xno" ; then
		openssl_check_nonfatal=1
	   fi
	]
)

# Sanity check OpenSSL headers
AC_MSG_CHECKING([whether OpenSSL's headers match the library])
AC_RUN_IFELSE(
	[AC_LANG_SOURCE([[
#include <string.h>
#include <openssl/opensslv.h>
int main(void) { exit(SSLeay() == OPENSSL_VERSION_NUMBER ? 0 : 1); }
	]])],
	[
		AC_MSG_RESULT(yes)
	],
	[
		AC_MSG_RESULT(no)
		if test "x$openssl_check_nonfatal" = "x"; then
			AC_MSG_WARN([Your OpenSSL headers do not match your
library. Check config.log for details.
If you are sure your installation is consistent, you can disable the check
by running "./configure --without-openssl-header-check".
Also see contrib/findssl.sh for help identifying header/library mismatches.
])
			use_openssl="no"
		else
			AC_MSG_WARN([Your OpenSSL headers do not match your
library. Check config.log for details.
Also see contrib/findssl.sh for help identifying header/library mismatches.])
			use_openssl="no"
		fi
	],
	[
		AC_MSG_WARN([cross compiling: not checking])
	]
)

AC_MSG_CHECKING([if programs using OpenSSL functions will link])
AC_LINK_IFELSE(
	[AC_LANG_SOURCE([[
#include <openssl/evp.h>
int main(void) { SSLeay_add_all_algorithms(); }
	]])],
	[
		AC_MSG_RESULT(yes)
	],
	[
		AC_MSG_RESULT(no)
		saved_LIBS="$LIBS"
		LIBS="$LIBS -ldl"
		AC_MSG_CHECKING([if programs using OpenSSL need -ldl])
		AC_LINK_IFELSE(
			[AC_LANG_SOURCE([[
#include <openssl/evp.h>
int main(void) { SSLeay_add_all_algorithms(); }
			]])],
			[
				AC_MSG_RESULT(yes)
			],
			[
				AC_MSG_RESULT(no)
				LIBS="$saved_LIBS"
			]
		)
	]
)

AC_ARG_WITH(ssl-engine,
	[  --with-ssl-engine       Enable OpenSSL (hardware) ENGINE support ],
	[ if test "x$withval" != "xno" ; then
		AC_MSG_CHECKING(for OpenSSL ENGINE support)
		AC_TRY_COMPILE(
			[ #include <openssl/engine.h>],
			[
ENGINE_load_builtin_engines();ENGINE_register_all_complete();
			],
			[ AC_MSG_RESULT(yes)
			  AC_DEFINE(USE_OPENSSL_ENGINE, 1,
			     [Enable OpenSSL engine support])
			],
			[ AC_MSG_WARN(OpenSSL ENGINE support not found)]
		)
	  fi ]
)

OPENSSL_CPPFLAGS="$CPPFLAGS"
OPENSSL_LIBS="$LDFLAGS $LIBS"

CPPFLAGS="$saved_CPPFLAGS"
LDFLAGS="$saved_LDFLAGS"
LIBS="$saved_saved_LIBS"

AC_SUBST(OPENSSL_CPPFLAGS)
AC_SUBST(OPENSSL_LIBS)
])
