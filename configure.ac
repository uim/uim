AC_PREREQ(2.57)

AC_INIT([uim], 0.4.5, [tkng@xem.jp])
AM_INIT_AUTOMAKE($PACKAGE_NAME, $PACKAGE_VERSION)

AC_CONFIG_SRCDIR([COPYING])

AM_CONFIG_HEADER([uim/config.h])

# Set empty values to cancel default "-g -O2"
CFLAGS=""
CXXFLAGS=""

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
AC_PROG_INTLTOOL

dnl ***********************
dnl *** Tests for iconv ***
dnl ***********************
dnl
dnl We do this before the gettext checks, to avoid distortion

AM_ICONV

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
    AC_CHECK_LIB(m17n, m17n_init, HAVE_M17NLIB=yes, HAVE_M17NLIB=no)
    if test "x$HAVE_M17NLIB" = "xyes"; then
      AC_CHECK_HEADER(m17n.h, HAVE_M17NLIB=yes, HAVE_M17NLIB=no)
      if test "x$HAVE_M17NLIB" = "xyes"; then
        AC_DEFINE(HAVE_M17NLIB, 1,
           [Define to 1 if you have m17nlib library and header file])
        M17NLIB_LIBS="-lm17n"
        use_m17nlib="yes"
      fi
    fi
  fi
  ],
  [
  AC_CHECK_LIB(m17n, m17n_init, HAVE_M17NLIB=yes, HAVE_M17NLIB=no)
  if test "x$HAVE_M17NLIB" = "xyes"; then
    AC_CHECK_HEADER(m17n.h, HAVE_M17NLIB=yes, HAVE_M17NLIB=no)
    if test "x$HAVE_M17NLIB" = "xyes"; then
      AC_DEFINE(HAVE_M17NLIB, 1,
         [Define to 1 if you have m17nlib library and header file])
      M17NLIB_LIBS="-lm17n"
      use_m17nlib="yes"
    fi
  fi
])

AC_SUBST(M17NLIB_LIBS)


AC_ARG_WITH(scim,
  AC_HELP_STRING([--with-scim],
                 [build against SCIM (experimental)]),
  [
    case $with_scim in
      no)
        use_scim="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(SCIM, scim >= 1.0.0, use_scim="yes",use_scim="no")
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(SCIM, scim >= 1.0.0, use_scim="yes",use_scim="no") ])

# *************************
# *** Tests for Canna ***
# *************************
AC_ARG_WITH(canna,
	AS_HELP_STRING([--with-canna], [Build with libcanna]),
	[
	if test "x$with_canna" = "xyes"; then
		AC_CHECK_HEADERS([canna/RK.h], [use_canna="yes"],
				 [use_canna="no"], [ #undef _WCHAR_T ])
	else
		use_canna="no"
	fi
	],[
	use_canna="no"
])

# Checks for header files.
AC_PATH_XTRA
AC_FUNC_ALLOCA
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h locale.h stdlib.h unistd.h])
AC_CHECK_HEADERS([locale.h stdlib.h string.h sys/ioctl.h sys/socket.h termios.h unistd.h wchar.h])
AC_CHECK_HEADERS([sys/time.h sys/stat.h sys/un.h getopt.h assert.h signal.h term.h ctype.h pwd.h stdarg.h fcntl.h])
AC_CHECK_HEADERS([pty.h util.h libutil.h])
AC_CHECK_HEADERS([curses.h stropts.h])
AC_CHECK_HEADERS([sys/param.h strings.h])

# Checks for library
AC_CHECK_LIB(dl,dlopen)
AC_CHECK_LIB(socket,socket)
AC_CHECK_LIB([curses], [setupterm])
AC_CHECK_LIB([ncurses], [setupterm])
AC_CHECK_LIB([util], [forkpty])

# Checks for function
AC_CHECK_FUNCS([forkpty])
AC_CHECK_FUNCS([cfmakeraw])
AC_CHECK_FUNCS([wcswidth])
AC_CHECK_FUNCS(getpeereid asprintf vasprintf setenv unsetenv strsep)
AC_CHECK_FUNCS(getpid stat mkdir chmod)
AC_SEARCH_LIBS(dlfunc,dl,
  [
    AC_DEFINE(HAVE_DLFUNC, 1,
              [Define to 1 if you have the `dlfunc' function.])
    
  ])

GETTEXT_PACKAGE=uim
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE",[gettext package name])

AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION(0.12)


# XXX:Ugly localedir definition... need more beautiful way.
if test x$prefix = xNONE; then
   # I can't imagine when prefix does not exist -- YamaKen 2004-09-30
   localedir="/usr/local/share/locale"
else
   #localedir="${prefix}/share/locale"

   # I believe that locale data must be installed into $datadir rather
   # than its default value $prefix/share -- YamaKen 2004-09-30
   localedir="${datadir}/locale"
fi
# we should use more safe macro such as AC_EXPAND_DIR
LOCALEDIR=`eval echo ${datadir}/locale`
AC_SUBST(localedir)
AC_DEFINE_UNQUOTED(LOCALEDIR, "$LOCALEDIR", [locale dir])
AC_DEFINE_UNQUOTED(GNOMELOCALEDIR, "$LOCALEDIR", [locale dir for gnome])

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
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.2.0, use_gtk2="yes",use_gtk2="no") ])

AC_ARG_WITH(gnome2,
  AC_HELP_STRING([--without-gnome2],
                 [don't build against Gnome2 (experimental)]),
  [
    case $with_gnome2 in
      no)
        use_gnome2="no"
      ;;
      yes|*)
        if test x$use_gtk2 = xyes ; then
          PKG_CHECK_MODULES(GNOME2, libgnome-2.0, use_gnome2="yes",use_gnome2="no")
        else
          use_gnome2="no"
        fi
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(GNOME2, libgnome-2.0, use_gnome2="yes",use_gnome2="no") ])

AC_ARG_ENABLE(applet,
  AC_HELP_STRING([--disable-applet],
                 [disable uim_helper_applet for Gnome (experimental)]),
  [
    case $enable_applet in
      no)
        use_applet="no"
      ;;
      yes|*)
        if test x$use_gnome2 = xyes ; then
          PKG_CHECK_MODULES(APPLET, libpanelapplet-2.0, use_applet="yes",use_applet="no")
        else
          use_applet="no"
        fi
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(APPLET, libpanelapplet-2.0, use_applet="yes",use_applet="no") ])

AC_ARG_ENABLE(debug,
  AC_HELP_STRING([--enable-debug],
                 [enable debugging]))

AC_ARG_ENABLE(compat-scm,
  AC_HELP_STRING([--disable-compat-scm],
                 [disable obsolete part of uim-scm API (experimental)]),
  [
    if test "$enable_compat_scm" != "no" ;then
      enable_compat_scm="yes"
    fi
  ],
  [
    enable_compat_scm="yes"
  ])

AC_ARG_ENABLE(compat-custom,
  AC_HELP_STRING([--enable-compat-custom],
                 [enable obsolete version of 'custom API' required by sumika. This exclusively disables new custom API]))

AC_ARG_ENABLE(scm-nested-eval,
  AC_HELP_STRING([--enable-scm-nested-eval],
                 [enable nested Scheme evaluation from C (i.e. C -> Scheme -> C -> Scheme) (for developer, experimental)]))

AC_ARG_ENABLE(callback-queue,
  AC_HELP_STRING([--disable-callback-queue],
                 [disable callback queue (for developer, experimental)]),
  [
    if test "$enable_callback_queue" != "no" ;then
      enable_callback_queue="yes"
    fi
  ],
  [
    enable_callback_queue="yes"
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
      ;;
    esac
  ],
  [ use_uim_fep="yes" ])

AC_ARG_ENABLE(sumika,
  AC_HELP_STRING([--enable-sumika],
                 [enable sumika (a dictionary utility for uim, experimental)]),
  [
    case $enable_sumika in
      no)
        use_sumika="no"
      ;;
      yes|*)
        if test "x$enable_compat_scm" != "xyes" \
                -o "x$enable_compat_custom" != "xyes" ; then
          AC_MSG_ERROR([Sumika requires --enable-compat-scm and --enable-compat-custom])
        fi
        PKG_CHECK_MODULES(SUMIKA, gtk+-2.0 >= 2.4.0,
                           use_sumika="yes",
                          AC_MSG_WARN([sumika needs Gtk+ 2.4 or higher. sumika disabled...]);use_sumika="no")
      ;;
    esac
  ],
  [ use_sumika="no" ])

AC_ARG_ENABLE(dict,
  AC_HELP_STRING([--enable-dict],
                 [enable uim-dict (a dictionary utility for uim, broken)]),
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


AM_CONDITIONAL(M17NLIB, test x$use_m17nlib = xyes)
AM_CONDITIONAL(SCIM, test x$use_scim = xyes)
AM_CONDITIONAL(CANNA, test x$use_canna = xyes)
AM_CONDITIONAL(GTK2, test x$use_gtk2 = xyes)
AM_CONDITIONAL(GNOME2, test x$use_gnome2 = xyes)
AM_CONDITIONAL(APPLET, test x$use_applet = xyes)
AM_CONDITIONAL(UIM_FEP, test x$use_uim_fep = xyes)
AM_CONDITIONAL(SUMIKA, test x$use_sumika = xyes)
AM_CONDITIONAL(DICT, test x$use_dict = xyes)
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)
AM_CONDITIONAL(COMPAT_SCM, test x$enable_compat_scm = xyes)
AM_CONDITIONAL(COMPAT_CUSTOM, test x$enable_compat_custom = xyes)
AM_CONDITIONAL(SCM_NESTED_EVAL, test x$enable_scm_nested_eval = xyes)
AM_CONDITIONAL(CALLBACK_QUEUE, test x$enable_callback_queue = xyes)

GTK_BINARY_VERSION=`pkg-config gtk+-2.0 --variable=gtk_binary_version`
AC_SUBST(GTK_BINARY_VERSION)

if test x$use_gtk2 = xyes ; then
AC_DEFINE(USE_GTK2, 1, [use gtk2])
fi

#if test x$use_gnome2 = xyes ; then
#AC_DEFINE(USE_GNOME2, 1, [use gnome2])
#fi

# add debugging flags to CFLAGS regardless of GCC=yes or not
if test x$enable_debug = xyes; then
	CFLAGS="$CFLAGS -g"
	CXXFLAGS="$CXXFLAGS -g"
else
	CFLAGS="$CFLAGS -O2"
	CXXFLAGS="$CXXFLAGS -O2"
fi

# add warning flags to CFLAGS if GCC=yes
if test x$CC = xgcc; then
	CFLAGS="$CFLAGS -Wall -std=gnu89 -pedantic -Wchar-subscripts -Wmissing-declarations -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare"
	CXXFLAGS="$CXXFLAGS -Wall"
fi

if test x$enable_compat_scm = xyes; then
  AC_DEFINE(UIM_COMPAT_SCM, 1,
            [Enable obsolete part of uim-scm API])
fi

if test x$enable_compat_custom = xyes; then
  AC_DEFINE(UIM_COMPAT_CUSTOM, 1,
            [Enable obsolete version of 'custom API' required by sumika])
fi

if test x$enable_scm_nested_eval = xyes; then
  AC_DEFINE(UIM_SCM_NESTED_EVAL, 1,
            [Enable nested Scheme evaluation from C])
  AC_DEFINE(NESTED_REPL_C_STRING, 1,
            [Enable nested Scheme evaluation from C])
fi

if test x$enable_callback_queue = xyes; then
  AC_DEFINE(UIM_CALLBACK_QUEUE, 1,
            [Enable callback queue])
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_FUNC_SELECT_ARGTYPES
AC_TYPE_SIGNAL

SRCDIR=$srcdir
AC_SUBST(SRCDIR)

PKG_CHECK_MODULES(GLIB, glib-2.0,,)

AC_CONFIG_FILES([test.sh
                 Makefile
                 po/Makefile.in
		 m4/Makefile
                 helper/Makefile
                 doc/Makefile
                 uim/Makefile
                 scm/Makefile
                 gtk/Makefile
                 xim/Makefile
		 fep/Makefile
		 sumika/Makefile
		 sumika/common/Makefile
		 sumika/gtk2/Makefile
		 sumika/include/Makefile
		 tables/Makefile
		 test/Makefile
                 data/Makefile
                 data/icons/Makefile
		 examples/Makefile
		 examples/uim-custom/Makefile
		 uim.pc])

AC_OUTPUT

AC_MSG_RESULT([
Configure Result :

   Canna        : ${use_canna}
   m17n-lib     : ${use_m17nlib}
   SCIM         : ${use_scim}
   Gtk+         : ${use_gtk2}
   Gnome        : ${use_gnome2}
   Gnome Applet : ${use_applet}
   FEP          : ${use_uim_fep}
   SUMIKA       : ${use_sumika}
   DICT         : ${use_dict}
])
