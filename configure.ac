AC_PREREQ(2.57)

AC_INIT([uim], 0.4.6alpha1, [tkng@xem.jp])
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

# ***********************
# *** Tests for Anthy ***
# ***********************
AC_ARG_WITH(anthy,
  AC_HELP_STRING([--without-anthy], [Don't build with libanthy]),
  [
    if test "x$with_anthy" = "xyes"; then
      AC_CHECK_HEADERS([anthy/anthy.h], [use_anthy="yes"],
                       [use_anthy="no"])
    else
      use_anthy="no"
    fi
  ],
  [
    AC_CHECK_HEADERS([anthy/anthy.h], [use_anthy="yes"],
                     [use_anthy="no"])
  ])

# ***********************
# *** Tests for Canna ***
# ***********************
AC_ARG_WITH(canna,
	AS_HELP_STRING([--with-canna], [Build with libcanna]),
	[
	if test "x$with_canna" = "xyes"; then
		AC_CHECK_HEADERS([canna/RK.h], [use_canna="yes"],
				 [use_canna="no"], [ #undef _WCHAR_T ])
    		AC_CHECK_LIB(canna, RkInitialize, use_canna=yes, use_canna=no)
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
AC_CHECK_LIB(curses, setupterm, FEP_LIBADD="-lcurses $FEP_LIBADD")
AC_CHECK_LIB(ncurses, setupterm, FEP_LIBADD="-lncurses $FEP_LIBADD")

# Checks for function
AC_CHECK_FUNCS(forkpty)
if test $ac_cv_func_forkpty = no; then
  AC_CHECK_LIB(util, forkpty, [AC_DEFINE(HAVE_FORKPTY) FEP_LIBADD="-lutil $FEP_LIBADD"])
fi
AC_SUBST(FEP_LIBADD)
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


# FIXME: Probably voilent way  -- YamaKen 2005-01-07
if test x$prefix = xNONE; then
   prefix=$ac_default_prefix
fi
if test x$exec_prefix = xNONE; then
   exec_prefix=$prefix
fi

LIBDIR=`eval echo ${libdir}`
AC_DEFINE_UNQUOTED(LIBDIR, "$LIBDIR", [libraries dir])
DATADIR=`eval echo ${datadir}`
AC_DEFINE_UNQUOTED(DATADIR, "$DATADIR", [read-only architecture-independent data dir])
# we should use more safe macro such as AC_EXPAND_DIR
LOCALEDIR=`eval echo ${datadir}/locale`
# backward compatibility
localedir=$LOCALEDIR
AC_DEFINE_UNQUOTED(LOCALEDIR, "$LOCALEDIR", [locale dir])
AC_DEFINE_UNQUOTED(GNOMELOCALEDIR, "$LOCALEDIR", [locale dir for gnome])
# define XLIB directory
AC_DEFINE_UNQUOTED(XLIB_DIR, "$x_libraries", [X11 Library Directory])

AC_DEFINE_UNQUOTED(UIM_PIXMAPSDIR, "${DATADIR}/${PACKAGE}/pixmaps", [pixmaps directory])

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
        PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0, use_gtk2.4="yes",use_gtk2.4="no")
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(GTK2, gtk+-2.0 >= 2.2.0, use_gtk2="yes",use_gtk2="no")
    PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0, use_gtk2_4="yes",use_gtk2_4="no") ])

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

dnl ****************************
dnl *** Check for Qt Library ***
dnl ****************************
AC_ARG_WITH(qt,
  AC_HELP_STRING([--with-qt],
                 [build with qt helper applications]),
  [
    case $enable_qt in
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
                 [Build qt-immodule extension]),
  [
    case $enable_qtimmodule in
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

AC_ARG_ENABLE(dummy,
  AC_HELP_STRING([--default-toolkit],
                 [Determine default toolkit]),
  [],
  [
  default_toolkit=gtk
  if test x"$default_toolkit" = "xgtk" && test x"use_gtk2" = "xyes"; then
     default_toolkit=gtk
  fi
  ])

AC_ARG_ENABLE(debug,
  AC_HELP_STRING([--enable-debug],
                 [enable debugging]),
  [],
  [
    enable_debug="no"
  ])

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

AC_ARG_ENABLE(compat-table,
  AC_HELP_STRING([--enable-compat-table],
                 [enable obsolete table API for hk.scm (broken)]),
  [],
  [
    enable_compat_table="no"
  ])


AC_ARG_ENABLE(scm-nested-eval,
  AC_HELP_STRING([--disable-scm-nested-eval],
                 [disable nested Scheme evaluation from C (i.e. C -> Scheme -> C -> Scheme) (for developer, will be obsolete)]),
  [
    if test "$enable_scm_nested_eval" != "no" ;then
      enable_scm_nested_eval="yes"
    fi
  ],
  [
    enable_scm_nested_eval="yes"
  ])

AC_ARG_ENABLE(callback-queue,
  AC_HELP_STRING([--enable-callback-queue],
                 [enable callback queue (for developer, will be obsolete)]),
  [],
  [
    enable_callback_queue="no"
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
AM_CONDITIONAL(ANTHY, test x$use_anthy = xyes)
AM_CONDITIONAL(CANNA, test x$use_canna = xyes)
AM_CONDITIONAL(PRIME, true)
AM_CONDITIONAL(SKK, true)
AM_CONDITIONAL(GTK2, test x$use_gtk2 = xyes)
AM_CONDITIONAL(GTK2_4, test x$use_gtk2_4 = xyes)
AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK, test x$default_toolkit = xgtk)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT,  test x$default_toolkit = xqt)
AM_CONDITIONAL(GNOME2, test x$use_gnome2 = xyes)
AM_CONDITIONAL(QT, test x$use_qt = xyes)
AM_CONDITIONAL(QT_IMMODULE, test x$use_qtimmodule = xyes)
AM_CONDITIONAL(APPLET, test x$use_applet = xyes)
AM_CONDITIONAL(UIM_FEP, test x$use_uim_fep = xyes)
AM_CONDITIONAL(DICT, test x$use_dict = xyes)
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)
AM_CONDITIONAL(COMPAT_SCM, test x$enable_compat_scm = xyes)
AM_CONDITIONAL(COMPAT_TABLE, test x$enable_compat_table = xyes)
AM_CONDITIONAL(SCM_NESTED_EVAL, test x$enable_scm_nested_eval = xyes)
AM_CONDITIONAL(CALLBACK_QUEUE, test x$enable_callback_queue = xyes)
AM_CONDITIONAL(NEED_SETENV_C, test $ac_cv_func_setenv = no -o $ac_cv_func_unsetenv = no)
AM_CONDITIONAL(NEED_STRSEP_C, test $ac_cv_func_strsep = no)

GTK_BINARY_VERSION=`pkg-config gtk+-2.0 --variable=gtk_binary_version`
AC_SUBST(GTK_BINARY_VERSION)

if test x$use_gtk2 = xyes ; then
AC_DEFINE(USE_GTK2, 1, [use gtk2])
fi

#if test x$use_gnome2 = xyes ; then
#AC_DEFINE(USE_GNOME2, 1, [use gnome2])
#fi

if test x$use_qt = xyes ; then
    # minimum version
    QT_VERSION=3.2.0
    QT_VERSION_NUM=320

    # GUESS $QTDIR
    if test -z "$QTDIR"; then
      QTDIR="/usr/lib/qt3"
    fi
    if test -z "$QTDIR"; then
      QTDIR="/usr/lib/qt"
    fi
    if test -z "$QTDIR"; then
      QTDIR="/usr"
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
       QTINCDIR="/usr/lib"
    fi

    # Check For MOC
    if test -x "$QTDIR/bin/moc"; then
      HOST_MOC="$QTDIR/bin/moc"
    else
      AC_CHECK_PROGS(HOST_MOC, moc, "")
    fi
    if test -z "$HOST_MOC"; then
      AC_MSG_ERROR([no acceptable moc( meta object compiler ) found])
    fi
    MOC=$HOST_MOC

    # Check For UIC
    if test -x "$QTDIR/bin/uic"; then
      HOST_UIC="$QTDIR/bin/uic"
    else
      AC_CHECK_PROGS(HOST_UIC, uic, "")
    fi
    if test -z "$HOST_UIC"; then
      AC_MSG_ERROR([no acceptable uic( user interface compiler ) found])
    fi
    UIC=$HOST_UIC

    # GUESS plugins dir for immodule installation
    AC_MSG_CHECKING(for qt-immodule plugins dir)
    if test -z "$QTDIR/plugins"; then
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
    QT_LIBS="-L/usr/X11R6/lib $QT_LDFLAGS $QT_LIB -lXext -lX11"

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
       AC_MSG_ERROR([Qt Helper requires at least version $QT_VERSION of Qt])
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

if test x$use_qt = xyes ; then
    AC_MSG_CHECKING(for qt-immodule patch)
    if test x$use_qtimmodule = xyes ; then
        # Check for immodule for Qt patch
        if test ! -e $QTINCDIR/qinputcontext.h -o ! -e $QTINCDIR/qinputcontextplugin.h; then
          AC_MSG_RESULT(no)
          $use_qtimmodule="cannot"
        else
	  AC_MSG_RESULT(yes)
        fi
    fi
fi

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
	CXXFLAGS="$CXXFLAGS -Wnon-virtual-dtor -Wno-long-long -ansi -Wcast-align -Wconversion -Wchar-subscripts -Wall -W -Wpointer-arith -Wwrite-strings -Wformat-security -Wmissing-format-attribute"
fi

if test x$enable_compat_scm = xyes; then
  AC_DEFINE(UIM_COMPAT_SCM, 1,
            [Enable obsolete part of uim-scm API])
fi

if test x$enable_compat_table = xyes; then
  AC_DEFINE(UIM_COMPAT_TABLE, 1,
            [Enable obsolete table API for hk.scm (broken)])
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
                 qt/Makefile
                 xim/Makefile
		 fep/Makefile
		 tables/Makefile
		 test/Makefile
		 examples/Makefile
		 examples/uim-custom/Makefile
		 pixmaps/Makefile
		 uim.pc])

AC_OUTPUT

AC_MSG_RESULT([
Configure Result :

   Anthy           : ${use_anthy}
   Canna           : ${use_canna}
   m17n-lib        : ${use_m17nlib}
   SCIM            : ${use_scim}
   Gtk+            : ${use_gtk2}
   Gnome           : ${use_gnome2}
   Gnome Applet    : ${use_applet}
   Qt              : ${use_qt}
   Qt immodule     : ${use_qtimmodule}
   FEP             : ${use_uim_fep}
   DICT            : ${use_dict}
   Default toolkit : ${default_toolkit}
])

if test x$enable_debug = xyes; then
AC_MSG_RESULT([
Configure Result for developers:

   DEBUG           : ${enable_debug}
   COMPAT_SCM      : ${enable_compat_scm}
   COMPAT_TABLE    : ${enable_compat_table}
   SCM_NESTED_EVAL : ${enable_scm_nested_eval}
   CALLBACK_QUEUE  : ${enable_callback_queue}
])
fi
