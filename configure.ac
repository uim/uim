AC_PREREQ(2.57)

AC_INIT([uim], 0.4.8alpha1, [tkng@xem.jp])
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
    PKG_CHECK_MODULES(M17NLIB, m17n-shell >= 1.2.0, use_m17nlib="yes",use_m17nlib="no")
  fi
  ],
  [
    PKG_CHECK_MODULES(M17NLIB, m17n-shell >= 1.2.0, use_m17nlib="yes",use_m17nlib="no")
])

AC_SUBST(M17NLIB_LIBS)


AC_ARG_WITH(scim,
  AC_HELP_STRING([--with-scim],
                 [build against SCIM (too experimental)]),
  [
    case $with_scim in
      no)
        use_scim="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(SCIM, scim >= 1.2.0, use_scim="yes",use_scim="no")
      ;;
    esac
  ],
  [ PKG_CHECK_MODULES(SCIM, scim >= 1.2.0, use_scim="yes",use_scim="no") ])

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

# ***********************
# *** Tests for PRIME ***
# ***********************
AC_ARG_WITH(prime,
	AS_HELP_STRING([--with-prime], [Build a plugin for PRIME]),
	[
	if test "x$with_prime" = "xyes"; then
	        PKG_CHECK_MODULES(PRIME, prime >= 0.8.5.2, use_prime="yes",use_prime="no")
	else
		use_prime="no"
	fi
	],[
	        PKG_CHECK_MODULES(PRIME, prime >= 0.8.5.2, use_prime="yes",use_prime="no")
])


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

# for uim-fep
AM_LANGINFO_CODESET

# Checks for header files.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h locale.h stdlib.h unistd.h])
AC_CHECK_HEADERS([string.h sys/ioctl.h sys/socket.h termios.h wchar.h])
AC_CHECK_HEADERS([sys/time.h sys/stat.h sys/un.h getopt.h assert.h signal.h term.h ncurses/term.h ctype.h pwd.h stdarg.h])
AC_CHECK_HEADERS([pty.h utmp.h util.h libutil.h])
AC_CHECK_HEADERS([curses.h stropts.h])
AC_CHECK_HEADERS([sys/param.h strings.h])

# Check for type
AC_CHECK_TYPES(sig_t, , ,
  [ #include <signal.h> ])

# Checks for library
AC_CHECK_LIB(dl,dlopen)
AC_CHECK_LIB(socket,socket)

# Checks for function
AC_CHECK_FUNCS([cfmakeraw])
AC_CHECK_FUNCS([wcswidth])
AC_CHECK_FUNCS(getpeereid asprintf vasprintf vsnprintf setenv unsetenv strsep)
AC_CHECK_FUNCS(getpid stat mkdir chmod)
AC_CACHE_CHECK([for C99 vsnprintf], uim_cv_HAVE_C99_VSNPRINTF,[
AC_TRY_RUN([
#include <sys/types.h>
#include <stdarg.h>
void foo(const char *format, ...) {
	va_list ap;
	int len;
	char buf[5];

	va_start(ap, format);
	len = vsnprintf(0, 0, format, ap);
	va_end(ap);
	if (len != 5) exit(1);

	if (snprintf(buf, 3, "hello") != 5 || strcmp(buf, "he") != 0) exit(1);

	exit(0);
}
main() { foo("hello"); }
],
uim_cv_HAVE_C99_VSNPRINTF=yes,uim_cv_HAVE_C99_VSNPRINTF=no,uim_cv_HAVE_C99_VSNPRINTF=cross)])
if test x"$uim_cv_HAVE_C99_VSNPRINTF" = x"yes"; then
  AC_DEFINE(HAVE_C99_VSNPRINTF, 1, [define if you have vsnprintf with C99 semantics (set by configure)])
fi

AC_SEARCH_LIBS(dlfunc,dl,
  [
    AC_DEFINE(HAVE_DLFUNC, 1,
              [Define to 1 if you have the `dlfunc' function.])

  ])

GETTEXT_PACKAGE=uim
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE,"$GETTEXT_PACKAGE",[gettext package name])

AM_GNU_GETTEXT([external], [need-ngettext])
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
        PKG_CHECK_MODULES(GTK2_4, gtk+-2.0 >= 2.4.0, use_gtk2_4="yes",use_gtk2_4="no")
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
                 [Build qt-immodule extension]),
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

default_toolkit="gtk"
AC_ARG_ENABLE(default-toolkit,
  AC_HELP_STRING([--default-toolkit],
                 [Determine default toolkit]),
  [],
  [
  if test x"$enable_default_toolkit" = "xgtk" && test x"use_gtk2" = "xyes"; then
     default_toolkit="gtk"
  fi
  if test x"$enable_default_toolkit" = "xqt" && test x"use_qt" = "xyes"; then
     default_toolkit="qt"
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
	    AC_MSG_WARN([fep needs libcurses or libncurses. disabled...]);use_uim_fep="no")])
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
        AC_MSG_WARN([fep needs libcurses or libncurses. disabled...]);use_uim_fep="no")])
    AC_CHECK_FUNCS(forkpty)
    if test $ac_cv_func_forkpty = no; then
      AC_CHECK_LIB(util, forkpty, [AC_DEFINE(HAVE_FORKPTY) FEP_LIBADD="-lutil $FEP_LIBADD"])
    fi
    AC_SUBST(FEP_LIBADD)
  ])

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

dnl ****************************
dnl *** Tests for EB Library ***
dnl ****************************

use_eb="no"
AC_ARG_WITH(eb,
  AS_HELP_STRING([--without-eb],
                 [Don't build against EB]),
  [
  if test "x$with_eb" = "xyes"; then
    _SAVE_LDFLAGS=$LDFLAGS
    EB_LDFLAGS="-lz"
    LDFLAGS="$LDFLAGS $EB_LDFLAGS"
    AC_CHECK_LIB(eb, eb_initialize_library, HAVE_EBLIB=yes, HAVE_EBLIB=no)
    if test "x$HAVE_EBLIB" = "xyes"; then
      AC_CHECK_HEADER(eb/eb.h, HAVE_EBLIB=yes, HAVE_EBLIB=no)
      if test "x$HAVE_EBLIB" = "xyes"; then
        AC_DEFINE(HAVE_EBLIB, 1,
           [Define to 1 if you have EB library and header file])
        EBLIB_LIBS="-leb -lz"
        use_eb="yes"
      fi
    LDFLAGS=$_SAVE_LDFLAGS
    fi
  fi
  ],
  [
  _SAVE_LDFLAGS=$LDFLAGS
  EB_LDFLAGS="-lz"
  LDFLAGS="$LDFLAGS $EB_LDFLAGS"
  AC_CHECK_LIB(eb, eb_initialize_library, HAVE_EBLIB=yes, HAVE_EBLIB=no)
  if test "x$HAVE_EBLIB" = "xyes"; then
    AC_CHECK_HEADER(eb/eb.h, HAVE_EBLIB=yes, HAVE_EBLIB=no)
    if test "x$HAVE_EBLIB" = "xyes"; then
      AC_DEFINE(HAVE_EBLIB, 1,
         [Define to 1 if you have EB library and header file])
      EBLIB_LIBS="-leb -lz"
      use_eb="yes"
    fi
  fi
  LDFLAGS=$_SAVE_LDFLAGS
])

AC_SUBST(EBLIB_LIBS)
dnl This code was taken from librep-0.17

dnl Save in case test with directory specified fails
_cppflags=${CPPFLAGS}
_ldflags=${LDFLAGS}

AC_ARG_WITH(readline-prefix,
        [  --with-readline-prefix=DIR path to readline compatible library],
        [ if test "$withval" != "no" -a "$withval" != "yes"; then

          CPPFLAGS="${CPPFLAGS} -I$withval/include"
          LDFLAGS="${LDFLAGS} -L$withval/lib"
        fi ])

ORIG_LIBS="$LIBS"
unset ac_lib_readline
AC_CACHE_CHECK([for a readline compatible library], ac_lib_readline, [
   for readline_lib in readline edit editline; do
      for termcap_lib in "" termcap terminfo termlib curses ncurses; do
         if test -z "$termcap_lib"; then
            TRY_LIB="-l$readline_lib"
         else
            TRY_LIB="-l$readline_lib -l$termcap_lib"
         fi
         LIBS="$TRY_LIB"
         AC_TRY_LINK_FUNC(readline, ac_lib_readline="$readline_lib $tcap")
         if test -n "$ac_lib_readline"; then
            break
         fi
      done
      if test -n "$ac_lib_readline"; then
         break
      fi
   done
   if test -z "$ac_lib_readline"; then
      ac_lib_readline="no"
   fi
])

if test -n "$ac_lib_readline"; then
   READLINE_LIBS="$LIBS"
else
   READLINE_LIBS=""
fi
LIBS="$ORIG_LIBS"

if test -z "$READLINE_LIBS"; then
   if test "$with_readline_prefix" = "yes"; then
      AC_MSG_ERROR([Can't find readline libraries])
   else
        CPPFLAGS=${_cppflags}
        LDFLAGS=${_ldflags}
   fi
fi

AC_SUBST(READLINE_LIBS)



AM_CONDITIONAL(M17NLIB, test x$use_m17nlib = xyes)
AM_CONDITIONAL(SCIM, test x$use_scim = xyes)
AM_CONDITIONAL(ANTHY, test x$use_anthy = xyes)
AM_CONDITIONAL(CANNA, test x$use_canna = xyes)
AM_CONDITIONAL(PRIME, test x$use_prime = xyes)
AM_CONDITIONAL(SKK, true)
AM_CONDITIONAL(GTK2, test x$use_gtk2 = xyes)
AM_CONDITIONAL(GTK2_4, test x$use_gtk2_4 = xyes)
AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK, test x$default_toolkit = xgtk)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT,  test x$default_toolkit = xqt)
AM_CONDITIONAL(GNOME2, test x$use_gnome2 = xyes)
AM_CONDITIONAL(APPLET, test x$use_applet = xyes)
AM_CONDITIONAL(UIM_FEP, test x$use_uim_fep = xyes)
AM_CONDITIONAL(XIM, test x$use_xim = xyes)
AM_CONDITIONAL(DICT, test x$use_dict = xyes)
AM_CONDITIONAL(EB, test x$use_eb = xyes)
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)
AM_CONDITIONAL(COMPAT_SCM, test x$enable_compat_scm = xyes)
AM_CONDITIONAL(COMPAT_TABLE, test x$enable_compat_table = xyes)
AM_CONDITIONAL(NEED_SETENV_C, test $ac_cv_func_setenv = no -o $ac_cv_func_unsetenv = no)
AM_CONDITIONAL(NEED_STRSEP_C, test $ac_cv_func_strsep = no)


if test "x$use_applet" = "xyes"; then
  AC_PROG_INTLTOOL
fi


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
	CXXFLAGS="$CXXFLAGS -Wnon-virtual-dtor -Wno-long-long -Wcast-align -Wconversion -Wchar-subscripts -Wall -W -Wpointer-arith -Wwrite-strings -Wformat-security"
fi

if test x$enable_compat_scm = xyes; then
  AC_DEFINE(UIM_COMPAT_SCM, 1,
            [Enable obsolete part of uim-scm API])
fi

if test x$enable_compat_table = xyes; then
  AC_DEFINE(UIM_COMPAT_TABLE, 1,
            [Enable obsolete table API for hk.scm (broken)])
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


AC_CONFIG_FILES([test.sh
                 Makefile
                 po/Makefile.in
		 m4/Makefile
                 helper/Makefile
                 doc/Makefile
                 uim/Makefile
		 sigscheme/Makefile
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
   PRIME           : ${use_prime}
   m17n-lib        : ${use_m17nlib}
   SCIM            : ${use_scim}
   Gtk+            : ${use_gtk2}
   Gnome           : ${use_gnome2}
   Gnome Applet    : ${use_applet}
   Qt              : ${use_qt}
   Qt immodule     : ${use_qtimmodule}
   FEP             : ${use_uim_fep}
   XIM             : ${use_xim}
   DICT            : ${use_dict}
   EB              : ${use_eb}
   Default toolkit : ${default_toolkit}
])

if test x$enable_debug = xyes; then
AC_MSG_RESULT([
Configure Result for developers:

   DEBUG           : ${enable_debug}
   COMPAT_SCM      : ${enable_compat_scm}
   COMPAT_TABLE    : ${enable_compat_table}
])
fi
