AC_PREREQ(2.57)
AC_INIT([uim], [1.2.1], [uim@lists.freedesktop.org])
AC_CONFIG_SRCDIR([uim/uim.c])
AC_CONFIG_HEADERS([uim/config.h])
AM_INIT_AUTOMAKE([1.8.3 dist-bzip2])

# Enable GNU extensions such as asprintf(3), BSD-originated functions,
# POSIX features and more on glibc (and some other
# implementations). See features.h. This macro should be placed here.
AC_GNU_SOURCE

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LN_S
AC_PROG_INSTALL
AC_PROG_MAKE_SET
AC_DISABLE_STATIC
AC_PROG_LIBTOOL
AC_PATH_PROG(RSVG, rsvg)

# Checks for libraries
AC_CHECK_LIB(dl,dlopen)
AC_CHECK_LIB(socket,socket)

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
                 [build against SCIM (too experimental)
		  @<:@default=no@:>@]),
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
fi
AC_SUBST(ANTHY_LIBS)
AC_SUBST(ANTHY_CFLAGS)

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
AC_CHECK_HEADERS([fcntl.h locale.h stdlib.h unistd.h errno.h])
AC_CHECK_HEADERS([string.h sys/ioctl.h sys/socket.h termios.h sys/termios.h wchar.h])
AC_CHECK_HEADERS([sys/time.h sys/stat.h sys/un.h getopt.h assert.h signal.h term.h ncurses/term.h ctype.h pwd.h stdarg.h])
AC_CHECK_HEADERS([pty.h utmp.h util.h libutil.h])
AC_CHECK_HEADERS([curses.h stropts.h])
AC_CHECK_HEADERS([sys/param.h strings.h])
AX_CREATE_STDINT_H(uim/uim-stdint.h)

# Check for types
AC_TYPE_SIZE_T
AC_TYPE_PID_T
AC_TYPE_SIGNAL
AC_CHECK_TYPES(sig_atomic_t, , ,
  [ #include <signal.h> ])
AC_CHECK_TYPES(sig_t, , ,
  [ #include <signal.h> ])

# Checks for structures

# Checks for compiler characteristics
AC_C_CONST

# Checks for library functions
AC_FUNC_REALLOC
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_FORK
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_SELECT_ARGTYPES
AC_CHECK_FUNCS([cfmakeraw])
AC_CHECK_FUNCS([wcswidth])
# BSD functions
AC_CHECK_FUNCS(bzero strdup)
# GNU functions
AC_CHECK_FUNCS(asprintf vasprintf)
# C99 functions
AC_CHECK_FUNCS(snprintf vsnprintf)
AC_REPLACE_FUNCS(getpeereid strsep setenv unsetenv strlcpy strlcat)
AC_CHECK_FUNCS(getpid stat mkdir chmod)
AC_CHECK_FUNCS(execv execvp)
AC_CHECK_FUNCS(isascii)
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
              [Define to 1 if you have the dlfunc function.])

  ])

# for uim-fep
AM_LANGINFO_CODESET

dnl Solaris 9 needs -lresolv for inet_aton
NETLIBS=""
AC_CHECK_FUNC(inet_aton, ,
  [AC_CHECK_LIB(resolv, inet_aton, [NETLIBS="-lresolv"])])
AC_SUBST(NETLIBS)

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

AC_ARG_ENABLE(applet,
  AC_HELP_STRING([--disable-applet],
                 [disable uim applet for Gnome panel]),
  [
    case $enable_applet in
      no)
        use_applet="no"
      ;;
      yes|*)
        PKG_CHECK_MODULES(APPLET, libpanelapplet-2.0, use_applet="yes",use_applet="no")
        PKG_CHECK_MODULES(APPLET_2_14, libpanelapplet-2.0 >= 2.14.0, panelapplet_have_set_background_api="yes",panelapplet_have_set_background_api="no")
      ;;
    esac
  ],
  [
    PKG_CHECK_MODULES(APPLET, libpanelapplet-2.0, use_applet="yes",use_applet="no")
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

default_toolkit="gtk"
AC_ARG_ENABLE(default-toolkit,
  AC_HELP_STRING([--enable-default-toolkit],
                 [Determine default toolkit (gtk or qt)
		  @<:@default=gtk@:>@]),
  [
  if test x"$enable_default_toolkit" = "xgtk" && test x"$use_gtk2" = "xyes"; then
     default_toolkit="gtk"
  fi
  if test x"$enable_default_toolkit" = "xqt" && test x"$use_qt" = "xyes"; then
     default_toolkit="qt"
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

# Check whether user wants libedit support
# This code was based on openssh-4.1p1

AC_ARG_WITH(libedit,
AC_HELP_STRING([--with-libedit[=DIR], Enable libedit support
				      @<:@default=yes@:>@]),
  [
   if test "x$with_libedit" != "xno"; then
        use_libedit="yes"
	libedit_path="$withval"
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
		   LIBEDIT_LIBS="-ledit -lcurses -l$libedit_path/lib"
		   AC_SUBST(LIBEDIT_LIBS)
		], [ 
                   AC_MSG_WARN("libedit not found. Disabled...")
                   use_libedit="no"
                ])
     CPPFLAGS=$saved_CPPFLAGS
     LDFLAGS=$saved_LDFLAGS
fi

dnl for uim-dict.xml.in
if test "x$use_anthy" = "xyes"; then
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
AM_CONDITIONAL(CANNA, test x$use_canna = xyes)
AM_CONDITIONAL(MANA, test x$use_mana = xyes)
AM_CONDITIONAL(PRIME, test x$use_prime = xyes)
AM_CONDITIONAL(SKK, true)
AM_CONDITIONAL(GTK2, test x$use_gtk2 = xyes)
AM_CONDITIONAL(GTK2_4, test x$use_gtk2_4 = xyes)
AM_CONDITIONAL(DEFAULT_TOOLKIT_GTK, test x$default_toolkit = xgtk)
AM_CONDITIONAL(DEFAULT_TOOLKIT_QT,  test x$default_toolkit = xqt)
AM_CONDITIONAL(APPLET, test x$use_applet = xyes)
AM_CONDITIONAL(UIM_FEP, test x$use_uim_fep = xyes)
AM_CONDITIONAL(UIM_EL, test x$use_uim_el = xyes)
AM_CONDITIONAL(XIM, test x$use_xim = xyes)
AM_CONDITIONAL(DICT, test x$use_dict = xyes)
AM_CONDITIONAL(EB, test x$use_eb = xyes)
AM_CONDITIONAL(LIBEDIT, test x$use_libedit = xyes)
AM_CONDITIONAL(DEBUG, test x$enable_debug = xyes)
AM_CONDITIONAL(COMPAT_SCM, test x$enable_compat_scm = xyes)
AM_CONDITIONAL(NEED_SETENV_C, test $ac_cv_func_setenv = no -o $ac_cv_func_unsetenv = no)
AM_CONDITIONAL(NEED_STRSEP_C, test $ac_cv_func_strsep = no)

AC_PROG_INTLTOOL([0.31], [no-xml])


GTK_BINARY_VERSION=`$PKG_CONFIG gtk+-2.0 --variable=gtk_binary_version`
AC_SUBST(GTK_BINARY_VERSION)

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

if test "x$use_qt" = "xyes"; then
    KDE_ICONDIR=`kde-config --expandvars --install icon`
    if test -z "$KDE_ICONDIR"; then
      KDE_ICONDIR="$prefix/share/icons"
    fi
    AC_DEFINE_UNQUOTED(KDE_ICONDIR, "$KDE_ICONDIR", [kde icon directory])
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
	else
	  if test x"$default_toolkit" = "xqt" && test x"$use_qt" = "xyes"; then
	    use_pref="yes"
	  else
	    use_pref="no"
	    AC_MSG_WARN([uim-pref needs Gtk+ or Qt toolkit, disabled...])
	  fi
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

# add debugging flags to CFLAGS regardless of GCC=yes or not
if test x$enable_debug = xyes; then
	CFLAGS="$CFLAGS -g -pipe"
	CXXFLAGS="$CXXFLAGS -g -pipe"
else
	CFLAGS="$CFLAGS -O2 -pipe"
	CXXFLAGS="$CXXFLAGS -O2 -pipe"
fi

# add warning flags to CFLAGS if GCC=yes
if test x$ac_cv_c_compiler_gnu = xyes; then
#if test x$CC = xgcc; then
	CFLAGS="$CFLAGS -Wall -std=gnu89 -pedantic -Wchar-subscripts -Wmissing-declarations -Wredundant-decls -Wmissing-prototypes -Wnested-externs -Wpointer-arith -Wcast-align -Wsign-compare"
fi

if test x$ac_cv_cxx_compiler_gnu = xyes; then
	CXXFLAGS="$CXXFLAGS -Wnon-virtual-dtor -Wno-long-long -Wcast-align -Wconversion -Wchar-subscripts -Wall -W -Wpointer-arith -Wwrite-strings -Wformat-security"
fi

AC_ARG_ENABLE(warnings-into-error,
    AC_HELP_STRING([--enable-warnings-into-error],
		   [Treat compiler warnings as error @<:@default=no if !debug@:>@]),
    [
        case $enable_warnings_into_error in
	no)
	    ;;
	yes|*)
	    CFLAGS="$CFLAGS -Werror"
	    CXXFLAGS="$CXXFLAGS -Werror"
	    ;;
	esac
    ], [
	    if test x$enable_debug = xyes; then
	      CFLAGS="$CFLAGS -Werror"
	      CXXFLAGS="$CXXFLAGS -Werror"
	    fi
    ])


if test x$enable_compat_scm = xyes; then
  AC_DEFINE(UIM_COMPAT_SCM, 1,
            [Enable obsolete part of uim-scm API])
fi

# Checks for system services


AH_BOTTOM([/* Include os specific header. */
#include "os_dep.h"])

# Add include path
INCLUDES='-I$(top_srcdir)/replace -I$(top_srcdir)/uim'
AC_SUBST(INCLUDES)

SRCDIR=$srcdir
AC_SUBST(SRCDIR)

AC_SUBST(abs_srcdir)
AC_SUBST(abs_builddir)
AC_SUBST(abs_top_srcdir)
AC_SUBST(abs_top_builddir)

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
		 xim/Makefile
		 fep/Makefile
		 emacs/Makefile
		 test/Makefile
		 examples/Makefile
		 examples/uim-custom/Makefile
		 pixmaps/Makefile
		 replace/Makefile
		 uim.pc
		 uim.desktop.in
		 uim.spec
		 ])

AC_OUTPUT

AC_MSG_RESULT([
Configure Result :

   Anthy           : ${use_anthy}
   Canna           : ${use_canna}
   Mana            : ${use_mana}
   PRIME           : ${use_prime}
   m17n-lib        : ${use_m17nlib}
   SCIM            : ${use_scim}
   Gtk+            : ${use_gtk2}
   Gnome Applet    : ${use_applet}
   Qt              : ${use_qt}
   Qt immodule     : ${use_qtimmodule}
   FEP             : ${use_uim_fep}
   Emacs           : ${use_uim_el}
   XIM             : ${use_xim}
   Pref            : ${use_pref}
   DICT            : ${use_dict}
   EB              : ${use_eb}
   libedit         : ${use_libedit}
   Default toolkit : ${default_toolkit}
])

if test x$enable_debug = xyes; then
AC_MSG_RESULT([
Configure Result for developers:

   DEBUG           : ${enable_debug}
   COMPAT_SCM      : ${enable_compat_scm}
])
fi
