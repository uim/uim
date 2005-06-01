dnl xft.m4
dnl Copyright (c) 2002 Henrik Kinnunen (fluxgen at linuxmail.org)
dnl Large changes for xine-ui usage: 2004 Daniel Caujolle-Bert (f1rmb@users.sourceforge.net)

dnl Permission is hereby granted, free of charge, to any person obtaining a
dnl copy of this software and associated documentation files (the "Software"),
dnl to deal in the Software without restriction, including without limitation
dnl the rights to use, copy, modify, merge, publish, distribute, sublicense,
dnl and/or sell copies of the Software, and to permit persons to whom the 
dnl Software is furnished to do so, subject to the following conditions:

dnl The above copyright notice and this permission notice shall be included in 
dnl all copies or substantial portions of the Software. 

dnl THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
dnl IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
dnl FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
dnl THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
dnl LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
dnl FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
dnl DEALINGS IN THE SOFTWARE.

dnl AM_PATH_XFT2([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
AC_DEFUN([AM_PATH_XFT2],
[

  if test "x$PKG_CONFIG" = "xno" ; then
    AC_PATH_PROG(XFT_CONFIG, xft-config, no)
    if test "x$XFT_CONFIG" = "xno" ; then
      ifelse([$2], , :, [$2])
    else
      XFT_CFLAGS=`$XFT_CONFIG --cflags`
      XFT_LIBS=`$XFT_CONFIG --libs`
      ifelse([$1], , :, [$1])
    fi
  else
    XFT_CFLAGS=`$PKG_CONFIG --cflags xft`
    XFT_LIBS=`$PKG_CONFIG --libs xft`
    ifelse([$1], , :, [$1])
  fi

])

dnl AM_PATH_XFT(default-value, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for Xft, and define XFT_CFLAGS and XFT_LIBS
AC_DEFUN([AM_PATH_XFT],
[
  AC_ARG_ENABLE(xft, [  --enable-xft            Xft (antialias) support (default=$1)],
    [ if test "x$enableval" = "xyes"; then
        TRY_XFT=yes
      else
	TRY_XFT=no
      fi
    ],
      TRY_XFT=$1
  )

  if test "x$TRY_XFT" = "xyes"; then
    AC_MSG_RESULT(yes)
    AM_PATH_XFT2([$2],
		 [$3]
		 AC_MSG_RESULT([Cant find Xft libraries! Disabling Xft]))
  else
    AC_MSG_RESULT(no)
	[$3]
  fi

  AC_SUBST(XFT_CFLAGS)
  AC_SUBST(XFT_LIBS)
])
