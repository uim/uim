##### 
#
# SYNOPSIS
#
#   AX_PATH_QMAKE5
#
# DESCRIPTION
#
#   Check for Qt5 version of qmake.
#
#   $QMAKE5 is set to absolute name of the executable if found.
#
# COPYLEFT
#
#   Copyright (c) 2007 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#   Copyright (c) 2012 Muneyuki Noguchi <nogu.dev AT gmail.com>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

AC_DEFUN([AX_PATH_QMAKE5], [
  ax_guessed_qt5_dirs="/usr/lib/qt5/bin:/usr/local/lib/qt5/bin:/usr/qt5/bin:/usr/local/qt5/bin:${QT5DIR}/bin:${QTDIR}/bin"
  AC_PROG_EGREP
  AC_PATH_PROGS(_QMAKE5, [qmake-qt5 qmake5], [], ["$PATH:$ax_guessed_qt5_dirs"])
  AC_PATH_PROGS(_QMAKE, [qmake], [], ["$PATH:$ax_guessed_qt5_dirs"])

  AC_CACHE_CHECK([for Qt5 version of qmake], ax_cv_path_QMAKE5, [
    ax_cv_path_QMAKE5=no
    for qmake5 in ${_QMAKE5} ${_QMAKE}; do
      if ($qmake5 --version 2>&1 | $EGREP -q 'Qt version 5'); then
        QMAKE5="$qmake5"
        ax_cv_path_QMAKE5="$qmake5"
	break
      fi
    done
  ])
  AC_SUBST([QMAKE5])
])
