##### 
#
# SYNOPSIS
#
#   AX_PATH_QMAKE6
#
# DESCRIPTION
#
#   Check for Qt6 version of qmake.
#
#   $QMAKE6 is set to absolute name of the executable if found.
#
# COPYLEFT
#
#   Copyright (c) 2007 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#   Copyright (c) 2012 Muneyuki Noguchi <nogu.dev AT gmail.com>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

AC_DEFUN([AX_PATH_QMAKE6], [
  ax_guessed_qt6_dirs="${QTDIR}/bin"
  ax_guessed_qt6_dirs="${QT6DIR}/bin:${ax_guessed_qt6_dirs}"
  ax_guessed_qt6_dirs="/usr/local/qt6/bin:${ax_guessed_qt6_dirs}"
  ax_guessed_qt6_dirs="/usr/qt6/bin:${ax_guessed_qt6_dirs}"
  ax_guessed_qt6_dirs="/usr/local/lib/qt6/bin:${ax_guessed_qt6_dirs}"
  ax_guessed_qt6_dirs="/usr/lib/qt6/bin:${ax_guessed_qt6_dirs}"
  if type dpkg-architecture > /dev/null 2>&1; then
    multiarch=$(dpkg-archtecture --query DEB_BUILD_MULTIARCH)
    ax_guessed_qt6_dirs="/usr/lib/${multiarch}/qt6/bin:${ax_guessed_qt6_dirs}"
  fi
  AC_PROG_EGREP
  AC_PATH_PROGS(_QMAKE6, [qmake-qt6 qmake6], [], ["$PATH:$ax_guessed_qt6_dirs"])
  AC_PATH_PROGS(_QMAKE, [qmake], [], ["$PATH:$ax_guessed_qt6_dirs"])

  AC_CACHE_CHECK([for Qt6 version of qmake], ax_cv_path_QMAKE6, [
    ax_cv_path_QMAKE6=no
    for qmake6 in ${_QMAKE6} ${_QMAKE}; do
      if ($qmake6 --version 2>&1 | $EGREP -q 'Qt version 6'); then
        QMAKE6="$qmake6"
        ax_cv_path_QMAKE6="$qmake6"
	break
      fi
    done
  ])
  AC_SUBST([QMAKE6])
])
