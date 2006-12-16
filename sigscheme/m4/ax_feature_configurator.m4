##### 
#
# SYNOPSIS
#
#   AX_FEATURE_CONFIGURATOR
#
# DESCRIPTION
#
#   A dependency-based feature configurator.
#
#   This macro suite is still under development.
#
# LAST MODIFICATION
#
#   2006-12-16
#
# COPYLEFT
#
#   Copyright (c) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

# AX_FEATURE_CONFIGURATOR
AC_DEFUN([AX_FEATURE_CONFIGURATOR], [
  AC_PROG_EGREP
  AX_FEATURE_ID_PREFIX([use_], [USE_], [USE_])
])

# AX_FEATURE_ID_PREFIX(SH_VAR_PREFIX, AC_DEF_PREFIX, AM_COND_PREFIX)
AC_DEFUN([AX_FEATURE_ID_PREFIX], [
  m4_undefine([AX_FEATURE_PREFIX_VAR])
  m4_undefine([AX_FEATURE_PREFIX_DEF])
  m4_undefine([AX_FEATURE_PREFIX_COND])

  m4_define([AX_FEATURE_PREFIX_VAR],  [$1])
  m4_define([AX_FEATURE_PREFIX_DEF],  [$2])
  m4_define([AX_FEATURE_PREFIX_COND], [$3])
])

# AX_FEATURE_VAR_Y(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_Y], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [yes], [yes])
])

# AX_FEATURE_VAR_N(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_N], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [no], [no])
])

# AX_FEATURE_VAR_X(FEATURE, DESC, VAL, VAL_CHOICES)
AC_DEFUN([AX_FEATURE_VAR_X], [
  if echo '[$4]' | $EGREP -q "\b[$3]\b"; then
    if test -z "${AX_FEATURE_PREFIX_VAR[]AS_TR_SH([$1])}"; then
      AX_FEATURE_PREFIX_VAR[]AS_TR_SH([$1])=[$3]
      AS_VAR_SET(AX_FEATURE_PREFIX_VAR[]AS_TR_SH([$1])_[$3], [yes])
    fi
    m4_define(AX_FEATURE_DESC_[]AS_TR_CPP([$1]), [$2])
  else
    AC_MSG_ERROR("invalid value --enable-[$1]=[$3]")
  fi
])

# AX_FEATURE_ARG_Y(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_ARG_Y], [
  AX_FEATURE_ARG_X([$1], m4_default([$2], [$1]), [yes], [yes no])
])

# AX_FEATURE_ARG_N(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_ARG_N], [
  AX_FEATURE_ARG_X([$1], m4_default([$2], [$1]), [no], [yes no])
])

# AX_FEATURE_ARG_X(FEATURE, DESC, DEFAULT_VAL, VAL_CHOICES, [ACTION])
AC_DEFUN([AX_FEATURE_ARG_X], [
  AC_ARG_ENABLE([$1],
    AS_HELP_STRING(m4_bmatch([$3],
                             [^yes$], [--disable-[$1]],
                             [^no$],  [--enable-[$1]],
                             [--enable-[$1]=m4_toupper([$1])]),
                   m4_bmatch([$3],
                             [^yes$], [disable ],
                             [^no$],  [enable ],
                             [])[$2]),
    [AX_FEATURE_VAR_X([$1], [$2], [${enable_]AS_TR_SH([$1])[}], [$4])],
    [
      [enable_]AS_TR_SH([$1])='AS_TR_SH([$3])'
      AX_FEATURE_VAR_X([$1], [$2], [${enable_]AS_TR_SH([$1])[}], [$4])
    ])
  [$5]
])

# AX_FEATURE_DEFINE(FEATURE, [AC_DEF_SYM], [AM_COND_SYM], [DESC])
AC_DEFUN([AX_FEATURE_DEFINE], [
  m4_define_default(AX_FEATURE_DESC_[]AS_TR_CPP([$1]),
                    [$1])
  if test -n "${AX_FEATURE_PREFIX_VAR[][$1]}" \
          -a "x${AX_FEATURE_PREFIX_VAR[][$1]}" != xno
  then
    AC_DEFINE(m4_default([$2], AX_FEATURE_PREFIX_DEF[]m4_toupper([$1])), 1,
              m4_default([$4], AX_FEATURE_DESC_[]AS_TR_CPP([$1])))
  fi
  AM_CONDITIONAL(m4_default([$3], AX_FEATURE_PREFIX_COND[]m4_toupper([$1])),
                 test "x[]AX_FEATURE_PREFIX_VAR[][$1]" != xno)
])

# AX_FEATURE_RESOLVE_DEPENDENCIES(RULESET, [VAL])
AC_DEFUN([AX_FEATURE_RESOLVE_DEPENDENCIES], [
])

# AX_FEATURE_RESOLVE_WEAK_DEPENDENCIES(RULESET)
AC_DEFUN([AX_FEATURE_RESOLVE_WEAK_DEPENDENCIES], [
])

# AX_FEATURE_DETECT_CONFLICTS(RULESET)
AC_DEFUN([AX_FEATURE_DETECT_CONFLICTS], [
])
