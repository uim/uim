##### http://websvn.freedesktop.org/uim/branches/r5rs/sigscheme/m4/ax_feature_configurator.m4
#
# SYNOPSIS
#
#   AX_FEATURE_CONFIGURATOR
#
# DESCRIPTION
#
#   A dependency-based feature configurator.
#
#   This macro suite is still under development. See
#   http://websvn.freedesktop.org/uim/branches/r5rs/sigscheme/configure.in
#   as a working example.
#
# LAST MODIFICATION
#
#   2006-12-17
#
# COPYLEFT
#
#   Copyright (c) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#
#   Copying and distribution of this file, with or without
#   modification, are permitted in any medium without royalty provided
#   the copyright notice and this notice are preserved.

# AX_FEATURE_CONFIGURATOR([SH_VAR_PREFIX], [AC_DEF_PREFIX], [AM_COND_PREFIX])
AC_DEFUN([AX_FEATURE_CONFIGURATOR], [
  AC_PROG_EGREP

  m4_define([AX_FEATURE_PREFIX_VAR],  m4_default([$1], [use_]))
  m4_define([AX_FEATURE_PREFIX_DEF],  m4_default([$2], [USE_]))
  m4_define([AX_FEATURE_PREFIX_COND], m4_default([$3], [USE_]))

  ax_feature_list_all=''
  ax_feature_list_explicit=''
  ax_feature_list_implicit=''
])

# AX_FEATURE_VAR_Y(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_Y], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [yes], [yes])
])

# AX_FEATURE_VAR_N(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_N], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [no], [no])
])

# AX_FEATURE_VAR_X(FEATURE, DESC, DEFAULT-VAL, VAL-CHOICES)
AC_DEFUN([AX_FEATURE_VAR_X], [
  m4_define(AX_FEATURE_DESC_[]AS_TR_CPP([$1]), [$2])

  if echo '[$4]' | $EGREP -q "\b[$3]\b"; then
    _ax_feature_id=AS_TR_SH([$1])
    _ax_feature_val=AS_VAR_GET(AX_FEATURE_PREFIX_VAR[]$_ax_feature_id)
    if test -z "$_ax_feature_val"; then
      _ax_feature_val=AS_TR_SH([$3])
    fi
    _ax_feature_id_val="${_ax_feature_id}_${_ax_feature_val}"
    ax_feature_list_all="$ax_feature_list_all $_ax_feature_id"

    AS_VAR_SET(AX_FEATURE_PREFIX_VAR[]$_ax_feature_id, $_ax_feature_val)
    AS_VAR_SET(AX_FEATURE_PREFIX_VAR[]$_ax_feature_id_val, [yes])
    if test "x$_ax_feature_val" != xno; then
      ax_feature_list_explicit="$ax_feature_list_explicit $_ax_feature_id"
      if test "x$_ax_feature_val" != xyes; then
        ax_feature_list_explicit="$ax_feature_list_explicit $_ax_feature_id_val"
      fi
    fi
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

# _AX_FEATURE_OVERRIDE_VALS(DEPENDENT-VAL, FEATURE-LIST)
AC_DEFUN([_AX_FEATURE_OVERRIDE_VALS], [
    for feature in [$2]; do
      _ax_feature_var="AX_FEATURE_PREFIX_VAR[]$feature"
      _ax_feature_val=AS_VAR_GET([$_ax_feature_var])
  
      if test -z "$_ax_feature_val" -o "x$_ax_feature_val" = xno; then
        AS_VAR_SET([${_ax_feature_var}], [$1])
        AS_VAR_SET([${_ax_feature_var}_][$1], [yes])
        AS_VAR_SET([${_ax_feature_var}_no], [""])
      fi
   done
])

# AX_FEATURE_RESOLVE_DEPENDENCIES(GROUP, RULESET, [DEPENDENT-VAL])
AC_DEFUN([AX_FEATURE_RESOLVE_DEPENDENCIES], [
  AC_MSG_CHECKING([$1][ dependencies of features])
  _ax_feature_resolve_deps () {
    # FIXME: improper temporary directory handlings
    rm -rf confdeps
    mkdir confdeps
    # FIXME: GNU make dependency
    gmake -s -C confdeps -f - @S|@@ __deps <<'EOT'
[$2]
[
%::
	@touch @S|@@
.PHONY: __deps
__deps:
	@ls]
EOT
    rm -rf confdeps
  }
  _ax_feature_list=`_ax_feature_resolve_deps $ax_feature_list_explicit`
  _AX_FEATURE_OVERRIDE_VALS(m4_default([$3], [yes]), [$_ax_feature_list])
  AC_MSG_RESULT([resolved])
])

# AX_FEATURE_RESOLVE_WEAK_DEPENDENCIES(RULESET)
AC_DEFUN([AX_FEATURE_RESOLVE_WEAK_DEPENDENCIES], [
  AX_FEATURE_RESOLVE_DEPENDENCIES([weak], [$1], [weak])
])

# AX_FEATURE_DETECT_CONFLICTS(RULESET)
AC_DEFUN([AX_FEATURE_DETECT_CONFLICTS], [
])
