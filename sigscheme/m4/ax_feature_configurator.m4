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
#   2006-12-18
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
  CHECK_GNU_MAKE
  if test "x$_cv_gnu_make_command" != x; then
    GMAKE=$_cv_gnu_make_command
  else
    AC_MSG_ERROR([GNU make is currently required to run ax_feature_configurator.m4])
  fi

  m4_define([AX_FEATURE_PREFIX_VAR],  m4_default([$1], [use_]))
  m4_define([AX_FEATURE_PREFIX_DEF],  m4_default([$2], [USE_]))
  m4_define([AX_FEATURE_PREFIX_COND], m4_default([$3], [USE_]))

  ax_feature_state='init'
  ax_feature_list_all=''
  ax_feature_list_explicit=''
  ax_feature_list_implicit=''
])

# AX_FEATURE_VAR_Y(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_Y], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [yes], [^yes$])
])

# AX_FEATURE_VAR_N(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_VAR_N], [
  AX_FEATURE_VAR_X([$1], m4_default([$2], [$1]), [no], [^no$])
])

# AX_FEATURE_VAR_X(FEATURE, DESC, DEFAULT-VAL, VAL-REGEXP)
AC_DEFUN([AX_FEATURE_VAR_X], [
  m4_define(AX_FEATURE_DESC_[]AS_TR_CPP([$1]), [$2])

  case $ax_feature_state in
    init|seed)
      ax_feature_state='seed'
      ;;
    *)
      AC_MSG_ERROR([invalid macros layout for ax_feature_configurator.m4])
      ;;
  esac

  if echo "[$3]" | $EGREP -q "AS_ESCAPE([$4])"; then
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
      ax_feature_list_implicit="$ax_feature_list_explicit"
    fi
  else
    AC_MSG_ERROR("invalid value --enable-[$1]=[$3]")
  fi
])

# AX_FEATURE_ARG_Y(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_ARG_Y], [
  AX_FEATURE_ARG_X([$1], m4_default([$2], [$1]), [yes], [^(yes|no)$])
])

# AX_FEATURE_ARG_N(FEATURE, [DESC])
AC_DEFUN([AX_FEATURE_ARG_N], [
  AX_FEATURE_ARG_X([$1], m4_default([$2], [$1]), [no], [^(yes|no)$])
])

# AX_FEATURE_ARG_X(FEATURE, DESC, DEFAULT-VAL, VAL-REGEXP, [ACTION])
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
    [
      # clear the predefined variable set by conf=CONF
      AX_FEATURE_PREFIX_VAR[]AS_TR_SH([$1])=""
    ],
    [
      [enable_]AS_TR_SH([$1])='AS_TR_SH([$3])'
    ])
  AX_FEATURE_VAR_X([$1], [$2], [${enable_]AS_TR_SH([$1])[}], [$4])
  [$5]
])

# AX_FEATURE_DEFINE(FEATURE, [AC_DEF_SYM], [AM_COND_SYM], [DESC])
AC_DEFUN([AX_FEATURE_DEFINE], [
  case $ax_feature_state in
    seed|dep_resolv|weakdep_resolv|conflict_resolv|define)
      ax_feature_state='define'
      ;;
    *)
      AC_MSG_ERROR([invalid macros layout for ax_feature_configurator.m4])
      ;;
  esac

  m4_define_default(AX_FEATURE_DESC_[]AS_TR_CPP([$1]),
                    [$1])
  if test "x${AX_FEATURE_PREFIX_VAR[][$1]:=no}" != xno; then
    AC_DEFINE(m4_default([$2], AX_FEATURE_PREFIX_DEF[]m4_toupper([$1])), 1,
              m4_default([$4], AX_FEATURE_DESC_[]AS_TR_CPP([$1])))
  fi
  AM_CONDITIONAL(m4_default([$3], AX_FEATURE_PREFIX_COND[]m4_toupper([$1])),
                 test "x${AX_FEATURE_PREFIX_VAR[$1]}" != xno)
])

# _AX_FEATURE_OVERRIDE_VALS(DEPENDENT-VAL, FEATURE-LIST)
AC_DEFUN([_AX_FEATURE_OVERRIDE_VALS], [
    for feature in [$2]; do
      _ax_feature_var="AX_FEATURE_PREFIX_VAR[]$feature"
      _ax_feature_val=AS_VAR_GET([$_ax_feature_var])
  
      if test -z "$_ax_feature_val"; then
        ax_feature_list_all="$ax_feature_list_all $feature"
      fi
      if test -z "$_ax_feature_val" -o "x$_ax_feature_val" = xno; then
        AS_VAR_SET([${_ax_feature_var}], [$1])
        AS_VAR_SET([${_ax_feature_var}_][$1], [yes])
        AS_VAR_SET([${_ax_feature_var}_no], [""])
        ax_feature_list_implicit="$ax_feature_list_implicit $feature"
      fi
   done
])

# AX_FEATURE_RESOLVE_DEPENDENCIES(GROUP, RULESET, [DEPENDENT-VAL])
AC_DEFUN([AX_FEATURE_RESOLVE_DEPENDENCIES], [
  case $ax_feature_state in
    seed|dep_resolv)
      ax_feature_state='dep_resolv'
      ;;
    *)
      AC_MSG_ERROR([invalid macros layout for ax_feature_configurator.m4])
      ;;
  esac

  AC_MSG_CHECKING([$1][ dependencies of features])
  _ax_feature_resolve_deps () {
    # FIXME: improper temporary directory handlings
    rm -rf confdeps
    mkdir confdeps
    # FIXME: GNU make dependency
    $GMAKE -s -C confdeps -f - @S|@@ __deps <<'EOT'
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
  case $ax_feature_state in
    seed|dep_resolv|weakdep_resolv)
      ax_feature_state='weakdep_resolv'
      ;;
    *)
      AC_MSG_ERROR([invalid macros layout for ax_feature_configurator.m4])
      ;;
  esac

  AX_FEATURE_RESOLVE_DEPENDENCIES([weak], [$1], [weakyes])
])

# FIXME: support weak dependency
# FIXME: support IF-CONFLICT handling
# AX_FEATURE_DETECT_CONFLICTS(RULESET, [IF-CONFLICT])
AC_DEFUN([AX_FEATURE_DETECT_CONFLICTS], [
  case $ax_feature_state in
    seed|dep_resolv|weakdep_resolv|conflict_resolv)
      ax_feature_state='conflict_resolv'
      ;;
    *)
      AC_MSG_ERROR([invalid macros layout for ax_feature_configurator.m4])
      ;;
  esac

  AC_MSG_CHECKING([conflicts between features])
  _ax_feature_list_expanded=''
  for feature in $ax_feature_list_all; do
    _ax_feature_val=AS_VAR_GET(AX_FEATURE_PREFIX_VAR[]$feature)
    _ax_feature_val=${_ax_feature_val:no}
    if test "x$_ax_feature_val" != xno; then
      _ax_feature_list_expanded="$_ax_feature_list_expanded $feature"
    fi
   _ax_feature_list_expanded="$_ax_feature_list_expanded ${feature}_${_ax_feature_val}"
  done

  _ax_feature_per_line_expanded () {
    for feature in $_ax_feature_list_expanded; do
      echo $feature;
    done
  }

  _ax_feature_conflict_regexps () {
    cat <<'EOT' | sed 's/ /|/g' | while read mutexes; do test -n "$mutexes" && echo "\\\\b(${mutexes})\\\\b"; done
[$1]
EOT
  }

  _ax_feature_check_conflict () {
    local err err_re conflicts

    err=0
    while read re; do
      if test 1 -lt `_ax_feature_per_line_expanded | $EGREP -c "$re"`; then
        err_re=`echo $re | sed 's/\\\\b//g'`
        conflicts=`_ax_feature_per_line_expanded | $EGREP "$re" | xargs echo`
      
        if test $err -eq 0; then
          AC_MSG_RESULT([detected])
        fi
        # Since this code is being executed in a piped subprocess,
        # AC_MSG_ERROR() does not terminate the configure process.
        AC_MSG_WARN([these features must exclusively be chosen from ][${err_re}: ${conflicts}])
        err=1
      fi
    done
    return $err
  }
  if (_ax_feature_conflict_regexps | _ax_feature_check_conflict); then
    AC_MSG_RESULT([ok])
  else
    AC_MSG_ERROR([feature conflict(s) between mutually exclusive ones have been detected])
  fi
])
