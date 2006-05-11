#!/bin/sh
# This thing may be better off incorporated in Makefile.am.

[ "x$2" = "x" ] || { echo >&2 "usage: collect.sh <c source>"; exit 1; }

echo "#line 1 \"$1\""

decls=`grep -n '^TST_CASE[[:space:]]*(' "$1" | \
    sed 's/^\([0-9]*:\)TST_CASE[[:space:]]*(/\1/'`
have_id=`echo "$decls" | sed -n '
    /^\([0-9]*\):\([_[:alnum:]]*\),.*$/ {
        s||\1 { p; s/^.*$/\2/; H; b skip_print; };|
        p
    }
    '`
need_id=`echo "$decls" | \
    grep -n '^[0-9]*:[[:space:]]*"' | \
    sed \
    's|^\([0-9]*\):\([0-9]*\):.*$|\2 { s/(/(tst_\1, /; p; s/^.*$/tst_\1/; H; b skip_print; };|'`

sed -n "$have_id""$need_id;p;"'
    : skip_print
    $ { g
        s/\n\([[:alnum:]_]*\)/\n    TST_REGISTER(\1)/g
        s/^/TST_LIST_BEGIN()/
        p
        i TST_LIST_END()
    }
    ' "$1"
