#!/bin/sh

# usage:
# $ uim-trace.sh uim-pref-gtk

gdb --quiet $* <uim-trace.gdb | \
egrep '("|uim_scm_eval )' | \
perl -pe 's/^[^"]+str=[^"]+"(.+)"\)?( at uim-scm.c:\d+)?$/\1/'
