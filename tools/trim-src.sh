#!/bin/sh

SUBDIRS='uim helper gtk xim examples'

for subdir in $SUBDIRS; do
    find $subdir \( -name '*.c' -or -name '*.h' -or -name '*.cpp' \) -exec perl -i -pe 's/\b(if|else|do|while|for|switch)\s*([{\(])/\1 \2/g; s/\)(|\s{2,}){/) {/g; s/(})\s*(if|else)/\1 \2/g' {} \;
done
