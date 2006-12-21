#!/bin/sh

SUBDIRS='uim replace helper gtk xim fep emacs examples'

for subdir in $SUBDIRS; do
    find $subdir \( -name '*.c' -or -name '*.h' -or -name '*.cpp' \) \
      -exec perl -i -pe 'next if (/===|DECLARE_|_BODY/); s/\b(if|else|do|while|for|switch|FOR_EACH|FOR_EACH_BUTLAST|FOR_EACH_PAIR|FOR_EACH_WHILE)\s*([{\(])/\1 \2/g; s/\)(|\s{2,}){/) {/g; s/(})\s*(if|else)/\1 \2/g; s/\s*([=!<>\+\-\*\/%&^|]=)\s*/ \1 /g; s/\s*\b([=?\+^\|])\b\s*/ \1 /g; s/\s*\b(&&|\|\||<<|>>|<<=|>>=)\b\s*/ \1 /g; s/([\(\[])\s+/\1/g; s/\s+([\)\]])/\1/g; if (!/Copyright|^\s*\/\*.+/*/\\s*$/) { s/\s+([,;])/\1/g; s/([,;])([^;\)\s])/\1 \2/g }' {} \;

# special operators: needs manual fixup
#    find $subdir \( -name '*.c' -or -name '*.h' -or -name '*.cpp' \) \
#      -exec perl -i -pe 'next if (/#include|Copyright|Project|and\/or|DECLARE_|(FileName|About)\s+:|\w\.[ch]: /); s/\s*\b([\*\-\/%:<>])\b\s*/ \1 /g' {} \;

# normalize function invocation: needs manual fixup
#    find $subdir \( -name '*.c' -or -name '*.h' -or -name '*.cpp' \) \
#      -exec perl -i -pe 'next if (/Copyright|DAMAGES|OR TORT|(FileName|About)\s+:/); s/(\w+)\s+\(/\1(/g; s/\b(if|else|do|while|for|switch|FOR_EACH|FOR_EACH_BUTLAST|FOR_EACH_PAIR|FOR_EACH_WHILE)\s*([{\(])/\1 \2/g' {} \;
done
