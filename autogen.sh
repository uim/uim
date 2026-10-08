#!/bin/sh

set -e

${AUTORECONF:-autoreconf} --force --install "$@"
cd subprojects/sigscheme
./autogen.sh "$@"
