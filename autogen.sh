#!/bin/sh

set -e

${AUTORECONF:-autoreconf} --force --install "$@"
cd sigscheme
./autogen.sh "$@"
