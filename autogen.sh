#!/bin/sh

set -e

${AUTORECONF:-autoreconf} --force --install "$@"
if command -v intltoolize >/dev/null; then
    intltoolize --copy --force --automake
fi
