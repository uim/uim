#!/bin/sh

set -e

if command -v intltoolize >/dev/null; then
  mkdir -p m4.generated
  intltoolize --copy --force --automake
fi
${AUTORECONF:-autoreconf} --force --install "$@"
