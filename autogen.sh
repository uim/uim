#!/bin/sh

${AUTORECONF:-autoreconf} --force --install "$@" \
  && intltoolize --copy --force --automake
