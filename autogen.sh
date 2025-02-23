#!/bin/sh

set -e

${AUTORECONF:-autoreconf} --force --install "$@"
