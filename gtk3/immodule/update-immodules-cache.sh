#!/bin/sh
#
# Usage: update-immodules-cache.sh QUERY_IMMODULES [GTK_PATH]
#
# Staged installs such as packaging are skipped because the cache is
# for the system that the files are installed to.

set -eu

if [ -n "${DESTDIR:-}" ]; then
  exit 0
fi

if ! command -v "$1" > /dev/null 2>&1; then
  echo "Warning: $1 isn't found. Update immodules.cache manually or" \
       "set GTK_IM_MODULE_FILE to use the uim immodule." >&2
  exit 0
fi

if [ -n "${2:-}" ]; then
  GTK_PATH="$2"
  export GTK_PATH
fi
exec "$1" --update-cache
