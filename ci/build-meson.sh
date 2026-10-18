#!/bin/bash
#
# Copyright (c) 2026 uim Project https://github.com/uim/uim
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. Neither the name of authors nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
# IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
# THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

set -eu

# Usage: build-meson.sh MESON_SETUP_OPTION...
#
# Builds, tests and installs uim with Meson in ./meson.

echo "::group::Meson: setup"
set -x
meson setup \
  --libdir=lib \
  --prefix=/tmp/meson \
  "$@" \
  meson \
  /source
set +x
echo "::endgroup::"

echo "::group::Meson: build"
set -x
meson compile -C meson > /dev/null
set +x
echo "::endgroup::"

echo "::group::Meson: test"
set -x
meson test -C meson --print-errorlogs
set +x
echo "::endgroup::"

echo "::group::Meson: install"
set -x
# Install scripts update system files such as GTK's immodules.cache.
# They load the installed modules that have no RPATH.
sudo env LD_LIBRARY_PATH=/tmp/meson/lib meson install -C meson
set +x
echo "::endgroup::"
