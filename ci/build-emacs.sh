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

# Tests uim.el with both build systems.

echo "::group::Autotools: configure"
set -x
mkdir -p autotools
cd autotools
/source/configure \
  --enable-emacs \
  --enable-maintainer-mode \
  --enable-tests \
  --prefix=/tmp/autotools
set +x
echo "::endgroup::"

echo "::group::Autotools: make"
set -x
make > /dev/null
set +x
echo "::endgroup::"

echo "::group::Autotools: test"
set -x
make check
set +x
echo "::endgroup::"
cd ..

echo "::group::Meson: setup"
set -x
meson setup \
  --prefix=/tmp/meson \
  -Demacs=enabled \
  -Dtests=enabled \
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
