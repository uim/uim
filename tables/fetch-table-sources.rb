#!/usr/bin/env ruby
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

require "digest"
require "fileutils"
require "net/http"
require "uri"

Source = Struct.new(:name, :url, :sha256)

SOURCES = [
  Source.new(
    "zm/zhengma.txt",
    "https://raw.githubusercontent.com/fcitx/fcitx5-table-extra/" \
      "dbc7154a7f0b9fc04313160ae8066ed4d8cbc446/tables/zhengma.txt",
    "9df93a2b8f0716d18827c2fb83a51a72cc9e181e8aecfe3160bcbc5183ef0608"
  ),
  Source.new(
    "zm/README",
    "https://raw.githubusercontent.com/fcitx/fcitx5-table-extra/" \
      "dbc7154a7f0b9fc04313160ae8066ed4d8cbc446/README",
    "547cdca96b767bd4c5eced3c53f812602e0be74bddf4569e122fc1873f66ca40"
  ),
  Source.new(
    "wb86/wubi-haifeng86.txt",
    "https://raw.githubusercontent.com/mike-fabian/ibus-table-chinese/" \
      "44301450e681c23d60301747856c74b5b9d1312e/" \
      "tables/wubi-haifeng/wubi-haifeng86.UTF-8",
    "0c94eae894741086aa7349fdbb3c258d30e9c6ac65a959f1993808d2a0db7e8b"
  ),
  Source.new(
    "wb86/COPYING",
    "https://raw.githubusercontent.com/mike-fabian/ibus-table-chinese/" \
      "44301450e681c23d60301747856c74b5b9d1312e/" \
      "tables/wubi-haifeng/COPYING",
    "a37e41f7092e7669123fbe010b30d502ad89306e120ecc23a77b3a89f7502bf4"
  ),
  Source.new(
    "wb86/README",
    "https://raw.githubusercontent.com/mike-fabian/ibus-table-chinese/" \
      "44301450e681c23d60301747856c74b5b9d1312e/" \
      "tables/wubi-haifeng/README",
    "c66b92734a0aa1152f9e2349cca1b90f8286044cbe1c90e0ee7826c7a1cd794f"
  )
].freeze

def fetch(uri, redirects = 5)
  raise "too many redirects for #{uri}" if redirects.zero?

  response = Net::HTTP.get_response(uri)
  case response
  when Net::HTTPSuccess
    response.body
  when Net::HTTPRedirection
    location = response["location"] or raise "redirect without location: #{uri}"
    fetch(URI.join(uri, location), redirects - 1)
  else
    raise "failed to fetch #{uri}: #{response.code} #{response.message}"
  end
end

destination = File.join(__dir__, "source")
FileUtils.mkdir_p(destination)

SOURCES.each do |source|
  contents = fetch(URI(source.url))
  actual = Digest::SHA256.hexdigest(contents)
  unless actual == source.sha256
    raise "SHA-256 mismatch for #{source.name}: #{actual}"
  end

  path = File.join(destination, source.name)
  FileUtils.mkdir_p(File.dirname(path))
  temporary = "#{path}.new"
  File.binwrite(temporary, contents)
  File.rename(temporary, path)
  warn "fetched #{source.name}"
end
