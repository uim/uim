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

require "stringio"
require "test/unit"
require_relative "../generate-table"

class GenerateTableTest < Test::Unit::TestCase
  def render(table)
    output = StringIO.new
    table.write(output)
    output.string
  end

  def test_wb86_normalizes_codes_markers_and_duplicates
    source = StringIO.new(<<~TABLE)
      ba\t吧\t20
      AAWI\t𤁱.\t100
      aawi\t𤁱.\t99
      aawi\t勤工俭学\t98
      aa\ta phrase.\t-1
    TABLE

    assert_equal(<<~TABLE, render(UimTableGenerator.parse_wb86(source)))
      aa ("a phrase.")
      aawi ("𤁱" "勤工俭学")
      ba ("吧")
    TABLE
  end

  def test_zm_ignores_header_and_phrase_construction_metadata
    source = StringIO.new(<<~TABLE)
      ;fcitx Version 0x03 Table file
      KeyCode=abcdefghijklmnopqrstuvwxyz
      [Data]
      b 不
      aa 一下
      aa 一下
      aa 天下 无敌
      ^aa 一
    TABLE

    assert_equal(<<~TABLE, render(UimTableGenerator.parse_zm(source)))
      aa ("一下" "天下 无敌")
      b ("不")
    TABLE
  end

  def test_scheme_string_escaping
    table = UimTableGenerator::Table.new
    table.add("a", "quote: \" slash: \\")

    assert_equal(%Q{a ("quote: \\" slash: \\\")\n}, render(table))
  end

  def test_rejects_malformed_wb86_line
    error = assert_raise(UimTableGenerator::ParseError) do
      UimTableGenerator.parse_wb86(StringIO.new("a only-two-fields\n"), "wb")
    end
    assert_match(/wb:1:/, error.message)
  end

  def test_rejects_zm_without_data_section
    assert_raise(UimTableGenerator::ParseError) do
      UimTableGenerator.parse_zm(StringIO.new("a 一\n"), "zm")
    end
  end
end
