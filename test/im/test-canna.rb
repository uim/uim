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


require "socket"

require_relative "fake-dictionary"
require_relative "im-driver"

# Stands in for cannaserver. It speaks the subset of the Canna protocol
# version 3 that uim uses.
#
# It splits a reading into the longest words in the dictionary. Every
# segment has its hiragana and katakana as the last candidates. Canna
# uses EUC-JP.
class FakeCannaServer
  include FakeDictionary

  INITIALIZE = 0x01
  CREATE_CONTEXT = 0x03
  CLOSE_CONTEXT = 0x05
  GET_DICTIONARY_LIST = 0x06
  MOUNT_DICTIONARY = 0x08
  BEGIN_CONVERT = 0x0f
  END_CONVERT = 0x10
  GET_CANDIDACY_LIST = 0x11
  GET_YOMI = 0x12
  RESIZE_PAUSE = 0x1a

  # The readings that uim asked for.
  attr_reader :readings

  def initialize
    @readings = []
    @segments = []
  end

  def start
    @server = TCPServer.new("127.0.0.1", 0)
    @thread = Thread.new {serve}
  end

  def port
    @server.addr[1]
  end

  def stop
    @server.close
    @thread.join
  end

  private
  def serve
    loop do
      begin
        client = @server.accept
      rescue IOError
        break
      end
      begin
        respond(client)
      ensure
        client.close
      end
    end
  end

  def respond(client)
    # The first request has a 32-bit operator and a 32-bit length.
    op, length = client.read(8).unpack("NN")
    raise "not initialized: #{op}" unless op == INITIALIZE
    client.read(length)
    client.write([3, 3].pack("nn"))
    while (header = client.read(4))
      op, _, length = header.unpack("CCn")
      body = client.read(length)
      case op
      when CREATE_CONTEXT
        client.write([0, 0].pack("Nn"))
      when CLOSE_CONTEXT, MOUNT_DICTIONARY, END_CONVERT
        client.write([0, 0].pack("NC"))
      when GET_DICTIONARY_LIST
        client.write([0, 1].pack("Nn") + "fake\0".b.ljust(1024, "\0"))
      when BEGIN_CONVERT
        _mode, _context, reading = body.unpack("NnZ*")
        reading = reading.force_encoding("EUC-JP").encode("UTF-8")
        @readings << reading
        @segments = split(reading)
        write_strings(client, first_candidates(@segments))
      when GET_CANDIDACY_LIST
        _context, segment, _size = body.unpack("nnn")
        write_strings(client, candidates(@segments[segment]))
      when GET_YOMI
        _context, segment, _size = body.unpack("nnn")
        write_strings(client, [@segments[segment]])
      when RESIZE_PAUSE
        # uim sends -1 to extend the segment and -2 to shrink it.
        _context, segment, direction = body.unpack("nnn")
        resize(segment, direction == 0xffff ? 1 : -1)
        write_strings(client, first_candidates(@segments[segment..]))
      else
        raise "unknown operator: #{op}"
      end
    end
  end

  def write_strings(client, strings)
    data = strings.collect {|s| s.encode("EUC-JP").b + "\0\0".b}.join
    data << "\0\0".b
    client.write([0, data.bytesize, strings.size].pack("nnn") + data)
  end

  def first_candidates(segments)
    segments.collect {|segment| candidates(segment)[0]}
  end

  # Moves a character between SEGMENT and the rest.
  def resize(segment, delta)
    reading = @segments[segment..].join
    length = @segments[segment].length + delta
    return if length < 1 or length > reading.length
    @segments[segment..] = [reading[0, length], reading[length..]]
    @segments.pop if @segments.last.empty?
  end
end

class CannaTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    omit("canna isn't enabled") unless IMDriver.available?("canna")
    @server = FakeCannaServer.new
    @server.start
    begin
      # Connect to the fake server because cannaserver uses a fixed port.
      @driver = IMDriver.new("canna", <<~SCHEME)
        (require-module "canna")
        (set! canna-lib-open-with-server
              (lambda (server)
                (tcp-connect "127.0.0.1" "#{@server.port}")))
        #{scheme}
      SCHEME
      begin
        @driver.start
        # Turn it on.
        @driver.press("S-space")
        yield
      ensure
        @driver.stop
      end
    ensure
      @server.stop
    end
  end

  # Additional configuration evaluated after canna is loaded.
  def scheme
    ""
  end

  def test_hiragana
    @driver.type("kanji")
    assert_equal([[UNDERLINE, "かんじ"], [CURSOR, ""]],
                 @driver.preedit)
  end

  def test_convert
    @driver.type("kanji")
    @driver.press("space")
    assert_equal([
                   ["かんじ"],
                   [[REVERSE | CURSOR, "漢字"]],
                 ],
                 [
                   @server.readings,
                   @driver.preedit,
                 ])
  end

  def test_next_candidate
    @driver.type("kanji")
    @driver.press("space", "space")
    assert_equal([
                   ["漢字", "感じ", "幹事", "かんじ", "カンジ"],
                   1,
                   [[REVERSE | CURSOR, "感じ"]],
                 ],
                 [
                   @driver.candidates,
                   @driver.candidate_index,
                   @driver.preedit,
                 ])
  end

  def test_commit
    @driver.type("kanji")
    @driver.press("space", "space", "return")
    assert_equal([["感じ"], []],
                 [@driver.commits, @driver.preedit])
  end

  def test_segments
    @driver.type("kanjihenkan")
    @driver.press("space")
    assert_equal([
                   ["かんじへんかん"],
                   [[REVERSE | CURSOR, "漢字"], [UNDERLINE, "変換"]],
                 ],
                 [
                   @server.readings,
                   @driver.preedit,
                 ])
  end

  def test_shrink_segment
    @driver.type("kanjihenkan")
    @driver.press("space", "S-left")
    assert_equal([[REVERSE | CURSOR, "缶"], [UNDERLINE, "事変感"]],
                 @driver.preedit)
  end

  def test_extend_segment
    @driver.type("kanjihenkan")
    @driver.press("space", "S-right")
    assert_equal([[REVERSE | CURSOR, "かんじへ"], [UNDERLINE, "んかん"]],
                 @driver.preedit)
  end

  data("hiragana" => ["f6", "かんじ"],
       "katakana" => ["f7", "カンジ"],
       "halfwidth katakana" => ["f8", "ｶﾝｼﾞ"],
       "fullwidth alphanumeric" => ["f9", "ｋａｎｊｉ"],
       "halfwidth alphanumeric" => ["f10", "kanji"])
  def test_transpose(data)
    key, expected = data
    @driver.type("kanji")
    @driver.press(key, "return")
    assert_equal([expected], @driver.commits)
  end

  def test_commit_as_opposite_kana
    @driver.type("kanji")
    @driver.press("S-q")
    assert_equal(["カンジ"], @driver.commits)
  end

  data("katakana" => ["S-f7", "カナ"],
       "halfwidth katakana" => ["S-f8", "ｶﾅ"],
       "fullwidth alphanumeric" => ["S-f9", "ｋａｎａ"])
  def test_mode(data)
    key, expected = data
    @driver.press(key)
    @driver.type("kana")
    @driver.press("return")
    assert_equal([expected], @driver.commits)
  end

  def test_fullwidth_symbols
    @driver.press("S-f9")
    @driver.type("-~")
    @driver.press("return")
    assert_equal(["－～"], @driver.commits)
  end

  def test_symbols
    @driver.type("vuxtunn,.")
    @driver.press("return")
    assert_equal(["う゛っん、。"], @driver.commits)
  end

  data("kana" => ["kana", "ti3", "かにあ"],
       "AZIK" => ["azik", "kzkq", "かんかい"],
       "KZIK" => ["kzik", "kzkq", "かんかい"],
       "ACT" => ["act", "kanji", "うんあねんい"])
  def test_input_rule(data)
    rule, keys, expected = data
    @driver.activate("action_canna_#{rule}")
    @driver.type(keys)
    @driver.press("return")
    assert_equal([expected], @driver.commits)
  end

  sub_test_case("custom rule") do
    # uim-pref saves these when the composing rule type is "Custom".
    def scheme
      <<~SCHEME
        (custom-set-value! 'ja-rk-rule-type 'custom)
        (custom-set-value! 'ja-rk-rule-table-basic
                           (cons '("q" "" "きゅー") ja-rk-rule-table-basic))
      SCHEME
    end

    def test_input
      @driver.type("kyaq")
      @driver.press("return")
      assert_equal(["きゃきゅー"], @driver.commits)
    end
  end
end
