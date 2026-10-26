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

require "json"
require "socket"
require "uri"

require_relative "fake-dictionary"
require_relative "im-driver"

# Stands in for the Google CGI API for Japanese Input. uim reaches it as
# an HTTP proxy, so no request leaves the machine.
#
# Like the real one, it converts each comma separated part of the
# reading as a segment, and splits a reading without a comma into
# segments by itself. Every segment has its hiragana and katakana as the
# last candidates.
class FakeGoogleTransliterate
  include FakeDictionary

  # The readings that uim asked for.
  attr_reader :readings

  def initialize
    @readings = []
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
    # GET http://www.google.com:80/transliterate?... HTTP/1.1
    request_target = client.gets.split(" ")[1]
    while (line = client.gets) and line != "\r\n"
    end
    query = request_target.split("?", 2)[1]
    reading = URI.decode_www_form(query).to_h["text"]
    @readings << reading
    body = JSON.generate(convert(reading))
    client.write("HTTP/1.1 200 OK\r\n" +
                 "Content-Type: text/javascript; charset=UTF-8\r\n" +
                 "Content-Length: #{body.bytesize}\r\n" +
                 "\r\n" +
                 body)
  end

  def convert(reading)
    if reading.include?(",")
      segments = reading.split(",")
    else
      segments = split(reading)
    end
    segments.collect do |segment|
      [segment, candidates(segment)]
    end
  end
end

class GoogleCGIAPIJPTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    @server = FakeGoogleTransliterate.new
    @server.start
    begin
      @driver = IMDriver.new("google-cgiapi-jp", <<~SCHEME)
        (set! http-proxy-setting 'user)
        (set! http-proxy-hostname "127.0.0.1")
        (set! http-proxy-port #{@server.port})
        (set! google-cgiapi-jp-use-ssl? #f)
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
    assert_equal([
                   ["かんじへんかん", "かん,じへんかん"],
                   [[REVERSE | CURSOR, "缶"], [UNDERLINE, "事変感"]],
                 ],
                 [
                   @server.readings,
                   @driver.preedit,
                 ])
  end

  def test_extend_segment
    @driver.type("kanjihenkan")
    @driver.press("space", "S-right")
    assert_equal([
                   ["かんじへんかん", "かんじへ,んかん"],
                   [[REVERSE | CURSOR, "かんじへ"], [UNDERLINE, "んかん"]],
                 ],
                 [
                   @server.readings,
                   @driver.preedit,
                 ])
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
    @driver.activate("action_google-cgiapi-jp_#{rule}")
    @driver.type(keys)
    @driver.press("return")
    assert_equal([expected], @driver.commits)
  end
end
