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

require "tmpdir"

require_relative "im-driver"

# These tests use the real libanthy and its dictionary. They check the
# first candidates only because the other candidates depend on the
# version of the dictionary.
class AnthyUTF8Test < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    omit("anthy-utf8 isn't enabled") unless IMDriver.available?("anthy-utf8")
    # libanthy writes what it learns to $XDG_CONFIG_HOME/anthy/.
    Dir.mktmpdir do |config_home|
      @driver = IMDriver.new("anthy-utf8",
                             "(require-module \"anthy-utf8\")",
                             env: {"XDG_CONFIG_HOME" => config_home})
      begin
        @driver.start
        # Turn it on.
        @driver.press("S-space")
        yield
      ensure
        @driver.stop
      end
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
    assert_equal([[REVERSE | CURSOR, "漢字"]],
                 @driver.preedit)
  end

  def test_next_candidate
    @driver.type("kanji")
    @driver.press("space", "space")
    assert_equal([
                   "漢字",
                   1,
                   [[REVERSE | CURSOR, @driver.candidates[1]]],
                 ],
                 [
                   @driver.candidates[0],
                   @driver.candidate_index,
                   @driver.preedit,
                 ])
  end

  def test_commit
    @driver.type("kanji")
    @driver.press("space", "space")
    candidate = @driver.candidates[1]
    @driver.press("return")
    assert_equal([[candidate], []],
                 [@driver.commits, @driver.preedit])
  end

  def test_segments
    @driver.type("watasinonamaeha")
    @driver.press("space")
    assert_equal([[REVERSE | CURSOR, "私の"], [UNDERLINE, "名前は"]],
                 @driver.preedit)
  end

  def test_shrink_segment
    @driver.type("watasinonamaeha")
    @driver.press("space", "S-left")
    assert_equal([
                   [REVERSE | CURSOR, "私"],
                   [UNDERLINE, "の"],
                   [UNDERLINE, "名前は"],
                 ],
                 @driver.preedit)
  end

  def test_extend_segment
    @driver.type("watasinonamaeha")
    @driver.press("space", "S-right")
    assert_equal([[REVERSE | CURSOR, "私のな"], [UNDERLINE, "前は"]],
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
    @driver.activate("action_anthy_utf8_#{rule}")
    @driver.type(keys)
    @driver.press("return")
    assert_equal([expected], @driver.commits)
  end
end
