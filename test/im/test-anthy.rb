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
#
# anthy and anthy-utf8 are the same input method except their names and
# actions. So they share these tests.
module AnthyTests
  include IMDriver::PreeditAttribute

  def setup
    omit("#{im} isn't enabled") unless IMDriver.available?(im)
    # libanthy writes what it learns to $XDG_CONFIG_HOME/anthy/. Old
    # libanthy writes it to ~/.anthy/ of the user regardless of $HOME.
    # So tests must not commit a candidate that changes what other tests
    # convert.
    Dir.mktmpdir do |config_home|
      @driver = IMDriver.new(im,
                             "(require-module \"#{im}\")",
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
    @driver.type("henkan")
    @driver.press("space")
    assert_equal([[REVERSE | CURSOR, "変換"]],
                 @driver.preedit)
  end

  def test_next_candidate
    @driver.type("henkan")
    @driver.press("space", "space")
    assert_equal([
                   "変換",
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
    @driver.type("nyuuryoku")
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

  def test_transpose
    [
      "f6",
      "f7",
      "f8",
      "f9",
      "f10",
    ].each do |key|
      @driver.type("aiu")
      @driver.press(key, "return")
    end
    assert_equal(["あいう", "アイウ", "ｱｲｳ", "ａｉｕ", "aiu"],
                 @driver.commits)
  end

  def test_mode
    [
      "S-f7",
      "S-f8",
      "S-f9",
    ].each do |key|
      @driver.press(key)
      @driver.type("kana")
      @driver.press("return")
    end
    assert_equal(["カナ", "ｶﾅ", "ｋａｎａ"],
                 @driver.commits)
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

  def test_input_rule
    [
      ["kana", "ti3"],
      ["azik", "kzkq"],
      ["kzik", "kzkq"],
      ["act", "kanji"],
    ].each do |rule, keys|
      @driver.activate("action_#{im.tr("-", "_")}_#{rule}")
      @driver.type(keys)
      @driver.press("return")
    end
    assert_equal(["かにあ", "かんかい", "かんかい", "うんあねんい"],
                 @driver.commits)
  end
end

class AnthyTest < Test::Unit::TestCase
  include AnthyTests

  def im
    "anthy"
  end
end

class AnthyUTF8Test < Test::Unit::TestCase
  include AnthyTests

  def im
    "anthy-utf8"
  end
end
