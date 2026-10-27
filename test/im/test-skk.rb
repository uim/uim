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

class SKKTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    omit("skk isn't enabled") unless IMDriver.available?("skk")
    Dir.mktmpdir do |dir|
      dictionary = File.join(dir, "SKK-JISYO")
      # SKK dictionaries are in EUC-JP by default.
      File.write(dictionary, <<~DICTIONARY.encode("EUC-JP"))
        ;; okuri-ari entries.
        ;; okuri-nasi entries.
        かんじ /漢字/感じ/幹事/
      DICTIONARY
      # The personal dictionaries are written to DIR.
      @driver = IMDriver.new("skk", <<~SCHEME)
        (require-module "skk")
        (set! skk-dic-file-name "#{dictionary}")
        (set! skk-personal-dic-filename "#{dir}/skk-jisyo")
        (set! skk-uim-personal-dic-filename "#{dir}/skk-uim-jisyo")
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
    end
  end

  # Additional configuration evaluated after skk is loaded.
  def scheme
    ""
  end

  def test_hiragana
    @driver.type("kanji")
    assert_equal(["か", "ん", "じ"], @driver.commits)
  end

  def test_preedit
    @driver.type("Kanji")
    assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "かんじ"]],
                 preedit)
  end

  def test_convert
    @driver.type("Kanji")
    @driver.press("space")
    assert_equal([[UNDERLINE, "▼"], [REVERSE | CURSOR, "漢字"]],
                 preedit)
  end

  def test_next_candidate
    @driver.type("Kanji")
    @driver.press("space", "space")
    assert_equal([[UNDERLINE, "▼"], [REVERSE | CURSOR, "感じ"]],
                 preedit)
  end

  def test_commit
    @driver.type("Kanji")
    @driver.press("space", "space", "return")
    assert_equal([["感じ"], []],
                 [@driver.commits, preedit])
  end

  data("katakana" => ["q", ["カ", "ナ"]],
       "halfwidth katakana" => ["C-q", ["ｶ", "ﾅ"]],
       "wide latin" => ["S-l", ["ｋ", "ａ", "ｎ", "ａ"]])
  def test_mode(data)
    key, expected = data
    @driver.press(key)
    @driver.type("kana")
    assert_equal(expected, @driver.commits)
  end

  data("AZIK" => ["azik", "kzkq", ["かん", "かい"]],
       "ACT" => ["act", "kanji", ["うん", "あ", "ねん", "い"]],
       "KZIK" => ["kzik", "kzkq", ["かん", "かい"]])
  def test_input_rule(data)
    rule, keys, expected = data
    @driver.activate("action_skk_#{rule}")
    @driver.type(keys)
    assert_equal(expected, @driver.commits)
  end

  sub_test_case("custom rule") do
    # uim-pref saves these when the composing rule type is "Custom".
    # They're changed after skk is loaded like uim-pref changes them
    # while uim is running.
    def scheme
      <<~SCHEME
        (custom-set-value! 'ja-rk-rule-type 'custom)
        (custom-set-value! 'ja-rk-rule-table-basic
                           (cons '("xyz" "" "きゅー") ja-rk-rule-table-basic))
      SCHEME
    end

    def test_input
      @driver.type("kyaxyz")
      assert_equal(["きゃ", "きゅー"], @driver.commits)
    end
  end

  # ~/.uim is loaded after skk when lazy loading is disabled.
  sub_test_case("~/.uim: ja-rk-rule-basic") do
    def scheme
      <<~SCHEME
        (set! ja-rk-rule-basic
              (cons '((("x" "y" "z")) ("きゅー" "キュー" "ｷｭｰ"))
                    ja-rk-rule-basic))
        (ja-rk-rule-update)
      SCHEME
    end

    def test_input
      @driver.type("kyaxyz")
      assert_equal(["きゃ", "きゅー"], @driver.commits)
    end
  end

  sub_test_case("~/.uim: skk-ja-rk-rule") do
    def scheme
      <<~SCHEME
        (set! skk-ja-rk-rule
              (cons '((("x" "y" "z")) ("きゅー" "キュー" "ｷｭｰ"))
                    skk-ja-rk-rule))
      SCHEME
    end

    def test_input
      @driver.type("kyaxyz")
      assert_equal(["きゃ", "きゅー"], @driver.commits)
    end
  end

  private
  # skk shows empty segments too, for example for the cursor and the
  # okurigana. They're ignored.
  def preedit
    @driver.preedit.reject {|_attribute, text| text.empty?}
  end
end
