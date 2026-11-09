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
require "tmpdir"

require_relative "im-driver"

module SKKFixture
  DICTIONARY_TEXT = <<~DICTIONARY
    ;; okuri-ari entries.
    ;; okuri-nasi entries.
    #がつ /月/
    あか /赤/
    あと /後/
    あんぜん /安全/
    か /蚊/
    かな /仮名/
    かんじ /漢字/感じ/幹事/
    きせつ /季節/
    さんか /参加/
    さんかん /参観/
    さんこう /参考/
    さんせい /賛成/
    ちゃわん /茶碗/
  DICTIONARY
end

# Stands in for skkserv. It answers lookups ("1") and completions ("4")
# from SKKFixture::DICTIONARY_TEXT. skkserv uses EUC-JP.
class FakeSKKServer
  DICTIONARY = SKKFixture::DICTIONARY_TEXT.lines.filter_map do |line|
    next if line.start_with?(";")
    reading, candidates = line.chomp.split(" /", 2)
    [reading, candidates.delete_suffix("/").split("/")]
  end.to_h

  def start
    @server = TCPServer.new("127.0.0.1", 0)
    @thread = Thread.new do
      loop do
        socket = @server.accept
        Thread.new(socket) {|client| serve(client)}
      end
    end
  end

  def port
    @server.addr[1]
  end

  def stop
    @thread.kill
    @server.close
  end

  private
  def serve(socket)
    socket.each_line do |line|
      request = line.force_encoding("EUC-JP").encode("UTF-8").strip
      socket.write(respond(request).encode("EUC-JP"))
    end
  ensure
    socket.close
  end

  def respond(request)
    command = request[0]
    key = request[1..]
    if command == "1" && DICTIONARY.key?(key)
      "1/#{DICTIONARY[key].join("/")}/\n"
    elsif command == "4"
      matches = DICTIONARY.keys.select {|reading| reading.start_with?(key)}
      matches.empty? ? "4\n" : "1/#{matches.join("/")}/\n"
    else
      "4\n"
    end
  end
end

class SKKTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    omit("skk isn't enabled") unless IMDriver.available?("skk")
    # It's used only when skkserv_scheme is used.
    @server = FakeSKKServer.new
    @server.start
    begin
      Dir.mktmpdir do |dir|
        dictionary = File.join(dir, "SKK-JISYO")
        # SKK dictionaries are in EUC-JP by default.
        File.write(dictionary, SKKFixture::DICTIONARY_TEXT.encode("EUC-JP"))
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
    ensure
      @server.stop
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

  # ~/.uim for uim 1.9.9 or earlier may change skk-ja-rk-rule. It's
  # ignored.
  sub_test_case("~/.uim: skk-ja-rk-rule") do
    def scheme
      <<~SCHEME
        (define skk-ja-rk-rule
          (append '(((("x" "y" "z")) ("きゅー" "キュー" "ｷｭｰ")))
                  skk-ja-rk-rule))
      SCHEME
    end

    def test_input
      @driver.type("kyaxyz")
      assert_equal(["きゃ"], @driver.commits)
    end
  end

  sub_test_case("C-i") do
    def test_only_pending_romaji
      commit_kana("kanji")
      @driver.press("K")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "かんじ"]],
                   preedit)
    end

    def test_only_pending_romaji_exact_match
      commit_kana("ka")
      @driver.type("K")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"]],
                   preedit)
    end

    def test_recently_used
      commit_kana("kanji")
      commit_kana("ka")
      @driver.press("K")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"]],
                   preedit)
    end

    def test_recently_used_next
      commit_kana("kanji")
      commit_kana("ka")
      @driver.press("K")
      @driver.press("C-i", "C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "かんじ"]],
                   preedit)
    end

    def test_kana_head_excludes_head
      commit_kana("kanji")
      commit_kana("ka")
      @driver.type("Ka")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "かんじ"]],
                   preedit)
    end

    def test_pending_k
      commit_kana("sankou")
      commit_kana("sanka")
      @driver.type("Sank")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんか"]],
                   preedit)
    end

    def test_pending_k_next
      commit_kana("sankou")
      commit_kana("sanka")
      @driver.type("Sank")
      @driver.press("C-i", "C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんこう"]],
                   preedit)
    end

    def test_c_of_cha
      commit_kana("chawan")
      @driver.type("C")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "ちゃわん"]],
                   preedit)
    end

    def test_no_candidate
      commit_kana("sankou")
      @driver.type("Sans")
      @driver.press("C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さん"], [UNDERLINE, "s"]],
                   preedit)
    end

    def test_cancel_no_candidate
      commit_kana("sankou")
      @driver.type("Sans")
      @driver.press("C-i")
      @driver.press("C-g")
      assert_equal([], preedit)
    end

    sub_test_case("skk-use-numeric-conversion?: #t") do
      def scheme
        super + <<~SCHEME
          (set! skk-use-numeric-conversion? #t)
        SCHEME
      end

      def test_number_head
        commit_kana("1gatsu")
        @driver.press("S-q")
        @driver.type("1g")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "1がつ"]],
                     preedit)
      end

      def test_number_head_after_cancel
        commit_kana("1gatsu")
        @driver.press("S-q")
        @driver.type("1g")
        @driver.press("C-i", "C-g", "C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "1がつ"]],
                     preedit)
      end
    end

    sub_test_case("skk-completion-use-pending-romaji?: #f") do
      def scheme
        super + <<~SCHEME
          (set! skk-completion-use-pending-romaji? #f)
        SCHEME
      end

      def test_only_pending_romaji
        commit_kana("sanka")
        commit_kana("sansei")
        @driver.type("S")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "s"]],
                     preedit)
      end

      def test_cancel_only_pending_romaji
        commit_kana("sanka")
        commit_kana("sansei")
        @driver.type("S")
        @driver.press("C-i")
        @driver.press("C-g")
        assert_equal([], preedit)
      end

      def test_kana_head
        commit_kana("sanka")
        commit_kana("sansei")
        @driver.type("Sa")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんせい"]],
                     preedit)
      end

      def test_pending_k
        commit_kana("sanka")
        commit_kana("sansei")
        @driver.type("Sank")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんせい"]],
                     preedit)
      end

      def test_pending_n
        commit_kana("kanji")
        commit_kana("kana")
        @driver.type("Kan")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "かんじ"]],
                     preedit)
      end

      def test_cancel_pending_n
        commit_kana("kanji")
        commit_kana("kana")
        @driver.type("Kan")
        @driver.press("C-i")
        @driver.press("C-g")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"], [UNDERLINE, "n"]],
                     preedit)
      end

      def test_pending_n_no_candidate
        @driver.type("Kan")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"], [UNDERLINE, "n"]],
                     preedit)
      end

      def test_cancel_pending_n_no_candidate
        @driver.type("Kan")
        @driver.press("C-i")
        @driver.press("C-g")
        assert_equal([], preedit)
      end

      def test_pending_n_candidate_only_for_head
        commit_kana("kana")
        @driver.type("Kan")
        @driver.press("C-i")
        assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"], [UNDERLINE, "n"]],
                     preedit)
      end
    end

    sub_test_case("skk-use-skkserv?: #t") do
      def scheme
        super + skkserv_scheme
      end

      sub_test_case("skk-skkserv-enable-completion?: #t") do
        def scheme
          super + <<~SCHEME
            (set! skk-skkserv-enable-completion? #t)
          SCHEME
        end

        def test_only_pending_romaji
          @driver.type("S")
          @driver.press("C-i")
          assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんか"]],
                       preedit)
        end
      end

      sub_test_case("skk-skkserv-enable-completion?: #f") do
        def scheme
          super + <<~SCHEME
            (set! skk-skkserv-enable-completion? #f)
          SCHEME
        end

        def test_pending_n
          @driver.type("Kan")
          @driver.press("C-i")
          assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"], [UNDERLINE, "n"]],
                       preedit)
        end
      end
    end
  end

  sub_test_case("C-A-i") do
    def test_restart_from_current_candidate
      commit_kana("sankan")
      commit_kana("sankou")
      commit_kana("sanka")
      @driver.type("Sank")
      @driver.press("C-i", "C-A-i", "C-i")
      assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "さんかん"]],
                   preedit)
    end
  end

  sub_test_case("skk-dcomp-activate?: #t") do
    def scheme
      super + <<~SCHEME
        (set! skk-dcomp-activate? #t)
      SCHEME
    end

    sub_test_case("dynamic completion") do
      def test_only_pending_romaji
        commit_kana("kanji")
        @driver.press("K")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "k"],
                       [UNDERLINE, "かんじ"],
                     ],
                     preedit)
      end

      def test_only_pending_romaji_after_shift_q
        commit_kana("kanji")
        @driver.press("S-q")
        @driver.type("k")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "k"],
                       [UNDERLINE, "かんじ"],
                     ],
                     preedit)
      end

      def test_recently_used_in_another_row
        commit_kana("kanji")
        commit_kana("kisetsu")
        @driver.press("K")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "k"],
                       [UNDERLINE, "きせつ"],
                     ],
                     preedit)
      end

      def test_c_of_cha
        commit_kana("chawan")
        @driver.type("C")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "c"],
                       [UNDERLINE, "ちゃわん"],
                     ],
                     preedit)
      end

      def test_pending_k_exact_match
        commit_kana("sanka")
        @driver.type("Sank")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "さん"],
                       [UNDERLINE, "k"],
                       [UNDERLINE, "か"],
                     ],
                     preedit)
      end

      sub_test_case("skk-completion-use-pending-romaji?: #f") do
        def scheme
          super + <<~SCHEME
            (set! skk-completion-use-pending-romaji? #f)
          SCHEME
        end

        def test_only_pending_romaji
          commit_kana("kanji")
          @driver.type("K")
          assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "k"]],
                       preedit)
        end

        def test_kana_head
          commit_kana("kanji")
          @driver.type("Ka")
          assert_equal([
                         [UNDERLINE, "▽"],
                         [UNDERLINE, "か"],
                         [UNDERLINE, "んじ"],
                       ],
                       preedit)
        end

        def test_pending_n
          commit_kana("kanji")
          @driver.type("Kan")
          assert_equal([[UNDERLINE, "▽"], [UNDERLINE, "か"], [UNDERLINE, "n"]],
                       preedit)
        end
      end
    end

    sub_test_case("C-i") do
      def test_cancel
        commit_kana("sanka")
        @driver.type("Sank")
        @driver.press("C-i")
        @driver.press("C-g")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "さん"],
                       [UNDERLINE, "k"],
                       [UNDERLINE, "か"],
                     ],
                     preedit)
      end
    end

    sub_test_case("C-A-i") do
      def test_cancel
        commit_kana("sanka")
        commit_kana("sankan")
        commit_kana("sankou")
        @driver.type("Sa")
        @driver.press("C-i", "C-i", "C-i", "C-A-i")
        @driver.press("C-g")
        assert_equal([
                       [UNDERLINE, "▽"],
                       [UNDERLINE, "さんか"],
                       [UNDERLINE, "ん"],
                     ],
                     preedit)
      end
    end

    sub_test_case("C-A-j") do
      def test_pending_k
        commit_kana("aka")
        commit_kana("ato")
        @driver.type("Ak")
        @driver.press("C-A-j")
        assert_equal([["赤", "後", "赤"], []],
                     [@driver.commits, preedit])
      end

      def test_pending_n
        commit_kana("anzen")
        @driver.type("An")
        @driver.press("C-A-j")
        assert_equal([["安全", "安全"], []], [@driver.commits, preedit])
      end

      def test_only_pending_romaji
        commit_kana("sanka")
        @driver.press("S")
        @driver.press("C-A-j")
        assert_equal([["参加", "参加"], []], [@driver.commits, preedit])
      end

      def test_no_candidate
        @driver.type("Un")
        @driver.press("C-A-j")
        assert_equal([["うん"], []], [@driver.commits, preedit])
      end
    end

    sub_test_case("A-space") do
      def test_pending_k
        commit_kana("aka")
        commit_kana("ato")
        @driver.type("Ak")
        @driver.press("A-space")
        assert_equal([[UNDERLINE, "▼"], [REVERSE | CURSOR, "赤"]],
                     preedit)
      end

      def test_pending_n
        commit_kana("anzen")
        @driver.type("An")
        @driver.press("A-space")
        assert_equal([[UNDERLINE, "▼"], [REVERSE | CURSOR, "安全"]],
                     preedit)
      end

      def test_only_pending_romaji
        commit_kana("sanka")
        @driver.press("S")
        @driver.press("A-space")
        assert_equal([[UNDERLINE, "▼"], [REVERSE | CURSOR, "参加"]],
                     preedit)
      end
    end
  end

  private
  # skkserv replaces the local dictionary, so the candidates come from
  # FakeSKKServer.
  def skkserv_scheme
    <<~SCHEME
      (set! skk-use-skkserv? #t)
      (set! skk-skkserv-use-env? #f)
      (set! skk-skkserv-hostname "127.0.0.1")
      (set! skk-skkserv-portnum #{@server.port})
    SCHEME
  end

  # Completion only uses words already looked up in the dictionary.
  def commit_kana(text)
    @driver.press("S-q")
    @driver.type(text)
    @driver.press("space", "return")
  end

  # skk shows empty segments too, for example for the cursor and the
  # okurigana. They're ignored.
  def preedit
    @driver.preedit.reject {|_attribute, text| text.empty?}
  end
end
