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
require "rbconfig"
require "tmpdir"

require_relative "im-driver"

class KarukanTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  RETURN = 0xff0d
  ESCAPE = 0xff1b

  def setup
    omit("karukan isn't enabled") unless IMDriver.available?("karukan")
    Dir.mktmpdir do |dir|
      @log = File.join(dir, "requests.jsonl")
      # karukan-server-command is a command without arguments.
      server = File.join(dir, "karukan-imserver")
      fake_server = File.join(__dir__, "fake-karukan-imserver.rb")
      File.write(server, <<~SH)
        #!/bin/sh
        exec "#{RbConfig.ruby}" "#{fake_server}"
      SH
      File.chmod(0755, server)
      @driver = IMDriver.new("karukan", <<~SCHEME,
        (require-module "karukan")
        (set! karukan-server-command "#{server}")
        #{custom_scheme}
      SCHEME
                             env: {"FAKE_KARUKAN_IMSERVER_LOG" => @log})
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

  # Overridden to customize karukan.
  def custom_scheme
    ""
  end

  def requests
    File.readlines(@log).collect {|line| JSON.parse(line)}
  end

  def surrounding_texts
    requests.select {|request| request["method"] == "set_surrounding_text"}
            .collect {|request| request["params"]}
  end

  def keysyms
    requests.select {|request| request["method"] == "process_key"}
            .collect {|request| request["params"]["keysym"]}
  end

  def test_preedit
    @driver.type("ab")
    assert_equal([[UNDERLINE, "ab"], [CURSOR, ""]],
                 @driver.preedit)
  end

  def test_commit
    @driver.type("ab")
    @driver.press("return")
    assert_equal(["ab"], @driver.commits)
  end

  def test_commit_clears_preedit
    @driver.type("ab")
    @driver.press("return")
    assert_equal([], @driver.preedit)
  end

  def test_requests
    @driver.type("a")
    assert_equal(["init", "reset", "set_surrounding_text", "process_key"],
                 requests.collect {|request| request["method"]})
  end

  def test_off
    @driver.type("a")
    @driver.press("return", "S-space")
    @driver.type("b")
    assert_equal(["a".ord, RETURN], keysyms)
  end

  def test_focus_out
    @driver.type("ab")
    @driver.focus_out
    assert_equal(["ab"], @driver.commits)
  end

  # "!" makes the fake server exit. The next key finds it and starts it
  # again.
  def test_restart
    @driver.type("a!b")
    assert_equal([[UNDERLINE, "b"], [CURSOR, ""]],
                 @driver.preedit)
  end

  sub_test_case("katakana mode") do
    def test_preedit
      @driver.press("C-k")
      @driver.type("ab")
      assert_equal([[UNDERLINE, "AB"], [CURSOR, ""]],
                   @driver.preedit)
    end

    def test_on
      @driver.press("C-k", "S-space", "S-space")
      @driver.type("ab")
      assert_equal([[UNDERLINE, "ab"], [CURSOR, ""]],
                   @driver.preedit)
    end
  end

  sub_test_case("karukan-use-surrounding-text?: #t") do
    def test_text
      @driver.text_before_cursor = "前の文"
      @driver.type("a")
      assert_equal([{"text" => "前の文", "cursor_pos" => 3}],
                   surrounding_texts)
    end

    # It's sent when a composition starts.
    def test_composing
      @driver.type("ab")
      @driver.press("return")
      @driver.type("c")
      assert_equal(2, surrounding_texts.size)
    end
  end

  sub_test_case("karukan-use-surrounding-text?: #f") do
    def custom_scheme
      "(set! karukan-use-surrounding-text? #f)"
    end

    def test_text
      @driver.text_before_cursor = "前の文"
      @driver.type("a")
      assert_equal([{"text" => "", "cursor_pos" => 0}],
                   surrounding_texts)
    end
  end

  sub_test_case("custom keys") do
    def test_backspace
      @driver.type("ab")
      @driver.press("C-h")
      assert_equal([[UNDERLINE, "a"], [CURSOR, ""]],
                   @driver.preedit)
    end

    def test_cancel
      @driver.type("ab")
      @driver.press("space", "C-g")
      assert_equal([[UNDERLINE, "ab"], [CURSOR, ""]],
                   @driver.preedit)
    end

    def test_next_candidate
      @driver.type("ab")
      @driver.press("space", "C-n", "return")
      assert_equal(["ab"], @driver.commits)
    end

    # Space isn't karukan-next-candidate-key unlike the global one: it
    # has other meanings in Karukan.
    def test_space
      @driver.press("space")
      assert_equal([" ".ord], keysyms)
    end

    # The modifiers are dropped.
    def test_sent_key
      @driver.type("a")
      @driver.press("C-g")
      assert_equal({"keysym" => ESCAPE,
                    "modifiers" => {
                      "shift" => false,
                      "control" => false,
                      "alt" => false,
                      "super" => false,
                    }},
                   requests.last["params"])
    end
  end

  sub_test_case("conversion") do
    def test_preedit
      @driver.type("ab")
      @driver.press("space")
      assert_equal([[REVERSE | CURSOR, "AB"]],
                   @driver.preedit)
    end

    def test_candidates
      @driver.type("ab")
      @driver.press("space")
      assert_equal([["AB", "ab"], 0],
                   [@driver.candidates, @driver.candidate_index])
    end

    def test_next_candidate
      @driver.type("ab")
      @driver.press("space", "space", "return")
      assert_equal(["ab"], @driver.commits)
    end

    def test_next_page
      @driver.type("ab")
      @driver.press("space", "pagedown")
      assert_equal([["Ab"], 0],
                   [@driver.candidates, @driver.candidate_index])
    end

    def test_highlight
      @driver.type("ab")
      @driver.press("space")
      @driver.highlight(1)
      assert_equal([[], [[REVERSE | CURSOR, "ab"]]],
                   [@driver.commits, @driver.preedit])
    end

    def test_commit_highlighted
      @driver.type("ab")
      @driver.press("space")
      @driver.highlight(1)
      @driver.press("return")
      assert_equal(["ab"], @driver.commits)
    end

    def test_focus_out
      @driver.type("ab")
      @driver.press("space")
      @driver.focus_out
      assert_equal([["AB"], [], nil],
                   [@driver.commits, @driver.preedit, @driver.candidates])
    end
  end
end
