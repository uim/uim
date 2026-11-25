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

require "tmpdir"

require_relative "im-driver"

# These use librime and the schemas of Rime that are installed.
class RimeTest < Test::Unit::TestCase
  include IMDriver::PreeditAttribute

  def setup
    omit("rime isn't enabled") unless IMDriver.available?("rime")
    shared_data_dir = ENV["RIME_SHARED_DATA_DIR"]
    omit("RIME_SHARED_DATA_DIR isn't set") if shared_data_dir.nil?
    schema_file = File.join(shared_data_dir, "#{schema}.schema.yaml")
    omit("#{schema} isn't installed") unless File.exist?(schema_file)
    Dir.mktmpdir do |dir|
      @driver = IMDriver.new("rime", <<~SCHEME)
        (require-module "rime")
        (set! rime-shared-data-dir "#{shared_data_dir}")
        (set! rime-user-data-dir "#{dir}")
        (set! rime-schema "#{schema}")
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

  def schema
    "luna_pinyin"
  end

  def test_preedit
    @driver.type("ni")
    assert_equal([[REVERSE, "ni"], [CURSOR, ""]],
                 @driver.preedit)
  end

  def test_candidates
    @driver.type("ni")
    assert_equal("你", @driver.candidates[0])
  end

  def test_candidate_index
    @driver.type("ni")
    assert_equal(0, @driver.candidate_index)
  end

  def test_next_candidate
    @driver.type("ni")
    @driver.press("down")
    assert_equal(1, @driver.candidate_index)
  end

  def test_next_page
    @driver.type("ni")
    first_page = @driver.candidates
    @driver.press("pagedown")
    assert_not_equal(first_page, @driver.candidates)
  end

  def test_commit
    @driver.type("ni")
    @driver.press("space")
    assert_equal(["你"], @driver.commits)
  end

  def test_commit_clears_preedit
    @driver.type("ni")
    @driver.press("space")
    assert_equal([], @driver.preedit)
  end

  def test_commit_hides_candidates
    @driver.type("ni")
    @driver.press("space")
    assert_nil(@driver.candidates)
  end

  def test_select_candidate
    @driver.type("ni")
    second = @driver.candidates[1]
    @driver.highlight(1)
    assert_equal([second], @driver.commits)
  end

  def test_cancel
    @driver.type("ni")
    @driver.press("escape")
    assert_equal([], @driver.preedit)
  end

  # What is composed is committed as librime converts it now.
  def test_focus_out
    @driver.type("ni")
    @driver.focus_out
    assert_equal(["你"], @driver.commits)
  end

  def test_off
    @driver.press("S-space")
    @driver.type("ni")
    assert_equal([], @driver.preedit)
  end

  # Left Shift is "inline_ascii" by default: what is typed after it
  # is added as ASCII while composing.
  def test_left_shift
    @driver.type("ni")
    @driver.press("shift_l")
    @driver.type("ab")
    assert_equal([[REVERSE, "niab"], [CURSOR, ""]],
                 @driver.preedit)
  end

  # It switches to the ASCII mode while not composing.
  def test_left_shift_ascii_mode
    @driver.press("shift_l")
    @driver.type("ni")
    assert_equal([], @driver.preedit)
  end

  def test_left_shift_twice
    @driver.press("shift_l")
    @driver.press("shift_l")
    @driver.type("ni")
    assert_equal([[REVERSE, "ni"], [CURSOR, ""]],
                 @driver.preedit)
  end

  # Shift from a bridge that doesn't tell the side is Left Shift.
  def test_shift_without_side
    @driver.type("ni")
    @driver.press("shift")
    @driver.type("ab")
    assert_equal([[REVERSE, "niab"], [CURSOR, ""]],
                 @driver.preedit)
  end

  # What the toolbar shows as the input mode.
  def input_mode
    @driver.modes[1]
  end

  # What the toolbar shows as the schema.
  def schema_indication
    @driver.modes[2]
  end

  def omit_unless_installed(schema)
    schema_file = File.join(ENV["RIME_SHARED_DATA_DIR"],
                            "#{schema}.schema.yaml")
    omit("#{schema} isn't installed") unless File.exist?(schema_file)
  end

  def test_schema_indication
    assert_equal("rime_schema_luna_pinyin", schema_indication)
  end

  # librime reports the schema of its menu while it's shown.
  def test_schema_indication_menu
    @driver.press("f4")
    assert_equal("rime_schema_luna_pinyin", schema_indication)
  end

  def test_action_schema
    omit_unless_installed("terra_pinyin")
    @driver.activate("action_rime_schema_terra_pinyin")
    @driver.type("pi\\")
    assert_equal([[REVERSE, "pì"], [CURSOR, ""]],
                 @driver.preedit)
  end

  def test_action_schema_indication
    omit_unless_installed("terra_pinyin")
    @driver.activate("action_rime_schema_terra_pinyin")
    assert_equal("rime_schema_terra_pinyin", schema_indication)
  end

  # What is composed is committed as turning Rime off does.
  def test_action_schema_composing
    omit_unless_installed("terra_pinyin")
    @driver.type("ni")
    @driver.activate("action_rime_schema_terra_pinyin")
    assert_equal([["你"], []],
                 [@driver.commits, @driver.preedit])
  end

  # Choosing the schema that is used now does nothing.
  def test_action_schema_same
    @driver.type("ni")
    @driver.activate("action_rime_schema_luna_pinyin")
    assert_equal([[], [[REVERSE, "ni"], [CURSOR, ""]]],
                 [@driver.commits, @driver.preedit])
  end

  # It also turns Rime on.
  def test_action_schema_off
    @driver.activate("action_rime_off")
    @driver.activate("action_rime_schema_luna_pinyin")
    assert_equal("on", input_mode)
  end

  def test_custom_schema
    omit_unless_installed("terra_pinyin")
    @driver.set_custom("rime-schema", "\"terra_pinyin\"")
    @driver.type("pi\\")
    assert_equal([[REVERSE, "pì"], [CURSOR, ""]],
                 @driver.preedit)
  end

  def test_custom_schema_indication
    omit_unless_installed("terra_pinyin")
    @driver.set_custom("rime-schema", "\"terra_pinyin\"")
    assert_equal("rime_schema_terra_pinyin", schema_indication)
  end

  # What is composed is committed as choosing a schema in the toolbar
  # does.
  def test_custom_schema_composing
    omit_unless_installed("terra_pinyin")
    @driver.type("ni")
    @driver.set_custom("rime-schema", "\"terra_pinyin\"")
    assert_equal([["你"], []],
                 [@driver.commits, @driver.preedit])
  end

  # uim-pref makes the hooks of all customs called even if they aren't
  # changed. The schema chosen in the toolbar is kept then.
  def test_custom_schema_same
    omit_unless_installed("terra_pinyin")
    @driver.activate("action_rime_schema_terra_pinyin")
    @driver.set_custom("rime-schema", "\"luna_pinyin\"")
    assert_equal("rime_schema_terra_pinyin", schema_indication)
  end

  # Rime isn't turned on unlike choosing a schema in the toolbar.
  def test_custom_schema_off
    omit_unless_installed("terra_pinyin")
    @driver.activate("action_rime_off")
    @driver.set_custom("rime-schema", "\"terra_pinyin\"")
    assert_equal(["off", "rime_schema_terra_pinyin"],
                 [input_mode, schema_indication])
  end

  # librime chooses the schema when it's empty. The schema used now is
  # kept.
  def test_custom_schema_empty
    omit_unless_installed("terra_pinyin")
    @driver.activate("action_rime_schema_terra_pinyin")
    @driver.set_custom("rime-schema", "\"\"")
    assert_equal("rime_schema_terra_pinyin", schema_indication)
  end

  def test_input_mode
    assert_equal("on", input_mode)
  end

  def test_input_mode_ascii
    @driver.press("shift_l")
    assert_equal("ascii", input_mode)
  end

  def test_input_mode_off
    @driver.activate("action_rime_off")
    assert_equal("off", input_mode)
  end

  def test_action_ascii
    @driver.activate("action_rime_ascii")
    @driver.type("ni")
    assert_equal([], @driver.preedit)
  end

  def test_action_ascii_input_mode
    @driver.activate("action_rime_ascii")
    assert_equal("ascii", input_mode)
  end

  # It leaves the ASCII mode.
  def test_action_on
    @driver.activate("action_rime_ascii")
    @driver.activate("action_rime_on")
    @driver.type("ni")
    assert_equal([[REVERSE, "ni"], [CURSOR, ""]],
                 @driver.preedit)
  end

  # Right Shift is "commit_text" by default: what is composed is
  # committed as it's converted.
  def test_right_shift
    @driver.type("ni")
    @driver.press("shift_r")
    assert_equal(["你"], @driver.commits)
  end

  # The menu of Rime to switch schemas and options.
  def test_menu
    @driver.press("f4")
    assert_equal("朙月拼音", @driver.candidates[0])
  end

  class TerraPinyinTest < self
    def schema
      "terra_pinyin"
    end

    def test_tone
      @driver.type("pi\\")
      assert_equal([[REVERSE, "pì"], [CURSOR, ""]],
                   @driver.preedit)
    end

    def test_tone_candidates
      @driver.type("pi\\")
      assert_equal("屁", @driver.candidates[0])
    end
  end
end
