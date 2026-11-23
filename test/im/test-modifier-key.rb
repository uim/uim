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

require_relative "im-driver"

# The input method for this test commits the name of a modifier key
# that its key handlers get. It doesn't pass the key to the application
# by itself.
class ModifierKeyTest < Test::Unit::TestCase
  def setup
    @driver = IMDriver.new("modifier-key-test", <<~SCHEME)
      (define (modifier-key-test-init-handler id im arg)
        (context-new id im))
      (define (modifier-key-test-key-press-handler c key state)
        (if (modifier-key? key state)
            (im-commit c (symbol->string key))
            (im-commit-raw c)))
      (define (modifier-key-test-key-release-handler c key state)
        (if (modifier-key? key state)
            (im-commit c (string-append "release " (symbol->string key)))
            (im-commit-raw c)))
      (set! enabled-im-list (cons 'modifier-key-test enabled-im-list))
      (register-im
       'modifier-key-test
       "*"
       "UTF-8"
       "modifier key"
       "Commits the name of a modifier key"
       #f
       modifier-key-test-init-handler
       #f
       context-mode-handler
       modifier-key-test-key-press-handler
       modifier-key-test-key-release-handler
       #f
       #f
       #f
       context-prop-activate-handler
       #f
       #f
       #f
       #f
       #f)
      #{scheme}
    SCHEME
    begin
      @driver.start
      yield
    ensure
      @driver.stop
    end
  end

  def scheme
    ""
  end

  def test_not_received
    @driver.press("shift_l")
    assert_equal([[], ["press", "release"]],
                 [@driver.commits, @driver.passed_keys])
  end

  class ReceiveTest < self
    def scheme
      "(im-receive-modifier-keys! 'modifier-key-test)"
    end

    def test_left
      @driver.press("shift_l")
      assert_equal(["Shift_L_key", "release Shift_L_key"],
                   @driver.commits)
    end

    def test_right
      @driver.press("shift_r")
      assert_equal(["Shift_R_key", "release Shift_R_key"],
                   @driver.commits)
    end

    def test_passed
      @driver.press("shift_l")
      assert_equal(["press", "release"],
                   @driver.passed_keys)
    end
  end
end
