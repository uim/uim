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

# Runs im-driver with an input method and keeps what the input method
# shows: the preedit, the committed strings and the candidates.
class IMDriver
  # UPreeditAttr
  module PreeditAttribute
    UNDERLINE = 1
    REVERSE = 2
    CURSOR = 4
  end

  class << self
    # Whether IM is built. Some input methods such as canna are built
    # only when they're enabled.
    def available?(im)
      ENV.fetch("LIBUIM_SCM_FILES", "").split(":").any? do |dir|
        path = File.join(dir, "installed-modules.scm")
        File.exist?(path) and File.read(path).include?("\"#{im}\"")
      end
    end
  end

  # [[attribute, text], ...]
  attr_reader :preedit
  attr_reader :commits
  # nil while the candidate window isn't shown.
  attr_reader :candidates
  attr_reader :candidate_index

  # SCHEME is evaluated before the input method starts, to configure
  # it. ENV is added to the environment of im-driver.
  def initialize(im, scheme, env: {})
    @im = im
    @scheme = scheme
    @env = env
    @preedit = []
    @commits = []
    @candidates = nil
    @candidate_index = nil
  end

  def start
    program = ENV["IM_DRIVER"]
    raise("IM_DRIVER is not set") if program.nil?

    @io = IO.popen(@env, [program, @im, "(begin #{@scheme})"], "r+")
    read_until("ready")
  end

  def stop
    return if @io.nil?
    @io.close_write
    @io.read
    @io.close
  end

  def press(*keys)
    keys.each do |key|
      run("key #{key}")
    end
  end

  def type(text)
    press(*text.chars)
  end

  def activate(action)
    run("action #{action}")
  end

  private
  def run(command)
    @io.puts(command)
    read_until("end")
  end

  def read_until(terminator)
    loop do
      line = @io.gets
      raise("im-driver exited before #{terminator}") if line.nil?
      name, *values = line.chomp.split("\t", -1)
      case name
      when terminator
        return
      when "preedit"
        @preedit = values.each_slice(2).collect do |attribute, text|
          [Integer(attribute, 10), text]
        end.reject do |attribute, text|
          text.empty? and attribute != PreeditAttribute::CURSOR
        end
      when "commit"
        @commits << values[0]
      when "candidates"
        @candidates = values
        @candidate_index = nil
      when "select"
        @candidate_index = Integer(values[0], 10)
      when "deactivate"
        @candidates = nil
        @candidate_index = nil
      else
        raise("unknown output from im-driver: #{line.inspect}")
      end
    end
  end
end
