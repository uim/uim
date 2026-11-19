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

# Stands in for karukan-imserver. It speaks the JSON-RPC 2.0 protocol
# version 1 of karukan-imserver a line at a time.
#
# The reading is what is typed as is. Space converts it: the
# candidates are its upper case and itself. Space again moves the
# cursor and Return commits the selected one. Ctrl+K enters the
# katakana mode, which is the upper case here, until a reset. Other
# keys aren't consumed.
# "!" makes it exit after the response as if it crashed.
#
# Each request is appended to the file of FAKE_KARUKAN_IMSERVER_LOG as
# a JSON line.

require "json"

class FakeKarukanIMServer
  RETURN = 0xff0d
  SPACE = 0x20
  EXCLAMATION_MARK = 0x21
  K = 0x6b

  def initialize(input, output, log)
    @input = input
    @output = output
    @log = log
    @katakana = false
    reset
  end

  def run
    @input.each_line do |line|
      @log.puts(line)
      @log.flush
      request = JSON.parse(line)
      result = dispatch(request["method"], request["params"] || {})
      next unless request.key?("id")
      @output.puts(JSON.generate({
                                   "jsonrpc" => "2.0",
                                   "id" => request["id"],
                                   "result" => result,
                                 }))
      @output.flush
      exit!(false) if @exit
    end
  end

  private
  def reset
    @reading = ""
    @candidates = nil
    @cursor = 0
  end

  def dispatch(method, params)
    case method
    when "init"
      {"protocol_version" => 1, "model_name" => "fake"}
    when "process_key"
      process_key(params["keysym"], params["modifiers"] || {})
    when "commit"
      key_result(true, commit)
    when "reset"
      @katakana = false
      reset
      {}
    else
      {}
    end
  end

  def process_key(keysym, modifiers)
    @exit = true if keysym == EXCLAMATION_MARK
    if keysym == K && modifiers["control"]
      @katakana = true
      return key_result(true, [])
    end
    if modifiers.values.any?
      return key_result(false, [])
    end
    if @candidates
      convert_key(keysym)
    else
      compose_key(keysym)
    end
  end

  def compose_key(keysym)
    case keysym
    when SPACE
      return key_result(false, []) if @reading.empty?
      @candidates = [@reading.upcase, @reading]
      @cursor = 0
      key_result(true, conversion_actions)
    when RETURN
      return key_result(false, []) if @reading.empty?
      key_result(true, commit)
    when 0x21..0x7e
      character = keysym.chr
      character = character.upcase if @katakana
      @reading += character
      key_result(true, [preedit(@reading, "underline")])
    else
      key_result(false, [])
    end
  end

  def convert_key(keysym)
    case keysym
    when SPACE
      @cursor = (@cursor + 1) % @candidates.size
      key_result(true, conversion_actions)
    when RETURN
      key_result(true, commit)
    else
      key_result(true, [])
    end
  end

  def commit
    text = @candidates ? @candidates[@cursor] : @reading
    reset
    # Like karukan-imserver, a commit doesn't clear the preedit
    # explicitly.
    [
      {"type" => "commit", "text" => text},
      {"type" => "hide_candidates"},
    ]
  end

  def conversion_actions
    [
      preedit(@candidates[@cursor], "highlight"),
      {
        "type" => "show_candidates",
        "candidates" => @candidates.collect {|text| {"text" => text}},
        "cursor" => @cursor,
        "page" => 0,
        "total_pages" => 1,
      },
    ]
  end

  def preedit(text, style)
    attributes = []
    unless text.empty?
      attributes << {"start" => 0, "end" => text.size, "style" => style}
    end
    {
      "type" => "update_preedit",
      "text" => text,
      "caret" => text.size,
      "attributes" => attributes,
    }
  end

  def key_result(consumed, actions)
    {
      "consumed" => consumed,
      "actions" => actions,
      "conversion_ms" => 0,
      "process_key_ms" => 0,
    }
  end
end

if __FILE__ == $0
  File.open(ENV.fetch("FAKE_KARUKAN_IMSERVER_LOG"), "a") do |log|
    FakeKarukanIMServer.new($stdin, $stdout, log).run
  end
end
