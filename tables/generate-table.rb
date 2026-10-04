#!/usr/bin/env ruby
#
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

module UimTableGenerator
  class ParseError < StandardError
  end

  class Table
    def initialize
      @candidates = {}
      @seen = {}
    end

    def add(code, candidate)
      pair = [code, candidate]
      return if @seen[pair]

      @seen[pair] = true
      (@candidates[code] ||= []) << candidate
    end

    def write(output)
      @candidates.keys.sort.each do |code|
        candidates = @candidates.fetch(code).map do |candidate|
          %Q{"#{escape(candidate)}"}
        end
        output.puts "#{code} (#{candidates.join(" ")})"
      end
    end

    private

    def escape(string)
      string.gsub("\\", "\\\\").gsub('"', '\\"').
        gsub("\n", "\\n").gsub("\r", "\\r")
    end
  end

  module_function

  def parse_wb86(input, path = "(input)")
    table = Table.new
    input.each_line.with_index(1) do |line, line_number|
      line = line.chomp
      fields = line.split("\t", -1)
      unless fields.length == 3
        raise ParseError, "#{path}:#{line_number}: expected three tab-separated fields"
      end

      code, candidate, frequency = fields
      code = code.downcase
      unless /\A[a-z]{1,4}\z/.match(code)
        raise ParseError, "#{path}:#{line_number}: invalid code: #{code.inspect}"
      end
      unless /\A-?\d+\z/.match(frequency)
        raise ParseError, "#{path}:#{line_number}: invalid frequency: #{frequency.inspect}"
      end

      # Haifeng marks rare, single-character entries with a trailing dot.
      if candidate.end_with?(".")
        without_dot = candidate[0...-1]
        candidate = without_dot if without_dot.each_char.one?
      end
      if candidate.empty?
        raise ParseError, "#{path}:#{line_number}: empty candidate"
      end

      table.add(code, candidate)
    end
    table
  end

  def parse_zm(input, path = "(input)")
    table = Table.new
    in_data = false
    found_data = false

    input.each_line.with_index(1) do |line, line_number|
      line = line.chomp
      unless in_data
        if line == "[Data]"
          in_data = true
          found_data = true
        end
        next
      end

      next if line.empty?

      code, candidate = line.split(/\s+/, 2)
      unless code && candidate && !candidate.empty?
        raise ParseError, "#{path}:#{line_number}: expected a code and candidate"
      end

      # Lines prefixed with ^ are Fcitx phrase-construction metadata, not
      # mappings a user can type.
      next if code.start_with?("^")

      unless /\A[a-z]{1,4}\z/.match(code)
        raise ParseError, "#{path}:#{line_number}: invalid code: #{code.inspect}"
      end

      table.add(code, candidate)
    end

    raise ParseError, "#{path}: missing [Data] section" unless found_data

    table
  end
end

if $PROGRAM_NAME == __FILE__
  unless ARGV.length == 2 && %w[wb86 zm].include?(ARGV[0])
    warn "usage: #{File.basename($PROGRAM_NAME)} {wb86|zm} SOURCE"
    exit 2
  end

  format, path = ARGV
  File.open(path, "r:BOM|UTF-8") do |input|
    table = if format == "wb86"
              UimTableGenerator.parse_wb86(input, path)
            else
              UimTableGenerator.parse_zm(input, path)
            end
    STDOUT.set_encoding(Encoding::UTF_8)
    table.write(STDOUT)
  end
end
