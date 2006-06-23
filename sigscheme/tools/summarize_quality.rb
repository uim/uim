#!/usr/bin/env ruby
#===========================================================================
#  Filename : summarize_quality.rb
#  About    : Summarize code quality of SigScheme based on the quality
#             assurance log
#
#  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
#
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. Neither the name of authors nor the names of its contributors
#     may be used to endorse or promote products derived from this software
#     without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
#  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
#  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
#  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
#  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#===========================================================================

# usage:
# summarize_quality.rb QALog

$line = "\n"
$status = ""
$status_section = false

def print_status
  print $status, $line if ($line != "\n")
  $status = ""
  $line = "\n"
end

print <<EOT
+-------- specification conformance check by eyes
|+------- specification conformance check by tests
||+------ general code review (logic, structure, meaning etc)
|||+----- 64-bit model capability check by eyes
||||+---- 64-bit model capability check by tests
|||||+--- coding style check (doc/style.txt)
||||||+-- normal case tests
|||||||+- corner case tests
||||||||
EOT

while gets
  case $_
  when /^$/
    print_status
  when /^Status$/
    status_section = true
  when /^Log$/
    status_section = false
  when /^([^:]+):\s+(.*)$/
    next unless status_section
    field, value = $1, $2
    if (field =~ /^(file|category)$/)
      $line = " #{value}" + $line
    else
      $status += (if (value == "") then " " else value[0,1] end)
    end
  end
end

print_status
