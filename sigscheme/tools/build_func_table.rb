#!/usr/bin/env ruby
#===========================================================================
#  FileName : build_func_table.rb
#  About    : script to building built-in function table
#
#  Copyright (C) 2005-2006 Kazuki Ohta <mover at hct.zaq.ne.jp>
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

require 'scm_decl.rb'

DATA_DIR    = "../tools"  # FIXME: make specifiable via commandline option
FILE_HEADER = "#{DATA_DIR}/functable-header.txt"
FILE_FOOTER = "#{DATA_DIR}/functable-footer.txt"

def table_header(table_name)
  "static const struct scm_func_registration_info #{table_name}[] = {"
end

def table_footer
  "};"
end

def build_table_body(filename)
  src = File.new(filename).read
  "    /* #{filename} */\n" + scm_generate_func_table_body(src)
end

def terminal_entry
  "    { NULL, NULL, SCM_FUNCTYPE_INVALID }"
end

def build_table(table_name, src_files)
  [
    table_header(table_name),
    src_files.collect { |src|
      build_table_body(src)
    },
    terminal_entry,
    table_footer,
    "\n"
  ].flatten.join("\n")
end

def file_header(table_filename)
  File.new(FILE_HEADER).read.gsub("@filename@", table_filename)
end

def file_footer(table_filename)
  File.new(FILE_FOOTER).read.gsub("@filename@", table_filename)
end

######################################################################

table_filename, table_name, *srcs = ARGV
table = build_table(table_name, srcs)
header = file_header(table_filename)
footer = file_footer(table_filename)

# The generated file should explicitly be opened by the script instead of
# printing to stdout redirected by shell, to avoid the accidencial file
# colluption problem that makes 'make' failed.
File.new(table_filename, "w").print(header, table, footer)
