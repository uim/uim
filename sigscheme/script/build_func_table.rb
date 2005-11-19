#!/usr/bin/env ruby
#===========================================================================
#  FileName : build_func_table.rb
#  About    : script to building built-in function table
#
#  Copyright (C) 2005      by Kazuki Ohta <mover at hct.zaq.ne.jp>
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
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
#  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
#  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
#  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
#  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
#  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#  SUCH DAMAGE.
#===========================================================================
require 'script/scm_decl.rb'

def search_declare_function(filename)
  puts "    /* #{filename} */"
  f = File.new(filename)

  print (scm_generate_func_table_body f.read)

  f.close
end

def build_table(filename)
  search_declare_function(filename)
end

def null_entry()
  puts "    { NULL, NULL, NULL }"
end

def print_tableheader(tablename)
  puts "struct builtin_func_info #{tablename}[] = {"
end

def print_tablefooter()
  puts "};"
  puts ""
end

def build_functable(tablename, filelist)
  print_tableheader(tablename)
  filelist.each { |filename|
    build_table(filename)
  }
  null_entry()
  print_tablefooter
end

def print_header()
  IO.readlines("./script/functable-header.txt").each { |line|
    puts line
  }
end

def print_footer()
  IO.readlines("script/functable-footer.txt").each { |line|
    puts line
  }
end

######################################################################

# Header
print_header

# Print Table
build_functable(ARGV[0],
                ARGV[1..-1])

# Footer
print_footer
