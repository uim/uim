#!/usr/bin/ruby
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

$FUNC_TYPE_INVALID   = 0
$FUNC_TYPE_SYNTAX    = 1
$FUNC_TYPE_PROCEDURE = 2
$FUNC_TYPE_REDUCTION = 3

$SCM2C_FUNCNAME_RULE = [
  [/^\+/,        "add"],
  [/^\*/,        "multiply"],
  [/^-/,         "subtract"],
  [/^\//,        "divide"],
  [/^<=/,         "less_eq"],
  [/^</,          "less"],
  [/^>=/,         "greater_eq"],
  [/^>/,          "greater"],
  [/^\=/,         "equal"],

  [/\*/,   "star"],
  [/->/,  "2"],
  [/\?/,  "p"],
  [/!/,  "d"],
  [/-/,    "_"]
]

def guess_c_funcname(scm_funcname, type)
  # guess prefix
  prefix = ""
  if type == $FUNC_TYPE_SYNTAX
    prefix = "ScmExp_"
  elsif type == $FUNC_TYPE_PROCEDURE
    prefix = "ScmOp_"
  elsif type == $FUNC_TYPE_REDUCTION
    prefix = "ScmOp_"
  end

  # apply replace rule
  c_funcname = scm_funcname
  $SCM2C_FUNCNAME_RULE.each { |rule|
    c_funcname = c_funcname.gsub(rule[0], rule[1])
  }
  
  return prefix + c_funcname
end

def search_declare_function(filename)
  IO.readlines(filename).each{ |line|
    if line.strip =~ /DECLARE_FUNCTION\(\"(\S+)\",\s*((Syntax|Procedure|Reduction)\S+)\);/
      scm_func = $1
      reg_func = "Scm_Register" + $2

      type = $FUNC_TYPE_INVALID
      if reg_func.index("Syntax")
        type = $FUNC_TYPE_SYNTAX
      elsif reg_func.index("Procedure")
        type = $FUNC_TYPE_PROCEDURE
      elsif reg_func.index("Reduction")
        type = $FUNC_TYPE_REDUCTION
      end

      c_func = guess_c_funcname(scm_func, type)

      puts "{ \"#{scm_func}\", #{c_func}, #{reg_func} },"
    end
  }
end

def build_table(filename)
  search_declare_function(filename)
end


def build_table_of_all_c_file
  Dir.foreach("./") { |file|
    if (file != "." && file != ".." && /[\w].\.c/ =~ file)
      build_table(file)
    end
  }
end


######################################################################

if ARGV.empty?
  puts "usage: ruby build_func_table.rb filename1 filename2 ..."
else
  ARGV.each{ |filename|
    build_table(filename)
  }
end
