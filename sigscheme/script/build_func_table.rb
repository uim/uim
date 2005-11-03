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

FUNC_TYPE_INVALID   = 0
FUNC_TYPE_SYNTAX    = 1
FUNC_TYPE_PROCEDURE = 2
FUNC_TYPE_REDUCTION = 3

TYPE2PREFIX = {
  FUNC_TYPE_SYNTAX    => "ScmExp_",
  FUNC_TYPE_PROCEDURE => "ScmOp_",
  FUNC_TYPE_REDUCTION => "ScmOp_",
}

SCM2C_FUNCNAME_RULE = [
  # prefix
  [/^\+/,        "add"],
  [/^\*/,        "multiply"],
  [/^-/,         "subtract"],
  [/^\//,        "divide"],
  [/^<=/,         "less_eq"],
  [/^</,          "less"],
  [/^>=/,         "greater_eq"],
  [/^>/,          "greater"],
  [/^\=/,         "equal"],
  [/^%%/,         "sscm_"],

  # suffix
  [/\?$/,  "p"],
  [/!$/,   "d"],

  # suffix or intermediate
  [/-/,   "_"],
  [/->/,  "2"],
  [/\?/,  "_"],
  [/!/,   "_"],
  [/\=/,  "equal"],
  [/\*/,  "star"],
  [/\+/,  "plus"],
]

def guess_c_funcname(scm_funcname, type)
  # guess prefix
  prefix = TYPE2PREFIX[type] || "";

  # apply replace rule
  c_funcname = scm_funcname
  SCM2C_FUNCNAME_RULE.each { |rule|
    c_funcname = c_funcname.gsub(rule[0], rule[1])
  }
  
  return prefix + c_funcname
end

def search_declare_function(filename)
  puts "    /* #{filename} */"
  IO.readlines(filename).each{ |line|
    if line.strip =~ /DECLARE_FUNCTION\(\"(\S+)\",\s*((Syntax|Procedure|Reduction)\S+)\);/
      scm_func = $1
      reg_func = "Scm_Register" + $2

      type = if reg_func.index("Syntax")
               FUNC_TYPE_SYNTAX
             elsif reg_func.index("Procedure")
               FUNC_TYPE_PROCEDURE
             elsif reg_func.index("Reduction")
               FUNC_TYPE_REDUCTION
             else
               FUNC_TYPE_INVALID
             end

      c_func = guess_c_funcname(scm_func, type)

      puts "    { \"#{scm_func}\", (ScmBuiltinFunc)#{c_func}, (ScmRegisterFunc)#{reg_func} },"
    end
  }
end

def build_table(filename)
  search_declare_function(filename)
end

def null_entry()
  puts "    {NULL, NULL, NULL}"
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


# R5RS
build_functable("r5rs_func_info_table",
                ["eval.c", "io.c", "operations.c", "sigscheme.c"])

# SRFI-1
build_functable("srfi1_func_info_table",
                ["operations-srfi1.c"])

# SRFI-2
build_functable("srfi2_func_info_table",
                ["operations-srfi2.c"])

# SRFI-6
build_functable("srfi6_func_info_table",
                ["operations-srfi6.c"])

# SRFI-8
build_functable("srfi8_func_info_table",
                ["operations-srfi8.c"])

# SRFI-23
build_functable("srfi23_func_info_table",
                ["operations-srfi23.c"])

# SRFI-34
build_functable("srfi34_func_info_table",
                ["operations-srfi34.c"])

# SRFI-38
build_functable("srfi38_func_info_table",
                ["operations-srfi38.c"])

# SRFI-60
build_functable("srfi60_func_info_table",
                ["operations-srfi60.c"])

# SIOD
build_functable("siod_func_info_table",
                ["operations-siod.c"])

# Footer
print_footer
