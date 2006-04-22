#===========================================================================
#  Filename : scm_decl.rb
#  About    : a function declaration processing library for SigScheme
#
#  Copyright (C) 2005-2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

# ScmObj
# scm_p_call_with_values(ScmObj producer, ScmObj consumer,
#                        ScmEvalState *eval_state)
# {
#     ScmObj vals;
#     DECLARE_FUNCTION("call-with-values", procedure_fixed_tailrec_2);
#
#         |
#         V
#
# $1  :prototype       SCM_EXPORT ScmObj scm_p_call_with_values(ScmObj producer, ScmObj consumer, ScmEvalState *eval_state)
# $2  :scope           SCM_EXPORT
# $3  :ret             ScmObj
# $4  :func            scm_p_call_with_values
# $5  :prefix          p
# $6  :func_body       call_with_values
# $7  :args            ScmObj producer, ScmObj consumer, ScmEvalState *eval_state
# $8  :proc            call-with-values
# $9  :functype_whole  procedure_fixed_tailrec_2
# $10 :functype_prefix procedure
# $11 :functype_spec   fixed_tailrec_2
SCM_DECL_RE = /\n((SCM_EXPORT\s+)?(ScmObj)\s+(scm_([sp])_(\w+))\(([^{]+)\))[ \t]*\n\s*\{[^{}]+DECLARE_FUNCTION\(\s*\"([^\"]+)\"[\s,]+(([^_]+)_([\w]+))\)/m


# :register_func   scm_register_procedure_fixed_tailrec_2
# :functype_code   SCM_PROCEDURE_FIXED_TAILREC_2
class String
  def scan_scm_decl
    res = []
    scan(SCM_DECL_RE) { |prototype, scope, ret, func, prefix, func_body, args, proc, functype_whole, functype_prefix, functype_spec|
      decl = {
        :prototype       => prototype.gsub(/\s+/, " "),
        :scope           => scope,
        :ret             => ret,
        :func            => func,
        :prefix          => prefix,
        :func_body       => func_body,
        :args            => args.gsub(/\s+/, " "),
        :proc            => proc,
        :register_func   => "scm_register_" + functype_whole,
        :functype_code   => "SCM_" + functype_whole.upcase,
        :functype_whole  => functype_whole,
        :functype_prefix => functype_prefix,
        :functype_spec   => functype_spec,
      }
      res << yield(decl)
    }
    res
  end
end

# Obsolete
#def scm_func_table_entry(decl)
#  proc, func, register_func = decl.values_at(:proc, :func, :register_func)
#  "{ \"#{proc}\", (ScmFuncType)#{func}, (ScmRegisterFunc)#{register_func} }"
#end

def scm_func_table_entry(decl)
  proc, func, functype_code = decl.values_at(:proc, :func, :functype_code)
  "{ \"#{proc}\", (ScmFuncType)#{func}, #{functype_code} }"
end

def scm_func_register_exp(decl)
  proc, func, register_func = decl.values_at(:proc, :func, :register_func)
  "#{register_func}(\"#{proc}\", #{func})"
end

def scm_generate_func_table_body(str)
  str.scan_scm_decl { |decl|
    entry = scm_func_table_entry(decl)
    "    #{entry},\n"
  }.join
end

def scm_generate_func_register_exps(str)
  str.scan_scm_decl { |decl|
    exp = scm_func_register_exp(decl)
    "    #{exp};\n"
  }.join
end

def scm_generate_func_prototypes(str)
  str.scan_scm_decl { |decl|
    "#{decl[:prototype]};\n"
  }.join
end


# usage examples

#src = ARGF.read

#print src.scan_scm_decl { |decl|
#  p decl
#}

#print src.scan_scm_decl { |decl|
#  if (/!$/ =~ decl[:proc])
#    func = decl[:func]
#    print "perl -i -pe 's/\\b#{func}\\b/#{func.sub(/d$/, "x")}/g'\n"
#  end
#}

#print scm_generate_func_table_body(src)
#print scm_generate_func_register_exps(src)
#print scm_generate_func_prototypes(src)
