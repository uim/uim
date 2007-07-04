/*

  Copyright (c) 2003-2007 uim Project http://code.google.com/p/uim/

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <config.h>
#include <assert.h>

#include "uim-compat-scm.h"


struct cmp_args {
  uim_lisp a;
  uim_lisp b;
};
static void *uim_scm_string_equal_internal(struct cmp_args *args);


uim_lisp
uim_scm_c_strs_into_list(int n_strs, const char *const *strs)
{
  uim_lisp lst, str;
  const char *c_str;
  int i;

  assert(n_strs >= 0);
  assert(strs);

  for (lst = uim_scm_null_list(), i = n_strs - 1; 0 <= i; i--) {
    c_str = strs[i];
    str = uim_scm_make_str(c_str);
    lst = uim_scm_cons(str, lst);
  }

  return lst;
}

uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b)
{
  struct cmp_args args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(a));
  assert(uim_scm_gc_protectedp(b));

  args.a = a;
  args.b = b;
  return (uim_bool)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_string_equal_internal, &args);
}

static void *
uim_scm_string_equal_internal(struct cmp_args *args)
{
  return (void *)SCM_TRUEP(scm_p_stringequalp((ScmObj)args->a,
                                              (ScmObj)args->b));
}
