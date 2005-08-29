/*

  Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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

#include "config.h"

#include <stdlib.h>
#include <string.h>

#include "uim-compat-scm.h"
#include "context.h"

static uim_lisp return_val;

/* will be deprecated. use uim_scm_c_str() instead */
char *
uim_get_c_string(uim_lisp str)
{
  return uim_scm_c_str(str);
}

long
uim_scm_repl_c_string(char *str, long want_init, long want_print)
{
  /* TODO: fix return value */
  Scm_eval_c_string(str);

  return 0;
}

int
uim_scm_symbol_value_int(const char *symbol_str)
{
  uim_lisp stack_start;
  uim_lisp val_;
  int val;

  uim_scm_gc_protect_stack(&stack_start);
  val_ = uim_scm_symbol_value(symbol_str);

  if NFALSEP(val_) {
    val = uim_scm_c_int(val_);
  } else {
    val = 0;
  }
  uim_scm_gc_unprotect_stack(&stack_start);

  return val;
}

uim_lisp
uim_scm_int_from_c_int(int integer)
{
  return uim_scm_make_int(integer);
}

char *
uim_scm_symbol_value_str(const char *symbol_str)
{
  uim_lisp stack_start;
  uim_lisp val_ = uim_scm_f();
  char *val;

  uim_scm_gc_protect_stack(&stack_start);
  val_ = uim_scm_symbol_value(symbol_str);

  if NFALSEP(val_) {
    val = uim_scm_c_str(val_);
  } else {
    val = NULL;
  }
  uim_scm_gc_unprotect_stack(&stack_start);

  return val;
}

/* backward compatibility */
char *
uim_symbol_value_str(const char *symbol_str)
{
  char *val;

  UIM_EVAL_FSTRING1(NULL, "(uim-symbol-value-str '%s)", symbol_str);
  val = uim_scm_c_str(uim_scm_return_value());

  return val;
}

/* temprary solution for getting an value from Scheme world */
uim_bool
uim_scm_symbol_value_bool(const char *symbol_str)
{
  uim_bool val;

  if (!symbol_str)
    return UIM_FALSE;

  val = uim_scm_c_bool(uim_scm_symbol_value(symbol_str));

  return val;
}

uim_lisp
uim_scm_str_from_c_str(const char *str)
{
  return uim_scm_make_str(str);
}

uim_lisp
uim_scm_c_strs_into_list(int n_strs, const char *const *strs)
{
  uim_lisp lst = (uim_lisp)SigScm_null;
  uim_lisp str = (uim_lisp)SigScm_null;
  const char *c_str;
  int i;

  for (i = n_strs - 1; 0 <= i; i--) {
    c_str = strs[i];
    str = (uim_lisp)Scm_NewStringCopying(c_str);
    lst = (uim_lisp)Scm_NewCons((ScmObj)str, (ScmObj)lst);
  }

  return (uim_lisp)lst;
}

uim_lisp
uim_scm_symbol_value(const char *symbol_str)
{
  return (uim_lisp)ScmOp_symbol_value(Scm_Intern(symbol_str));
}

uim_lisp
uim_scm_intern_c_str(const char *str)
{
  return uim_scm_make_symbol(str);
}

uim_lisp
uim_scm_qintern_c_str(const char *str)
{
  return uim_scm_quote(uim_scm_intern_c_str(str));
}

uim_lisp
uim_scm_nth(uim_lisp n, uim_lisp lst)
{
  return (uim_lisp)ScmOp_list_ref((ScmObj)lst,
				  (ScmObj)n);
}

/* Is this function used from somewhere? I think this function could be removed. */
uim_lisp
uim_scm_nreverse(uim_lisp cell)
{
  fprintf(stderr, "uim_scm_nreverse : not implemented yet.\n");
#if 0
  return (uim_lisp)nreverse((uim_lisp)cell);
#endif
}

void
uim_scm_init_fsubr(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp))
{
  Scm_RegisterFuncRawList(name, fcn);
}

void
uim_scm_provide(const char *feature)
{
  ScmOp_provide(Scm_NewStringCopying(feature));
}


/*
  - list_repl must always returns same list for each evaluation
  - returns NULL terminated array. NULL will not appeared except terminator
  - non-string element such as #f is converted to ""
 */
void **
uim_scm_c_list(const char *list_repl, const char *mapper_proc,
	       uim_scm_c_list_conv_func conv_func)
{
  int list_len, i;
  void **result;

  UIM_EVAL_FSTRING1(NULL, "(length %s)", list_repl);
  return_val = uim_scm_return_value();
  list_len = uim_scm_c_int(return_val);

  result = (void **)malloc(sizeof(void *) * (list_len + 1));
  if (!result)
    return NULL;

  result[list_len] = NULL;
  for (i = 0; i < list_len; i++) {
    UIM_EVAL_FSTRING3(NULL, "(%s (nth %d %s))", mapper_proc, i, list_repl);
    return_val = uim_scm_return_value();
    result[i] = (*conv_func)(return_val);
  }

  return result;
}

char *
uim_scm_c_str_failsafe(uim_lisp str)
{
  return (NFALSEP(str)) ? uim_scm_c_str(str) : strdup("");
}

char **
uim_scm_c_str_list(const char *list_repl, const char *mapper_proc)
{
  void **list;
  
  list = uim_scm_c_list(list_repl, mapper_proc,
			(uim_scm_c_list_conv_func)uim_scm_c_str_failsafe);

  return (char **)list;
}

void
uim_scm_c_list_free(void **list, uim_scm_c_list_free_func free_func)
{
  void *elem;
  void **p;

  if (!list)
    return;

  for (p = list; *p; p++) {
    elem = *p;
    free_func(elem);
  }
  free(list);
}

void
uim_init_compat_scm_subrs(void)
{
}
