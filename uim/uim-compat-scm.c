/*

  Copyright (c) 2003-2008 uim Project http://code.google.com/p/uim/

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

#include <stdlib.h>
#include <string.h>

#include "uim-compat-scm.h"
#include "uim-internal.h"

#if UIM_SCM_GCC4_READY_GC
static void *uim_scm_symbol_value_int_internal(const char *symbol_str);
static char *uim_scm_symbol_value_str_internal(const char *symbol_str);
#endif

extern uim_lisp uim_scm_last_val;

/* will be deprecated. use uim_scm_c_str() instead */
char *
uim_get_c_string(uim_lisp str)
{
  return uim_scm_c_str(str);
}

long
uim_scm_repl_c_string(char *str, long want_init, long want_print)
{
  uim_scm_last_val = (uim_lisp)scm_eval_c_string(str);

  return 0;
}

int
uim_scm_symbol_value_int(const char *symbol_str)
#if UIM_SCM_GCC4_READY_GC
{
  return (int)(intptr_t)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_symbol_value_int_internal, (void *)symbol_str);
}

static void *
uim_scm_symbol_value_int_internal(const char *symbol_str)
#endif
{
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;
#endif
  uim_lisp val_;
  int val;

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_protect_stack(&stack_start);
#endif
  val_ = uim_scm_symbol_value(symbol_str);

  if (UIM_SCM_NFALSEP(val_)) {
    val = uim_scm_c_int(val_);
  } else {
    val = 0;
  }
#if UIM_SCM_GCC4_READY_GC
  return (void *)(intptr_t)val;
#else
  uim_scm_gc_unprotect_stack(&stack_start);

  return val;
#endif
}

uim_lisp
uim_scm_int_from_c_int(int integer)
{
  return uim_scm_make_int(integer);
}

char *
uim_scm_symbol_value_str(const char *symbol_str)
#if UIM_SCM_GCC4_READY_GC
{
  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_symbol_value_str_internal, (void *)symbol_str);
}

static char *
uim_scm_symbol_value_str_internal(const char *symbol_str)
#endif
{
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;
#endif
  uim_lisp val_ = uim_scm_f();
  char *val;

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_protect_stack(&stack_start);
#endif
  val_ = uim_scm_symbol_value(symbol_str);

  if (UIM_SCM_NFALSEP(val_)) {
    val = uim_scm_c_str(val_);
  } else {
    val = NULL;
  }
#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

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
  uim_lisp lst, str;
  const char *c_str;
  int i;

  for (lst = uim_scm_null_list(), i = n_strs - 1; 0 <= i; i--) {
    c_str = strs[i];
    str = uim_scm_make_str(c_str);
    lst = uim_scm_cons(str, lst);
  }

  return lst;
}

uim_lisp
uim_scm_symbol_value(const char *symbol_str)
{
  ScmObj symbol;

  symbol = scm_intern(symbol_str);
  if (SCM_TRUEP(scm_p_symbol_boundp(symbol, SCM_NULL))) {
    return (uim_lisp)scm_p_symbol_value(symbol);
  } else {
    return uim_scm_f();
  }
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
uim_scm_quote(uim_lisp obj)
{
  return (uim_lisp)SCM_LIST_2(SCM_SYM_QUOTE, (ScmObj)obj);
}

uim_lisp
uim_scm_nth(uim_lisp n, uim_lisp lst)
{
  return (uim_lisp)scm_p_list_ref((ScmObj)lst,
				  (ScmObj)n);
}

uim_lisp
uim_scm_list1(uim_lisp elm1)
{
  return uim_scm_cons(elm1, uim_scm_null_list());
}

uim_lisp
uim_scm_list2(uim_lisp elm1, uim_lisp elm2)
{
  return uim_scm_cons(elm1, uim_scm_list1(elm2));
}

uim_lisp
uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3)
{
  return uim_scm_cons(elm1, uim_scm_list2(elm2, elm3));
}

uim_lisp
uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4)
{
  return uim_scm_cons(elm1, uim_scm_list3(elm2, elm3, elm4));
}

uim_lisp
uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4,
              uim_lisp elm5)
{
  return uim_scm_cons(elm1, uim_scm_list4(elm2, elm3, elm4, elm5));
}

/* Is this function used from somewhere? I think this function could be removed. */
/*
 * Not only this function but all functions of this file should be
 * removed. See the header comment of uim-compat-scm.h. Remove these
 * two comments if you have been satisfied by this answer.
 *   -- YamaKen 2005-09-18
 */
uim_lisp
uim_scm_nreverse(uim_lisp cell)
{
  fprintf(stderr, "uim_scm_nreverse : not implemented yet.\n");
  return uim_scm_null_list();
#if 0
  return (uim_lisp)nreverse((uim_lisp)cell);
#endif
}

void
uim_scm_init_fsubr(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp))
{
  scm_register_func(name, (scm_syntax_variadic_0)fcn, SCM_SYNTAX_VARIADIC_0);
}

void
uim_scm_provide(const char *feature)
{
  scm_p_provide(SCM_CONST_STRING(feature));
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
  list_len = uim_scm_c_int(uim_scm_return_value());

  result = (void **)malloc(sizeof(void *) * (list_len + 1));
  if (!result)
    return NULL;

  result[list_len] = NULL;
  for (i = 0; i < list_len; i++) {
    UIM_EVAL_FSTRING3(NULL, "(%s (nth %d %s))", mapper_proc, i, list_repl);
    result[i] = (*conv_func)(uim_scm_return_value());
  }

  return result;
}

char *
uim_scm_c_str_failsafe(uim_lisp str)
{
  return (UIM_SCM_NFALSEP(str)) ? uim_scm_c_str(str) : strdup("");
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
