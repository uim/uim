/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
/* FIXME: relative path should not be used */
#include "../sigscheme/sigscheme.h"
#include "../sigscheme/sigschemetype.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "context.h"

static uim_lisp true_sym;
static uim_lisp false_sym;
static uim_lisp protected_arg0;

static int uim_siod_fatal;
static FILE *uim_output = NULL;

#ifdef UIM_COMPAT_SCM
#include "uim-compat-scm.c"
#endif


FILE *
uim_scm_get_output(void)
{
  return uim_output;
}

void
uim_scm_set_output(FILE *fp)
{
  uim_output = fp;
}

uim_bool
uim_scm_c_bool(uim_lisp val)
{
  return NFALSEP(val);
}

uim_lisp
uim_scm_make_bool(uim_bool val)
{
  return (val) ? uim_scm_t() : uim_scm_f();
}

int
uim_scm_c_int(uim_lisp integer)
{
  int c_int;
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);  /* required for my_err() */

  protected_arg0 = integer;
  c_int = Scm_GetInt((ScmObj)integer);
  uim_scm_gc_unprotect_stack(&stack_start);

  return c_int;
}

uim_lisp
uim_scm_make_int(int integer)
{
  return (uim_lisp)Scm_NewInt(integer);
}

char *
uim_scm_c_str(uim_lisp str)
{
  const char *c_str;

  c_str = uim_scm_refer_c_str(str);

  return (c_str) ? strdup(c_str) : NULL;
}

const char *
uim_scm_refer_c_str(uim_lisp str)
{
  char *c_str;
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);  /* required for my_err() */
  protected_arg0 = str;
  /* FIXME: return value of this function must be freed somewhere... */
  c_str = Scm_GetString((ScmObj)str);
  uim_scm_gc_unprotect_stack(&stack_start);

  return c_str;
}

uim_lisp
uim_scm_make_str(const char *str)
{
  return (uim_lisp)Scm_NewStringCopying(str);
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  return strdup((char*)SCM_SYMBOL_NAME((ScmObj)symbol));
}

uim_lisp
uim_scm_make_symbol(const char *name)
{
  return (uim_lisp)Scm_Intern(name);
}

void *
uim_scm_c_ptr(uim_lisp ptr)
{
  return Scm_GetCPointer((ScmObj)ptr);
}

uim_lisp
uim_scm_make_ptr(void *ptr)
{
  return (uim_lisp)Scm_NewCPointer(ptr);
}

uim_func_ptr
uim_scm_c_func_ptr(uim_lisp func_ptr)
{
  return (uim_func_ptr)Scm_GetCFuncPointer((ScmObj)func_ptr);
}

uim_lisp
uim_scm_make_func_ptr(uim_func_ptr func_ptr)
{
  return (uim_lisp)Scm_NewCFuncPointer((C_FUNC)func_ptr);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  SigScm_gc_protect((ScmObj)(*location));
}

void
uim_scm_gc_protect_stack(uim_lisp *stack_start)
{
  SigScm_gc_protect_stack((ScmObj*)stack_start);
}

void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  SigScm_gc_unprotect_stack((ScmObj*)stack_start);
}

uim_bool
uim_scm_is_alive(void)
{
  return (!uim_siod_fatal);
}

long
uim_scm_get_verbose_level(void)
{
#if 0
  return siod_verbose_level;
#endif
  return 0;
}

void
uim_scm_set_verbose_level(long new_value)
{
#if 0
  siod_verbose_level = new_value;
#endif
}

void
uim_scm_set_lib_path(const char *path)
{
  SigScm_set_lib_path(path);
}

uim_bool
uim_scm_load_file(const char *fn)
{
  if (!fn)
    return UIM_FALSE;

  /* TODO: fixme! */
  SigScm_load(fn);

  return UIM_TRUE;
}

uim_lisp
uim_scm_t(void)
{
  return (uim_lisp)SCM_TRUE;
}

uim_lisp
uim_scm_f(void)
{
  return (uim_lisp)SCM_FALSE;
}

uim_lisp
uim_scm_null_list(void)
{
  return (uim_lisp)SCM_NULL;
}

uim_bool
uim_scm_nullp(uim_lisp obj)
{
  if (SCM_NULLP((ScmObj)obj))
    return UIM_TRUE;

  return UIM_FALSE;
}

uim_bool
uim_scm_consp(uim_lisp obj)
{
  if (SCM_CONSP((ScmObj)obj))
    return UIM_TRUE;

  return UIM_FALSE;  
}

uim_bool
uim_scm_integerp(uim_lisp obj)
{
  if (SCM_INTP((ScmObj)obj))
    return UIM_TRUE;

  return UIM_FALSE;  
}

uim_bool
uim_scm_stringp(uim_lisp obj)
{
  if (SCM_STRINGP((ScmObj)obj))
    return UIM_TRUE;

  return UIM_FALSE;  
}

uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b)
{
  if (SCM_EQ(ScmOp_eqp((ScmObj) a, (ScmObj) b), SCM_TRUE))
    return UIM_TRUE;

  return UIM_FALSE;
}

uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b)
{
  if(SCM_EQ(ScmOp_string_equal((ScmObj)a, (ScmObj)b), SCM_TRUE))
    return UIM_TRUE;

  return UIM_FALSE;
}

uim_lisp
uim_scm_eval(uim_lisp obj)
{
  uim_lisp ret;  /* intentionally outside of next stack_start */
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);
  ret = (uim_lisp)ScmOp_eval((ScmObj)obj, SCM_NULL);
  uim_scm_gc_unprotect_stack(&stack_start);
  return ret;
}

#ifdef UIM_SCM_EXTENDED_API
uim_lisp
uim_scm_apply(uim_lisp proc, uim_lisp args)
{
  return (uim_lisp)ScmOp_apply(Scm_NewCons((ScmObj)proc,
					   Scm_NewCons((ScmObj)args, SCM_NULL)),	
			       SCM_NULL);
}

uim_lisp
uim_scm_quote(uim_lisp obj)
{
  /* TODO : fixme Kazuki Ohta <mover@hct.zaq.ne.jp> */
  return (uim_lisp)Scm_NewCons(SCM_QUOTE,
			       Scm_NewCons((ScmObj)obj,
					   SCM_NULL));
}
#endif  /* UIM_SCM_EXTENDED_API */

uim_lisp
uim_scm_eval_c_string(const char *str)
{
  return (uim_lisp)Scm_eval_c_string(str);
}

uim_lisp
uim_scm_return_value(void)
{
  /* FIXME: This function should be removed. */
  return (uim_lisp)Scm_return_value();
}

uim_lisp
uim_scm_car(uim_lisp pair)
{
  return (uim_lisp)ScmOp_car((ScmObj)pair);
}

uim_lisp
uim_scm_cdr(uim_lisp pair)
{
  return (uim_lisp)ScmOp_cdr((ScmObj)pair);
}

uim_lisp
uim_scm_cadr(uim_lisp lst)
{
  return (uim_lisp)ScmOp_cadr((ScmObj)lst);
}

uim_lisp
uim_scm_caar(uim_lisp lst)
{
  return (uim_lisp)ScmOp_caar((ScmObj)lst);
}

uim_lisp
uim_scm_cdar(uim_lisp lst)
{
  return (uim_lisp)ScmOp_cdar((ScmObj)lst);
}

uim_lisp
uim_scm_cddr(uim_lisp lst)
{
  return (uim_lisp)ScmOp_cddr((ScmObj)lst);
}

uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr)
{
  return (uim_lisp)Scm_NewCons((ScmObj)car, (ScmObj)cdr);
}

uim_lisp
uim_scm_length(uim_lisp lst)
{
  return (uim_lisp)ScmOp_length((ScmObj)lst);
}

uim_lisp
uim_scm_reverse(uim_lisp lst)
{
  return (uim_lisp)ScmOp_reverse((ScmObj)lst);
}

#ifdef UIM_SCM_EXTENDED_API
uim_lisp
uim_scm_list1(uim_lisp elm1)
{
  uim_lisp lst;
  lst = uim_scm_cons(elm1, uim_scm_null_list());
  return lst;
}

uim_lisp
uim_scm_list2(uim_lisp elm1, uim_lisp elm2)
{
  uim_lisp lst;
  lst = uim_scm_cons(elm1, uim_scm_cons(elm2, uim_scm_null_list()));
  return lst;
}

uim_lisp
uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3)
{
  uim_lisp lst;
  lst = uim_scm_cons(elm1, uim_scm_cons(elm2, uim_scm_cons(elm3, uim_scm_null_list())));
  return lst;
}

uim_lisp
uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4)
{
  uim_lisp lst;
  lst = uim_scm_cons(elm1, uim_scm_list3(elm2, elm3, elm4));
  return lst;
}

uim_lisp
uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4,
              uim_lisp elm5)
{
  uim_lisp lst;
  lst = uim_scm_cons(elm1, uim_scm_cons(elm2, uim_scm_list3(elm3, elm4, elm5)));
  return lst;
}
#endif  /* UIM_SCM_EXTENDED_API */

uim_bool
uim_scm_require_file(const char *fn)
{
  if (!fn)
    return UIM_FALSE;

  ScmOp_require(Scm_NewStringCopying(fn));

  /* TODO: fixme */
  return UIM_TRUE;
}

#if 0
siod_init_subr(const char *name, long type, SUBR_FUNC fcn)
{
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);
  init_subr(name, type, fcn);
  uim_scm_gc_unprotect_stack(&stack_start);
}
#endif

void
uim_scm_init_subr_0(const char *name, uim_lisp (*func)(void))
{
  Scm_RegisterFunc0(name, func);
}

void
uim_scm_init_subr_1(const char *name, uim_lisp (*func)(uim_lisp))
{
  Scm_RegisterFunc1(name, func);
}

void
uim_scm_init_subr_2(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp))
{
  Scm_RegisterFunc2(name, func);
}

void
uim_scm_init_subr_3(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp))
{
  Scm_RegisterFunc3(name, func);
}

void
uim_scm_init_subr_4(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp))
{
  Scm_RegisterFunc4(name, func);
}

void
uim_scm_init_subr_5(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp))
{
  Scm_RegisterFunc5(name, func);
}

static void
exit_hook(void)
{
#if 0
  uim_siod_fatal = 1;
#endif
}

void
uim_scm_init(const char *verbose_level)
{
  if (!uim_output) {
    uim_output = stderr;
  }

  SigScm_Initialize();
  true_sym  = (uim_lisp)SCM_TRUE;
  false_sym = (uim_lisp)SCM_FALSE;
  protected_arg0 = uim_scm_f();
}

void
uim_scm_quit(void)
{
  SigScm_Finalize();
  uim_output = NULL;
}
