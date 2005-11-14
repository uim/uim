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
#include "sigscheme/sigscheme.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-internal.h"


#ifdef UIM_COMPAT_SCM
#include "uim-compat-scm.c"
#endif

/* FIXME: violent internal access */
extern ScmObj scm_current_output_port;
extern ScmObj scm_current_error_port;

static void uim_scm_error(const char *msg, uim_lisp errobj);

#if UIM_SCM_GCC4_READY_GC
static void uim_scm_error_internal(const char *msg, uim_lisp errobj);
static int uim_scm_c_int_internal(uim_lisp integer);
static const char *uim_scm_refer_c_str_internal(uim_lisp str);
static uim_lisp uim_scm_eval_internal(uim_lisp obj);
static uim_lisp uim_scm_eval_c_string_internal(const char *str);
#endif

static uim_lisp true_sym;
static uim_lisp false_sym;
static uim_lisp protected_arg0;

static int uim_siod_fatal;
static FILE *uim_output = NULL;

#if UIM_SCM_GCC4_READY_GC
/* See also the comment about these variables in uim-scm.h */
uim_lisp *(*volatile uim_scm_gc_protect_stack_ptr)(void)
  = &uim_scm_gc_protect_stack_internal;
#endif /* UIM_SCM_GCC4_READY_GC */


static void
uim_scm_error(const char *msg, uim_lisp errobj)
#if UIM_SCM_GCC4_READY_GC
{
  UIM_SCM_GC_PROTECTED_CALL_VOID(uim_scm_error_internal, (msg, errobj));
}

static void
uim_scm_error_internal(const char *msg, uim_lisp errobj)
#endif /* UIM_SCM_GCC4_READY_GC */
{
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);
#endif

  /* FIXME: don't terminate the process */
  SigScm_ErrorObj(msg, (ScmObj)errobj);

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif
}

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
#if UIM_SCM_GCC4_READY_GC
{
  int ret;

  UIM_SCM_GC_PROTECTED_CALL(ret, int, uim_scm_c_int_internal, (integer));

  return ret;
}

static int
uim_scm_c_int_internal(uim_lisp integer)
#endif
{
  int c_int;
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;

  /* stack protection is required for my_err() */
  uim_scm_gc_protect_stack(&stack_start);
#endif

  if (SCM_INTP((ScmObj)integer)) {
    c_int = SCM_INT_VALUE((ScmObj)integer);
  } else {
    uim_scm_error("uim_scm_c_int: number required but got ",
                  (uim_lisp)integer);
    c_int = -1;
  }

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

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
#if UIM_SCM_GCC4_READY_GC
{
  const char *ret;

  UIM_SCM_GC_PROTECTED_CALL(ret, const char *, uim_scm_refer_c_str_internal, (str));

  return ret;
}

static const char *
uim_scm_refer_c_str_internal(uim_lisp str)
#endif
{
  char *c_str;
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;

  /* stack protection is required for my_err() */
  uim_scm_gc_protect_stack(&stack_start);
#endif

  if (SCM_STRINGP((ScmObj)str)) {
    c_str = SCM_STRING_STR((ScmObj)str);
  } else if (SCM_SYMBOLP((ScmObj)str)) {
    c_str = SCM_SYMBOL_NAME((ScmObj)str);
  } else {
    uim_scm_error("uim_scm_refer_c_str: string or symbol required but got ",
                  (uim_lisp)str);
    c_str = NULL;
  }

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

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
  if (SCM_C_POINTERP((ScmObj)ptr)) {
    return SCM_C_POINTER_VALUE((ScmObj)ptr);
  } else {
    uim_scm_error("uim_scm_c_ptr: C pointer required but got ", (uim_lisp)ptr);
    return NULL;
  }
}

uim_lisp
uim_scm_make_ptr(void *ptr)
{
  return (uim_lisp)Scm_NewCPointer(ptr);
}

uim_func_ptr
uim_scm_c_func_ptr(uim_lisp func_ptr)
{
  if (SCM_C_FUNCPOINTERP((ScmObj)func_ptr)) {
    return SCM_C_FUNCPOINTER_VALUE((ScmObj)func_ptr);
  } else {
    uim_scm_error("uim_scm_c_func_ptr: C function pointer required but got ",
                  (uim_lisp)func_ptr);
    return NULL;
  }
}

uim_lisp
uim_scm_make_func_ptr(uim_func_ptr func_ptr)
{
  return (uim_lisp)Scm_NewCFuncPointer((ScmCFunc)func_ptr);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  SigScm_GC_Protect((ScmObj *)location);
}

void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  SigScm_GC_UnprotectStack((ScmObj*)stack_start);
}

#if UIM_SCM_GCC4_READY_GC
uim_lisp *
uim_scm_gc_protect_stack_internal(void)
{
  /*
   * &stack_start will be relocated to start of the frame of subsequent
   * function call
   */
  ScmObj stack_start;

  /* intentionally returns invalidated local address */
  return (uim_lisp *)SigScm_GC_ProtectStack(&stack_start);
}
#else /* UIM_SCM_GCC4_READY_GC */
void
uim_scm_gc_protect_stack(uim_lisp *stack_start)
{
  SigScm_GC_ProtectStack((ScmObj*)stack_start);
}
#endif /* UIM_SCM_GCC4_READY_GC */

uim_bool
uim_scm_is_alive(void)
{
  return (!uim_siod_fatal);
}

long
uim_scm_get_verbose_level(void)
{
  return (long)SigScm_GetVerboseLevel();
}

void
uim_scm_set_verbose_level(long new_value)
{
  SigScm_SetVerboseLevel(new_value);
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

  /* FIXME! */
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
  return (SCM_NULLP((ScmObj)obj));
}

uim_bool
uim_scm_consp(uim_lisp obj)
{
  return (SCM_CONSP((ScmObj)obj));
}

uim_bool
uim_scm_integerp(uim_lisp obj)
{
  return (SCM_INTP((ScmObj)obj));
}

uim_bool
uim_scm_stringp(uim_lisp obj)
{
  return (SCM_STRINGP((ScmObj)obj));
}

uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b)
{
  return (SCM_EQ((ScmObj)a, (ScmObj)b));
}

uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b)
{
  return (SCM_NFALSEP(ScmOp_stringequalp((ScmObj)a, (ScmObj)b)));
}

uim_lisp
uim_scm_eval(uim_lisp obj)
#if UIM_SCM_GCC4_READY_GC
{
  uim_lisp ret;

  UIM_SCM_GC_PROTECTED_CALL(ret, uim_lisp, uim_scm_eval_internal, (obj));

  return ret;
}

static uim_lisp
uim_scm_eval_internal(uim_lisp obj)
#endif
{
  uim_lisp ret;  /* intentionally outside of next stack_start */
#if !UIM_SCM_GCC4_READY_GC
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);
#endif

  ret = (uim_lisp)ScmOp_eval((ScmObj)obj, SCM_NULL);

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

  return ret;
}

#ifdef UIM_SCM_EXTENDED_API
uim_lisp
uim_scm_apply(uim_lisp proc, uim_lisp args)
{
  return (uim_lisp)Scm_call((ScmObj)proc, Scm_NewCons((ScmObj)args, SCM_NULL));
}
#endif  /* UIM_SCM_EXTENDED_API */

uim_lisp
uim_scm_eval_c_string(const char *str)
#if UIM_SCM_GCC4_READY_GC
{
  uim_lisp ret;

  UIM_SCM_GC_PROTECTED_CALL(ret, uim_lisp, uim_scm_eval_c_string_internal, (str));

  return ret;
}

static uim_lisp
uim_scm_eval_c_string_internal(const char *str)
#endif
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

uim_bool
uim_scm_require_file(const char *fn)
{
  uim_bool ret;

  if (!fn)
    return UIM_FALSE;

  UIM_EVAL_FSTRING1(NULL, "(guard (err (else #f)) (require \"%s\"))", fn);
  ret = uim_scm_c_bool(uim_scm_return_value());

  return ret;
}

typedef ScmObj (*ScmProcedureFixed0)(void);
typedef ScmObj (*ScmProcedureFixed1)(ScmObj arg1);
typedef ScmObj (*ScmProcedureFixed2)(ScmObj arg1, ScmObj arg2);
typedef ScmObj (*ScmProcedureFixed3)(ScmObj arg1, ScmObj arg2, ScmObj arg3);
typedef ScmObj (*ScmProcedureFixed4)(ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4);
typedef ScmObj (*ScmProcedureFixed5)(ScmObj arg1, ScmObj arg2, ScmObj arg3, ScmObj arg4, ScmObj arg5);

void
uim_scm_init_subr_0(const char *name, uim_lisp (*func)(void))
{
  Scm_RegisterProcedureFixed0(name, (ScmProcedureFixed0)func);
}

void
uim_scm_init_subr_1(const char *name, uim_lisp (*func)(uim_lisp))
{
  Scm_RegisterProcedureFixed1(name, (ScmProcedureFixed1)func);
}

void
uim_scm_init_subr_2(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp))
{
  Scm_RegisterProcedureFixed2(name, (ScmProcedureFixed2)func);
}

void
uim_scm_init_subr_3(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp))
{
  Scm_RegisterProcedureFixed3(name, (ScmProcedureFixed3)func);
}

void
uim_scm_init_subr_4(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp))
{
  Scm_RegisterProcedureFixed4(name, (ScmProcedureFixed4)func);
}

void
uim_scm_init_subr_5(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp))
{
  Scm_RegisterProcedureFixed5(name, (ScmProcedureFixed5)func);
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
  long vlevel = 2;
  ScmObj output_port;

  if (!uim_output)
    uim_output = stderr;

  if (verbose_level && isdigit(verbose_level[0])) {
    vlevel = atoi(verbose_level) % 10;
  }

  SigScm_Initialize();

  /* GC safe */
  output_port = Scm_MakeSharedFilePort(uim_output, "uim", SCM_PORTFLAG_OUTPUT);
  scm_current_output_port = scm_current_error_port = output_port;

#ifdef DEBUG_SCM
  /* required by test-im.scm */
  uim_scm_provide("debug");
#endif

  Scm_Use("srfi-23");
  Scm_Use("srfi-34");
  Scm_Use("siod");

  true_sym  = (uim_lisp)SCM_TRUE;
  false_sym = (uim_lisp)SCM_FALSE;
  protected_arg0 = uim_scm_f();

  uim_scm_set_verbose_level(vlevel);
}

void
uim_scm_quit(void)
{
  SigScm_Finalize();
  uim_output = NULL;
}
