/*

  Copyright (c) 2003-2007 uim Project http://uim.freedesktop.org/

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

/*
 * To avoid namespace pollution, all SigScheme functions and variables
 * are defined as static and wrapped into uim-scm.c by direct
 * inclusion instead of being linked via public symbols.
 *   -- YamaKen 2004-12-21, 2005-01-10, 2006-04-02
 */
/* This file must be included before uim's config.h */
#include "sigscheme-combined.c"
#if !SSCM_VERSION_REQUIRE(0, 7, 3)
#error "SigScheme version 0.7.3 or later is required"
#endif

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "uim-stdint.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-internal.h"
/* To avoid macro name conflict with SigScheme, uim-scm-abbrev.h should not
 * be included. */

#ifdef UIM_COMPAT_SCM
#include "uim-compat-scm.c"
#endif

/* FIXME: illegal internal access */
#define scm_out SCM_GLOBAL_VAR(port, scm_out)
#define scm_err SCM_GLOBAL_VAR(port, scm_err)

static void uim_scm_error(const char *msg, uim_lisp errobj);

struct call_args {
  uim_lisp proc;
  uim_lisp args;
  uim_lisp failed;
};
static void *uim_scm_call_internal(struct call_args *args);
static void *uim_scm_call_with_guard_internal(struct call_args *args);

#if UIM_SCM_GCC4_READY_GC
struct uim_scm_error_args {
  const char *msg;
  uim_lisp errobj;
};
static void *uim_scm_error_internal(struct uim_scm_error_args *args);

static void *uim_scm_c_int_internal(void *uim_lisp_integer);
static const char *uim_scm_refer_c_str_internal(void *uim_lisp_str);
static void *uim_scm_eval_internal(void *uim_lisp_obj);
#endif

uim_lisp uim_scm_last_val;
static uim_bool sscm_is_exit_with_fatal_error;
static FILE *uim_output = NULL;

static void
uim_scm_error(const char *msg, uim_lisp errobj)
#if UIM_SCM_GCC4_READY_GC
{
  struct uim_scm_error_args args;

  args.msg = msg;
  args.errobj = errobj;
  uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_error_internal, &args);
}

static void *
uim_scm_error_internal(struct uim_scm_error_args *args)
{
  /* FIXME: don't terminate the process */
  scm_error_obj(NULL, args->msg, (ScmObj)args->errobj);
  return NULL;
}
#else /* UIM_SCM_GCC4_READY_GC */
static void
uim_scm_error_internal(const char *msg, uim_lisp errobj)
{
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);

  /* FIXME: don't terminate the process */
  scm_error_obj(NULL, msg, (ScmObj)errobj);

  uim_scm_gc_unprotect_stack(&stack_start);
}
#endif /* UIM_SCM_GCC4_READY_GC */

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

void
uim_scm_ensure(uim_bool cond)
{
  SCM_ENSURE(cond);
}

uim_bool
uim_scm_c_bool(uim_lisp val)
{
  return UIM_SCM_NFALSEP(val);
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
  return (int)(intptr_t)uim_scm_call_with_gc_ready_stack(uim_scm_c_int_internal, (void *)integer);
}

static void *
uim_scm_c_int_internal(void *uim_lisp_integer)
#endif
{
  int c_int;
#if UIM_SCM_GCC4_READY_GC
  uim_lisp integer;
#else
  uim_lisp stack_start;
#endif

#if UIM_SCM_GCC4_READY_GC
  integer = (uim_lisp)uim_lisp_integer;
#else
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

#if UIM_SCM_GCC4_READY_GC
  return (void *)(intptr_t)c_int;
#else
  uim_scm_gc_unprotect_stack(&stack_start);

  return c_int;
#endif
}

uim_lisp
uim_scm_make_int(int integer)
{
  return (uim_lisp)SCM_MAKE_INT(integer);
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
  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_refer_c_str_internal, (void *)str);
}

static const char *
uim_scm_refer_c_str_internal(void *uim_lisp_str)
#endif
{
  char *c_str;
#if UIM_SCM_GCC4_READY_GC
  uim_lisp str;
#else
  uim_lisp stack_start;
#endif

#if UIM_SCM_GCC4_READY_GC
  str = (uim_lisp)uim_lisp_str;
#else
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
  return (uim_lisp)SCM_MAKE_STRING_COPYING(str, SCM_STRLEN_UNKNOWN);
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  return strdup((char *)SCM_SYMBOL_NAME((ScmObj)symbol));
}

uim_lisp
uim_scm_make_symbol(const char *name)
{
  return (uim_lisp)scm_intern(name);
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
  return (uim_lisp)SCM_MAKE_C_POINTER(ptr);
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
  return (uim_lisp)SCM_MAKE_C_FUNCPOINTER((ScmCFunc)func_ptr);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  scm_gc_protect((ScmObj *)location);
}

#if UIM_SCM_GCC4_READY_GC
void *
uim_scm_call_with_gc_ready_stack(uim_gc_gate_func_ptr func, void *arg)
{
  return scm_call_with_gc_ready_stack(func, arg);
}
#else
void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  scm_gc_unprotect_stack((ScmObj*)stack_start);
}

void
uim_scm_gc_protect_stack(uim_lisp *stack_start)
{
  scm_gc_protect_stack((ScmObj*)stack_start);
}
#endif /* UIM_SCM_GCC4_READY_GC */

uim_bool
uim_scm_is_alive(void)
{
  return (!sscm_is_exit_with_fatal_error);
}

long
uim_scm_get_verbose_level(void)
{
  return (long)scm_get_verbose_level();
}

void
uim_scm_set_verbose_level(long new_value)
{
  scm_set_verbose_level(new_value);
}

void
uim_scm_set_lib_path(const char *path)
{
  scm_set_lib_path(path);
}

uim_bool
uim_scm_load_file(const char *fn)
{
  uim_lisp ok;

  if (!fn)
    return UIM_FALSE;

  /* (guard (err (else #f)) (load "<fn>")) */
  ok = uim_scm_call_with_guard(uim_scm_f(),
                               uim_scm_make_symbol("load"),
                               uim_scm_list1(uim_scm_make_str(fn)));

  return uim_scm_c_bool(ok);
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
uim_scm_symbolp(uim_lisp obj)
{
  return (SCM_SYMBOLP((ScmObj)obj));
}

uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b)
{
  return (SCM_EQ((ScmObj)a, (ScmObj)b));
}

uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b)
{
  return (SCM_TRUEP(scm_p_stringequalp((ScmObj)a, (ScmObj)b)));
}

uim_lisp
uim_scm_eval(uim_lisp obj)
#if UIM_SCM_GCC4_READY_GC
{
  return (uim_lisp)uim_scm_call_with_gc_ready_stack(uim_scm_eval_internal,
						    (void *)obj);
}

static void *
uim_scm_eval_internal(void *uim_lisp_obj)
#endif
{
  uim_lisp ret;  /* intentionally outside of next stack_start */
#if UIM_SCM_GCC4_READY_GC
  uim_lisp obj;
#else
  uim_lisp stack_start;
#endif

#if UIM_SCM_GCC4_READY_GC
  obj = (uim_lisp)uim_lisp_obj;
#else
  uim_scm_gc_protect_stack(&stack_start);
#endif

  uim_scm_last_val = ret = (uim_lisp)scm_p_eval((ScmObj)obj, SCM_NULL);

#if UIM_SCM_GCC4_READY_GC
  return (void *)ret;
#else
  uim_scm_gc_unprotect_stack(&stack_start);

  return ret;
#endif
}

uim_lisp
uim_scm_eval_c_string(const char *str)
{
  uim_scm_last_val = (uim_lisp)scm_eval_c_string(str);

  return uim_scm_last_val;
}

uim_lisp
uim_scm_call0(uim_lisp proc)
{
  return uim_scm_call(proc, uim_scm_null());
}

uim_lisp
uim_scm_call1(uim_lisp proc, uim_lisp arg1)
{
  return uim_scm_call(proc, uim_scm_list1(arg1));
}

uim_lisp
uim_scm_call2(uim_lisp proc, uim_lisp arg1, uim_lisp arg2)
{
  return uim_scm_call(proc, uim_scm_list2(arg1, arg2));
}

uim_lisp
uim_scm_call3(uim_lisp proc, uim_lisp arg1, uim_lisp arg2, uim_lisp arg3)
{
  return uim_scm_call(proc, uim_scm_list3(arg1, arg2, arg3));
}

uim_lisp
uim_scm_call(uim_lisp proc, uim_lisp args)
{
  struct call_args _args;

  _args.proc = proc;
  _args.args = args;
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_call_internal, &_args);
}

static void *
uim_scm_call_internal(struct call_args *args)
{
  if (uim_scm_symbolp(args->proc))
    args->proc = uim_scm_eval(args->proc);

  return (void *)scm_call((ScmObj)args->proc, (ScmObj)args->args);
}

uim_lisp
uim_scm_call_with_guard(uim_lisp failed, uim_lisp proc, uim_lisp args)
{
  struct call_args _args;

  _args.failed = failed;
  _args.proc = proc;
  _args.args = args;
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_call_with_guard_internal, &_args);
}

static void *
uim_scm_call_with_guard_internal(struct call_args *args)
{
  uim_lisp form;

  /* (guard (err (else '<failed>)) (apply <proc> '<args>)) */
  form = uim_scm_list3(uim_scm_make_symbol("guard"),
                       uim_scm_list2(uim_scm_make_symbol("err"),
                                     uim_scm_list2(uim_scm_make_symbol("else"),
                                                   uim_scm_quote(args->failed))),
                       uim_scm_list3(uim_scm_make_symbol("apply"),
                                     args->proc,
                                     uim_scm_quote(args->args)));

  return (void *)uim_scm_eval(form);
}

uim_lisp
uim_scm_return_value(void)
{
  /* FIXME: This function should be removed. */
  return uim_scm_last_val;
}

uim_lisp
uim_scm_car(uim_lisp pair)
{
  return (uim_lisp)scm_p_car((ScmObj)pair);
}

uim_lisp
uim_scm_cdr(uim_lisp pair)
{
  return (uim_lisp)scm_p_cdr((ScmObj)pair);
}

uim_lisp
uim_scm_cadr(uim_lisp lst)
{
  return (uim_lisp)scm_p_cadr((ScmObj)lst);
}

uim_lisp
uim_scm_caar(uim_lisp lst)
{
  return (uim_lisp)scm_p_caar((ScmObj)lst);
}

uim_lisp
uim_scm_cdar(uim_lisp lst)
{
  return (uim_lisp)scm_p_cdar((ScmObj)lst);
}

uim_lisp
uim_scm_cddr(uim_lisp lst)
{
  return (uim_lisp)scm_p_cddr((ScmObj)lst);
}

uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr)
{
  return (uim_lisp)SCM_CONS((ScmObj)car, (ScmObj)cdr);
}

long
uim_scm_length(uim_lisp lst)
{
  uim_lisp len;

  len = (uim_lisp)scm_p_length((ScmObj)lst);
  return uim_scm_c_int(len);
}

uim_lisp
uim_scm_reverse(uim_lisp lst)
{
  return (uim_lisp)scm_p_reverse((ScmObj)lst);
}

uim_bool
uim_scm_require_file(const char *fn)
{
  uim_lisp ok;

  if (!fn)
    return UIM_FALSE;

  /* (guard (err (else #f)) (load "<fn>")) */
  ok = uim_scm_call_with_guard(uim_scm_f(),
                               uim_scm_make_symbol("require"),
                               uim_scm_list1(uim_scm_make_str(fn)));

  return uim_scm_c_bool(ok);
}

void
uim_scm_init_subr_0(const char *name, uim_lisp (*func)(void))
{
  scm_register_func(name, (scm_procedure_fixed_0)func, SCM_PROCEDURE_FIXED_0);
}

void
uim_scm_init_subr_1(const char *name, uim_lisp (*func)(uim_lisp))
{
  scm_register_func(name, (scm_procedure_fixed_1)func, SCM_PROCEDURE_FIXED_1);
}

void
uim_scm_init_subr_2(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp))
{
  scm_register_func(name, (scm_procedure_fixed_2)func, SCM_PROCEDURE_FIXED_2);
}

void
uim_scm_init_subr_3(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp))
{
  scm_register_func(name, (scm_procedure_fixed_3)func, SCM_PROCEDURE_FIXED_3);
}

void
uim_scm_init_subr_4(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
                                                       uim_lisp))
{
  scm_register_func(name, (scm_procedure_fixed_4)func, SCM_PROCEDURE_FIXED_4);
}

void
uim_scm_init_subr_5(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
                                                       uim_lisp, uim_lisp))
{
  scm_register_func(name, (scm_procedure_fixed_5)func, SCM_PROCEDURE_FIXED_5);
}

static void
exit_hook(void)
{
  sscm_is_exit_with_fatal_error = UIM_TRUE;
  /* FIXME: Add longjmp() to outermost uim API call, and make all API
   * calls uim_scm_is_alive()-sensitive. It should be fixed on uim
   * 1.5.  -- YamaKen 2006-06-06, 2006-12-27 */
}

void
uim_scm_init(const char *verbose_level)
{
  ScmStorageConf storage_conf;
  long vlevel = 2;
  ScmObj output_port;

  if (!uim_output)
    uim_output = stderr;

  if (verbose_level && isdigit(verbose_level[0])) {
    vlevel = atoi(verbose_level) % 10;
  }

#if SCM_USE_MULTIBYTE_CHAR
  /* *GC safe operation*
   * 
   * Set the raw unibyte codec which accepts all (multi)byte sequence
   * although it slashes a multibyte character on Scheme-level
   * character processing. Since current uim implementation treats a
   * multibyte character as string, it is not a problem. The name
   * "ISO-8859-1" is a dummy name for the codec.
   */
  scm_current_char_codec = scm_mb_find_codec("ISO-8859-1");
#endif

  /* 128KB/heap, max 0.99GB on 32-bit systems. Since maximum length of list can
   * be represented by a Scheme integer, SCM_INT_MAX limits the number of cons
   * cells. */
  storage_conf.heap_size            = 16384;
  storage_conf.heap_alloc_threshold = 16384;
  storage_conf.n_heaps_max          = SCM_INT_MAX / storage_conf.heap_size;
  storage_conf.n_heaps_init         = 1;
  storage_conf.symbol_hash_size     = 1024;
  scm_initialize(&storage_conf);
  scm_set_fatal_error_callback(exit_hook);

  /* GC safe */
  output_port = scm_make_shared_file_port(uim_output, "uim", SCM_PORTFLAG_OUTPUT);
  scm_out = scm_err = output_port;

#ifdef DEBUG_SCM
  /* required by test-im.scm */
  uim_scm_provide("debug");
#endif

  scm_use("srfi-23");
  scm_use("srfi-34");
  scm_use("siod");

  uim_scm_gc_protect(&uim_scm_last_val);
  uim_scm_set_verbose_level(vlevel);
}

void
uim_scm_quit(void)
{
  scm_finalize();
  sscm_is_exit_with_fatal_error = UIM_FALSE;
  uim_output = NULL;
}
