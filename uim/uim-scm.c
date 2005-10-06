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
#include "siod.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-internal.h"

/*
  To avoid namespace pollution, all siod functions are defined as
  static and wrapped into uim-scm.c by direct inclusion rather than
  linked via public symbols. After elaboration of uim-scm API, the
  Scheme interpreter implementation can be switched to another one
  such as uim-scm-tinyscheme.c or uim-scm-gauche.c. But *.[hc] under
  uim/ and *.scm are still depending on siod in several ways. At least
  full test suite for *.scm files are required to migrate to another
  Scheme implementation.  -- YamaKen 2004-12-21, 2005-01-10
*/
#include "slib.c"
#ifdef UIM_COMPAT_SCM
#include "uim-compat-scm.c"
#endif

static void siod_init_subr(const char *name, long type, SUBR_FUNC fcn);

#if UIM_SCM_GCC4_READY_GC
static int uim_scm_c_int_internal(uim_lisp integer);
static const char *uim_scm_refer_c_str_internal(uim_lisp str);
static uim_lisp uim_scm_eval_internal(uim_lisp obj);
static void siod_init_subr_internal(const char *name, long type, SUBR_FUNC fcn);
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

  protected_arg0 = integer;
  c_int = get_c_int((LISP)integer);

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

  return c_int;
}

uim_lisp
uim_scm_make_int(int integer)
{
  return (uim_lisp)intcons(integer);
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

  protected_arg0 = str;
  c_str = get_c_string((LISP)str);

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

  return c_str;
}

uim_lisp
uim_scm_make_str(const char *str)
{
  int unknown_strlen = -1;
  return (uim_lisp)strcons(unknown_strlen, str);
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  /* siod dependent */
  return uim_scm_c_str(symbol);
}

uim_lisp
uim_scm_make_symbol(const char *str)
{
  return (uim_lisp)rintern(str);
}

void *
uim_scm_c_ptr(uim_lisp ptr)
{
  return get_c_pointer((LISP)ptr);
}

uim_lisp
uim_scm_make_ptr(void *ptr)
{
  return (uim_lisp)ptrcons(ptr);
}

uim_func_ptr
uim_scm_c_func_ptr(uim_lisp func_ptr)
{
  return get_c_func_pointer((LISP)func_ptr);
}

uim_lisp
uim_scm_make_func_ptr(uim_func_ptr func_ptr)
{
  return (uim_lisp)funcptrcons(func_ptr);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  siod_gc_protect((LISP *)location);
}

void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start)
{
  siod_gc_unprotect_stack((LISP *)stack_start);
}

#if UIM_SCM_GCC4_READY_GC
uim_lisp *
uim_scm_gc_protect_stack_internal(void)
{
  /*
   * &stack_start will be relocated to start of the frame of subsequent
   * function call
   */
  LISP stack_start;

  siod_gc_protect_stack(&stack_start);

  /* intentionally returns invalidated local address */
  return (uim_lisp *)&stack_start;
}
#else /* UIM_SCM_GCC4_READY_GC */
void
uim_scm_gc_protect_stack(uim_lisp *stack_start)
{
  siod_gc_protect_stack((LISP *)stack_start);
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
  return siod_verbose_level;
}

void
uim_scm_set_verbose_level(long new_value)
{
  siod_verbose_level = new_value;
}

void
uim_scm_set_lib_path(const char *path)
{
  siod_set_lib_path(path);
}

uim_bool
uim_scm_load_file(const char *fn)
{
  uim_bool succeeded;

  if (!fn)
    return UIM_FALSE;

  UIM_EVAL_FSTRING1(NULL, "(*catch 'errobj (load \"%s\" #f #f))", fn);
  succeeded = FALSEP(uim_scm_return_value()); /* has not been caught */

  return succeeded;
}

uim_lisp
uim_scm_t(void)
{
  return (uim_lisp)true_sym;
}

uim_lisp
uim_scm_f(void)
{
  return (uim_lisp)false_sym;
}

uim_lisp
uim_scm_null_list(void)
{
  return (uim_lisp)NIL;
}

uim_bool
uim_scm_nullp(uim_lisp obj)
{
  return NULLP((LISP)obj);
}

uim_bool
uim_scm_consp(uim_lisp obj)
{
  return CONSP((LISP)obj);
}

uim_bool
uim_scm_integerp(uim_lisp obj)
{
  return INTNUMP((LISP)obj);
}

uim_bool
uim_scm_stringp(uim_lisp obj)
{
  return STRINGP((LISP)obj);
}

uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b)
{
  return EQ(a, b);
}

uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b)
{
  return NFALSEP((uim_lisp)string_equal((LISP)a, (LISP)b));
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

  ret = (uim_lisp)leval((LISP)obj, NIL);

#if !UIM_SCM_GCC4_READY_GC
  uim_scm_gc_unprotect_stack(&stack_start);
#endif

  return ret;
}

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
  repl_c_string((char *)str, 0, 0);
  return uim_scm_return_value();
}

uim_lisp
uim_scm_return_value(void)
{
  return (uim_lisp)siod_return_value();
}

uim_lisp
uim_scm_car(uim_lisp cell)
{
  return (uim_lisp)car((LISP)cell);
}

uim_lisp
uim_scm_cdr(uim_lisp cell)
{
  return (uim_lisp)cdr((LISP)cell);
}

uim_lisp
uim_scm_cadr(uim_lisp cell)
{
  return (uim_lisp)cadr((LISP)cell);
}

uim_lisp
uim_scm_caar(uim_lisp cell)
{
  return (uim_lisp)caar((LISP)cell);
}

uim_lisp
uim_scm_cdar(uim_lisp cell)
{
  return (uim_lisp)cdar((LISP)cell);
}

uim_lisp
uim_scm_cddr(uim_lisp cell)
{
  return (uim_lisp)cddr((LISP)cell);
}

uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr)
{
  return (uim_lisp)cons((LISP)car, (LISP)cdr);
}

uim_lisp
uim_scm_length(uim_lisp list)
{
  /*
    although nlength() of siod returns length of anything, this
    function should be called only for list
  */
  return (uim_lisp)uim_scm_make_int(nlength((LISP)list));
}

uim_lisp
uim_scm_reverse(uim_lisp cell)
{
  return (uim_lisp)reverse((LISP)cell);
}

uim_bool
uim_scm_require_file(const char *fn)
{
  uim_bool succeeded;

  if (!fn)
    return UIM_FALSE;

  UIM_EVAL_FSTRING2(NULL, "(eq? '*%s-loaded* (*catch 'errobj (require \"%s\")))", fn, fn);
  succeeded = uim_scm_c_bool(uim_scm_return_value());

  return succeeded;
}

static void
siod_init_subr(const char *name, long type, SUBR_FUNC fcn)
#if UIM_SCM_GCC4_READY_GC
{
  UIM_SCM_GC_PROTECTED_CALL_VOID(siod_init_subr_internal, (name, type, fcn));
}

static void
siod_init_subr_internal(const char *name, long type, SUBR_FUNC fcn)
{
  init_subr(name, type, fcn);
}
#else
{
  uim_lisp stack_start;

  uim_scm_gc_protect_stack(&stack_start);
  init_subr(name, type, fcn);
  uim_scm_gc_unprotect_stack(&stack_start);
}
#endif

void
uim_scm_init_subr_0(const char *name, uim_lisp (*fcn)(void))
{
  siod_init_subr(name, tc_subr_0, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_1(const char *name, uim_lisp (*fcn)(uim_lisp))
{
  siod_init_subr(name, tc_subr_1, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_2(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp))
{
  siod_init_subr(name, tc_subr_2, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_3(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp))
{
  siod_init_subr(name, tc_subr_3, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_4(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp))
{
  siod_init_subr(name, tc_subr_4, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_5(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp))
{
  siod_init_subr(name, tc_subr_5, (SUBR_FUNC)fcn);
}

static void
exit_hook(void)
{
  uim_siod_fatal = 1;
}

void
uim_scm_init(const char *verbose_level)
{
  char *siod_argv[] =
    {
      "siod",
      "-v0",          /* siod_verbose_level */
      "-h16384:64",   /* heap_size(unit: lisp objects):nheaps */
      "-t16384",      /* heap_alloc_threshold (unit: lisp objects) */
      "-o1024",       /* obarray_dim (hash size of symbol table) */
      "-s262144",     /* stack_size (unit: bytes) */
      "-n128"         /* inums_dim (preallocated fixnum objects) */
    };
  char verbose_argv[] = "-v4";
  int siod_argc, warnflag = 1;

  if (!uim_output) {
    uim_output = stderr;
  }

  if (verbose_level) {
    if (isdigit(verbose_level[0])) {
      if (isdigit(verbose_level[1]))
	verbose_argv[2] = '9';	/* SIOD's max verbose level is 5 */
      else
	verbose_argv[2] = verbose_level[0];
    }
    siod_argv[1] = verbose_argv;
  }
  /* init siod */
  siod_argc = sizeof(siod_argv) / sizeof(char *);
  siod_init(siod_argc, siod_argv, warnflag, uim_output);
  set_fatal_exit_hook(exit_hook);

  true_sym  = (uim_lisp)siod_true_value();
#if 0
  false_sym = (uim_lisp)siod_false_value();
#else
  /* false_sym has to be NIL until bug #617 and #642 are fixed
   * -- YamaKen
   */
  false_sym = (uim_lisp)NIL;
#endif
  uim_scm_gc_protect(&true_sym);
  uim_scm_gc_protect(&false_sym);

  protected_arg0 = uim_scm_f();
  uim_scm_gc_protect(&protected_arg0);
}

void
uim_scm_quit(void)
{
  siod_quit();
  uim_output = NULL;
}
