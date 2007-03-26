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

/*                         !!CAUTION!!

   This API is designed for input method plugin and internal uim
   implementation. Other developers should not use this API since the
   API easily causes fatal crash involving GC if you does not pay
   attention enough. Be careful.

   This API is not intended to provide all R5RS features. Only 'core'
   ones to write Scheme-C adapters should be added. Consider how
   frequently it will be used, and whether it should be written by C,
   when you add an API function.

                                                2005-01-10 YamaKen
*/


/* API and ABI are unstable */
#ifndef _uim_scm_h_included_
#define _uim_scm_h_included_

#include <stdio.h>
#include "uim.h"

#ifdef __cplusplus
extern "C" {
#endif


/*
  uim companion tools should treat lisp object as opaque. struct
  uim_opaque exists only for type check and has no actual definition.
*/
typedef struct uim_opaque * uim_lisp;
typedef void (*uim_func_ptr)(void);
typedef void *(*uim_gc_gate_func_ptr)(void *);

#define UIM_SCM_FALSEP(x)  (uim_scm_eq((x), uim_scm_f()))
#define UIM_SCM_NFALSEP(x) (!uim_scm_eq((x), uim_scm_f()))

#if 1
/* deprecated: replace with UIM_SCM_N?FALSEP() */
#define FALSEP(x)  (UIM_SCM_FALSEP(x))
#define NFALSEP(x) (UIM_SCM_NFALSEP(x))
#define TRUEP(x) (uim_scm_eq(x, uim_scm_t()))
#define NTRUEP(x) (!uim_scm_eq(x, uim_scm_t()))
#endif


/* 'uim_scm' prefix is not appropriate for these functions... any ideas? */
FILE *
uim_scm_get_output(void);
void
uim_scm_set_output(FILE *fp);
  
uim_bool
uim_scm_is_alive(void);
long
uim_scm_get_verbose_level(void);
void
uim_scm_set_verbose_level(long new_value);
void
uim_scm_set_lib_path(const char *path);

void
uim_scm_gc_protect(uim_lisp *location);
void uim_scm_gc_unprotect(uim_lisp *location);
void *
uim_scm_call_with_gc_ready_stack(uim_gc_gate_func_ptr func, void *arg);
uim_bool uim_scm_gc_protectedp(uim_lisp obj);
uim_bool uim_scm_gc_protected_contextp(void);
/* for semantic assertions */
#define uim_scm_gc_any_contextp()                                            \
  (!uim_scm_gc_protected_contextp() || uim_scm_gc_protected_contextp())

/* evaluations */
uim_lisp uim_scm_symbol_value(const char *symbol_str);
uim_bool uim_scm_symbol_value_bool(const char *symbol_str);
int uim_scm_symbol_value_int(const char *symbol_str);
char *uim_scm_symbol_value_str(const char *symbol_str);

uim_bool
uim_scm_load_file(const char *fn);
uim_bool
uim_scm_require_file(const char *fn);
uim_lisp
uim_scm_eval(uim_lisp obj);
uim_lisp
uim_scm_eval_c_string(const char *str);

uim_lisp uim_scm_call(uim_lisp proc, uim_lisp args);
uim_lisp uim_scm_call_with_guard(uim_lisp failed,
                                 uim_lisp proc, uim_lisp args);
uim_lisp uim_scm_callf(const char *proc, const char *args_fmt, ...);
uim_lisp uim_scm_callf_with_guard(uim_lisp failed,
                                  const char *proc, const char *args_fmt, ...);

void uim_scm_ensure(uim_bool cond);

/* type conversions */
uim_bool
uim_scm_c_bool(uim_lisp val);
uim_lisp
uim_scm_make_bool(uim_bool val);

long uim_scm_c_int(uim_lisp integer);
uim_lisp uim_scm_make_int(long integer);

char *
uim_scm_c_str(uim_lisp str);
const char *
uim_scm_refer_c_str(uim_lisp str);
uim_lisp
uim_scm_make_str(const char *str);

char *
uim_scm_c_symbol(uim_lisp str);
uim_lisp
uim_scm_make_symbol(const char *str);

void *
uim_scm_c_ptr(uim_lisp ptr);
uim_lisp
uim_scm_make_ptr(void *ptr);

uim_func_ptr
uim_scm_c_func_ptr(uim_lisp func_ptr);
uim_lisp
uim_scm_make_func_ptr(uim_func_ptr func_ptr);

void
uim_scm_init_subr_0(const char *name, uim_lisp (*fcn)(void));
void 
uim_scm_init_subr_1(const char *name, uim_lisp (*fcn)(uim_lisp));
void
uim_scm_init_subr_2(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp));
void
uim_scm_init_subr_3(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp));
void
uim_scm_init_subr_4(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
		   				 uim_lisp));
void
uim_scm_init_subr_5(const char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp));
void uim_scm_init_fsubr(const char *name,
                        uim_lisp (*fcn)(uim_lisp args, uim_lisp env));


/* constants */
uim_lisp
uim_scm_t(void);
uim_lisp
uim_scm_f(void);
uim_lisp uim_scm_null(void);
#define uim_scm_null_list uim_scm_null

/* list constructors */
uim_lisp uim_scm_quote(uim_lisp obj);
uim_lisp uim_scm_list1(uim_lisp elm1);
uim_lisp uim_scm_list2(uim_lisp elm1, uim_lisp elm2);
uim_lisp uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3);
uim_lisp uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
                       uim_lisp elm4);
uim_lisp uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
                       uim_lisp elm4, uim_lisp elm5);

/* predicates */
uim_bool
uim_scm_nullp(uim_lisp obj);
uim_bool
uim_scm_consp(uim_lisp obj);
uim_bool
uim_scm_integerp(uim_lisp obj);
uim_bool
uim_scm_stringp(uim_lisp obj);
uim_bool uim_scm_symbolp(uim_lisp obj);
uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b);

/* list operations */
uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr);
uim_lisp
uim_scm_car(uim_lisp pair);
uim_lisp
uim_scm_cdr(uim_lisp pair);

long uim_scm_length(uim_lisp lst);


#ifdef __cplusplus
}
#endif
#endif
