/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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

                                                2004-12-21 YamaKen
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

#if 1
/* will be deprecated. use uim_scm_t() and uim_scm_f() for new design */
extern uim_lisp true_sym;
extern uim_lisp false_sym;
#endif

#define TRUEP(x) (uim_scm_eq(x, true_sym))
#define FALSEP(x) (uim_scm_eq(x, false_sym))
 
#define NTRUEP(x) (!uim_scm_eq(x, true_sym))
#define NFALSEP(x) (!uim_scm_eq(x, false_sym))


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
void
uim_scm_gc_protect_stack(uim_lisp *stack_start);
void
uim_scm_gc_unprotect_stack(uim_lisp *stack_start);

/* evaluations */
uim_bool
uim_scm_load_file(const char *fn);
uim_bool
uim_scm_require_file(const char *fn);
uim_lisp
uim_scm_eval(uim_lisp obj);
uim_lisp
uim_scm_eval_c_string(const char *str);
uim_lisp
uim_scm_return_value(void);
/*
  TODO: reorganize UIM_EVAL_FSTRINGn(), uim_sizeof_sexp_str() and
  uim_eval_string() in context.h into this file
*/

/* type conversions */
uim_bool
uim_scm_c_bool(uim_lisp val);
uim_lisp
uim_scm_make_bool(uim_bool val);

int
uim_scm_c_int(uim_lisp integer);
uim_lisp
uim_scm_make_int(int integer);

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

void
uim_scm_init_subr_0(char *name, uim_lisp (*fcn)(void));
void
uim_scm_init_subr_1(char *name, uim_lisp (*fcn)(uim_lisp));
void
uim_scm_init_subr_2(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp));
void
uim_scm_init_subr_3(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp));
void
uim_scm_init_subr_4(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
		   				 uim_lisp));
void
uim_scm_init_subr_5(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp));

/* constants */
uim_lisp
uim_scm_t(void);
uim_lisp
uim_scm_f(void);
uim_lisp
uim_scm_null_list(void);

/* predicates */
uim_bool
uim_scm_nullp(uim_lisp obj);
uim_bool
uim_scm_consp(uim_lisp obj);
uim_bool
uim_scm_integerp(uim_lisp obj);
uim_bool
uim_scm_stringp(uim_lisp obj);
uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b);
uim_bool
uim_scm_string_equal(uim_lisp a, uim_lisp b);

/* list operations */
uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr);
uim_lisp
uim_scm_car(uim_lisp cell);
uim_lisp
uim_scm_cdr(uim_lisp cell);
uim_lisp
uim_scm_cadr(uim_lisp cell);
uim_lisp
uim_scm_caar(uim_lisp cell);
uim_lisp
uim_scm_cdar(uim_lisp cell);
uim_lisp
uim_scm_cddr(uim_lisp cell);

#ifdef __cplusplus
}
#endif
#endif
