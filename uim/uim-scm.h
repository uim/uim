/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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


#ifndef UIM_SCM_H
#define UIM_SCM_H

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#define UIM_SCM_USE_DEPRECATED_API 1


#define UIM_SCM_FOR_EACH(elm, lst)					\
  while (uim_scm_consp(lst)						\
	 && ((elm) = uim_scm_car(lst), (lst) = uim_scm_cdr(lst), 1))


#ifndef UIM_BOOL_DEFINED
/*
 * A boolean type for uim to explicitly indicate intention about values.
 *
 *                           *** IMPORTANT ***
 *
 * Do not test a value with (val == UIM_TRUE). The UIM_TRUE is only A TYPICAL
 * VALUE FOR TRUE. Use (val) or (val != UIM_FALSE) instead.
 *
 */
typedef int uim_bool;

#define UIM_FALSE 0
#define UIM_TRUE 1

#define UIM_BOOL_DEFINED 1
#endif /* UIM_BOOL_DEFINED */

/*
  uim companion tools should treat lisp object as opaque. struct
  uim_opaque exists only for type check and has no actual definition.
*/
typedef struct uim_opaque * uim_lisp;
typedef void (*uim_func_ptr)(void);
typedef void *(*uim_gc_gate_func_ptr)(void *);


/* subsystem interfaces */
/* uim_scm_init(), uim_scm_quit() and uim_scm_set_fatal_error_hook() are
 * called from libuim internal. Ordinary user must not call it directly. */
void uim_scm_init(const char *system_load_path);
void uim_scm_quit(void);
uim_bool uim_scm_is_initialized(void);
void uim_scm_set_fatal_error_hook(void (*hook)(void));
void uim_scm_set_lib_path(const char *path);

/* GC protections */
void uim_scm_gc_protect(uim_lisp *location);
void uim_scm_gc_unprotect(uim_lisp *location);
void *uim_scm_call_with_gc_ready_stack(uim_gc_gate_func_ptr func, void *arg);
uim_bool uim_scm_gc_protectedp(uim_lisp obj);
uim_bool uim_scm_gc_protected_contextp(void);
/* for semantic assertions */
#define uim_scm_gc_any_contextp()					\
  (uim_scm_is_initialized()						\
   && (!uim_scm_gc_protected_contextp() || uim_scm_gc_protected_contextp()))

/* errors: can be caught by SRFI-34 'guard' */
void uim_scm_error(const char *msg);
void uim_scm_error_obj(const char *msg, uim_lisp errobj);
#define UIM_SCM_ENSURE(cond, msg)					\
  ((cond) || (uim_scm_error(msg), UIM_TRUE))
#define UIM_SCM_ENSURE_OBJ(cond, msg, obj)				\
  ((cond) || (uim_scm_error_obj((msg), (obj)), UIM_TRUE))
#define UIM_SCM_ENSURE_TYPE(type, obj)					\
  UIM_SCM_ENSURE_OBJ(uim_scm_##type##p(obj), #type " required but got", (obj))

/* evaluations */
uim_lisp uim_scm_symbol_value     (const char *symbol_str);
uim_bool uim_scm_symbol_value_bool(const char *symbol_str);
long     uim_scm_symbol_value_int (const char *symbol_str);
char    *uim_scm_symbol_value_str (const char *symbol_str);

uim_lisp uim_scm_eval(uim_lisp obj);
uim_lisp uim_scm_eval_c_string(const char *str);

uim_lisp uim_scm_call(uim_lisp proc, uim_lisp args);
uim_lisp uim_scm_call_with_guard(uim_lisp failed,
                                 uim_lisp proc, uim_lisp args);
uim_lisp uim_scm_callf(const char *proc, const char *args_fmt, ...);
uim_lisp uim_scm_callf_with_guard(uim_lisp failed,
                                  const char *proc, const char *args_fmt, ...);

uim_bool uim_scm_load_file(const char *fn);
uim_bool uim_scm_require_file(const char *fn);

/* type conversions */
long uim_scm_c_bool(uim_lisp val);
uim_lisp uim_scm_make_bool(long val);

long uim_scm_c_int(uim_lisp integer);
uim_lisp uim_scm_make_int(long integer);

long uim_scm_c_char(uim_lisp ch);
uim_lisp uim_scm_make_char(long ch);

char *uim_scm_c_str(uim_lisp str);
const char *uim_scm_refer_c_str(uim_lisp str);
uim_lisp uim_scm_make_str(const char *str);
uim_lisp uim_scm_make_str_directly(char *str);

char *uim_scm_c_symbol(uim_lisp str);
uim_lisp uim_scm_make_symbol(const char *str);

void *uim_scm_c_ptr(uim_lisp ptr);
void uim_scm_nullify_c_ptr(uim_lisp ptr);
uim_lisp uim_scm_make_ptr(void *ptr);

uim_func_ptr uim_scm_c_func_ptr(uim_lisp func_ptr);
uim_lisp uim_scm_make_func_ptr(uim_func_ptr func_ptr);

/* C array <-> Scheme list converters */
uim_lisp uim_scm_array2list(void **ary, size_t len, uim_lisp (*conv)(void *));
void **uim_scm_list2array(uim_lisp lst, size_t *len, void *(*conv)(uim_lisp));

/* C array <-> Scheme vector converters */
uim_lisp uim_scm_array2vector(void **ary, size_t len, uim_lisp (*conv)(void *));
void **uim_scm_vector2array(uim_lisp vec, size_t *len, void *(*conv)(uim_lisp));

/* procedure initializers */
void uim_scm_init_proc0(const char *name, uim_lisp (*func)(void));
void uim_scm_init_proc1(const char *name, uim_lisp (*func)(uim_lisp));
void uim_scm_init_proc2(const char *name,
			uim_lisp (*func)(uim_lisp, uim_lisp));
void uim_scm_init_proc3(const char *name,
			uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp));
void uim_scm_init_proc4(const char *name,
			uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
					 uim_lisp));
void uim_scm_init_proc5(const char *name,
			uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp,
					 uim_lisp, uim_lisp));


/* predicates */
uim_bool uim_scm_truep(uim_lisp obj);  /* (if obj #t #f) */
uim_bool uim_scm_falsep(uim_lisp obj);
uim_bool uim_scm_nullp(uim_lisp obj);
uim_bool uim_scm_consp(uim_lisp obj);
uim_bool uim_scm_listp(uim_lisp obj);  /* does not detect circular list */
uim_bool uim_scm_intp(uim_lisp obj);
uim_bool uim_scm_charp(uim_lisp obj);
uim_bool uim_scm_vectorp(uim_lisp obj);
uim_bool uim_scm_strp(uim_lisp obj);
uim_bool uim_scm_symbolp(uim_lisp obj);
uim_bool uim_scm_ptrp(uim_lisp obj);
uim_bool uim_scm_func_ptrp(uim_lisp obj);
uim_bool uim_scm_eq(uim_lisp a, uim_lisp b);

/* constants */
uim_lisp uim_scm_t(void);
uim_lisp uim_scm_f(void);
uim_lisp uim_scm_null(void);
uim_lisp uim_scm_eof(void);

/* list constructors */
uim_lisp uim_scm_quote(uim_lisp obj);
uim_lisp uim_scm_list1(uim_lisp elm1);
uim_lisp uim_scm_list2(uim_lisp elm1, uim_lisp elm2);
uim_lisp uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3);
uim_lisp uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
                       uim_lisp elm4);
uim_lisp uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3,
                       uim_lisp elm4, uim_lisp elm5);

/* list operations */
uim_lisp uim_scm_cons(uim_lisp car, uim_lisp cdr);
uim_lisp uim_scm_car(uim_lisp pair);
uim_lisp uim_scm_cdr(uim_lisp pair);
void uim_scm_set_car(uim_lisp pair, uim_lisp car);
void uim_scm_set_cdr(uim_lisp pair, uim_lisp cdr);

long uim_scm_length(uim_lisp lst);

/* vector operations */
uim_lisp uim_scm_vector_ref(uim_lisp vec, long i);
void uim_scm_vector_set(uim_lisp vec, long i, uim_lisp elm);
long uim_scm_vector_length(uim_lisp vec);


#if UIM_SCM_USE_DEPRECATED_API
/* deprecated: replace with uim_scm_falsep() and uim_scm_truep(), or
 * FALSEP() and TRUEP() in uim-scm-abbrev.h */
#define UIM_SCM_FALSEP(x)  (uim_scm_falsep(x))
#define UIM_SCM_NFALSEP(x) (uim_scm_truep(x))

#define uim_scm_integerp  uim_scm_intp
#define uim_scm_stringp   uim_scm_strp
#define uim_scm_null_list uim_scm_null

#define uim_scm_init_subr_0 uim_scm_init_proc0
#define uim_scm_init_subr_1 uim_scm_init_proc1
#define uim_scm_init_subr_2 uim_scm_init_proc2
#define uim_scm_init_subr_3 uim_scm_init_proc3
#define uim_scm_init_subr_4 uim_scm_init_proc4
#define uim_scm_init_subr_5 uim_scm_init_proc5

#endif /* UIM_SCM_USE_DEPRECATED_API */


#ifdef __cplusplus
}
#endif
#endif
