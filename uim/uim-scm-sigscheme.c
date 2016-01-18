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

/*
 * To avoid namespace pollution, all SigScheme functions and variables
 * are defined as static and wrapped into uim-scm.c by direct
 * inclusion instead of being linked via public symbols.
 *   -- YamaKen 2004-12-21, 2005-01-10, 2006-04-02
 */
/* This file must be included before uim's config.h */
#include "sigscheme-combined.c"
#if !SSCM_VERSION_REQUIRE(0, 8, 5)
#error "SigScheme version 0.8.5 or later is required"
#endif

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <assert.h>

#include "uim-scm.h"
/* To avoid macro name conflict with SigScheme, uim-scm-abbrev.h should not
 * be included. */


static uim_lisp protected;
static uim_bool initialized;

static void *uim_scm_error_internal(const char *msg);
struct uim_scm_error_obj_args {
  const char *msg;
  uim_lisp errobj;
};
static void *uim_scm_error_obj_internal(struct uim_scm_error_obj_args *args);

struct call_args {
  uim_lisp proc;
  uim_lisp args;
  uim_lisp failed;
};
static void *uim_scm_call_internal(struct call_args *args);
static void *uim_scm_call_with_guard_internal(struct call_args *args);

struct callf_args {
  const char *proc;
  const char *args_fmt;
  va_list args;
  uim_bool with_guard;
  uim_lisp failed;
};
static void *uim_scm_callf_internal(struct callf_args *args);

static void *uim_scm_c_int_internal(void *uim_lisp_integer);
static void *uim_scm_make_int_internal(void *integer);
static void *uim_scm_c_char_internal(void *uim_lisp_ch);
static void *uim_scm_make_char_internal(intptr_t ch);
static const char *uim_scm_refer_c_str_internal(void *uim_lisp_str);
static void *uim_scm_make_str_internal(const char *str);
static void *uim_scm_make_str_directly_internal(char *str);
static void *uim_scm_make_symbol_internal(const char *name);
static void *uim_scm_make_ptr_internal(void *ptr);
static void *uim_scm_make_func_ptr_internal(uim_func_ptr func_ptr);
static void *uim_scm_symbol_value_internal(const char *symbol_str);
static void *uim_scm_symbol_value_int_internal(const char *symbol_str);
static char *uim_scm_symbol_value_str_internal(const char *symbol_str);
struct array2list_args {
  void **ary;
  size_t len;
  uim_lisp (*conv)(void *);
};
static void *uim_scm_array2list_internal(struct array2list_args *args);
struct list2array_args {
  uim_lisp lst;
  size_t *len;
  void *(*conv)(uim_lisp);
};
static void *uim_scm_list2array_internal(struct list2array_args *args);
struct array2vector_args {
  void **ary;
  size_t len;
  uim_lisp (*conv)(void *);
};
static void *uim_scm_array2vector_internal(struct array2vector_args *args);
struct vector2array_args {
  uim_lisp vec;
  size_t *len;
  void *(*conv)(uim_lisp);
};
static void *uim_scm_vector2array_internal(struct vector2array_args *args);
static void *uim_scm_eval_internal(void *uim_lisp_obj);
static void *uim_scm_quote_internal(void *obj);
struct cons_args {
  uim_lisp car;
  uim_lisp cdr;
};
static void *uim_scm_cons_internal(struct cons_args *args);


void
uim_scm_set_fatal_error_hook(void (*hook)(void))
{
  scm_set_fatal_error_callback(hook);
}

void
uim_scm_error(const char *msg)
{
  assert(uim_scm_gc_any_contextp());
  assert(msg);

  uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_error_internal, (char *)msg);
}

static void *
uim_scm_error_internal(const char *msg)
{
  scm_plain_error(msg);
  SCM_NOTREACHED;
}

void
uim_scm_error_obj(const char *msg, uim_lisp errobj)
{
  struct uim_scm_error_obj_args args;

  assert(uim_scm_gc_any_contextp());
  assert(msg);
  assert(uim_scm_gc_protectedp(errobj));

  args.msg = msg;
  args.errobj = errobj;
  uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_error_obj_internal, &args);
}

static void *
uim_scm_error_obj_internal(struct uim_scm_error_obj_args *args)
{
  scm_error_obj(NULL, args->msg, (ScmObj)args->errobj);
  SCM_NOTREACHED;
}

/* can be passed to uim_scm_list2null_term_array() */
long
uim_scm_c_bool(uim_lisp val)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_scm_truep(val)) ? UIM_TRUE : UIM_FALSE;
}

/* can be passed to uim_scm_array2list() */
uim_lisp
uim_scm_make_bool(long val)
{
  assert(uim_scm_gc_any_contextp());

  return (val) ? uim_scm_t() : uim_scm_f();
}

long
uim_scm_c_int(uim_lisp integer)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(integer));

  return (long)(intptr_t)uim_scm_call_with_gc_ready_stack(uim_scm_c_int_internal, (void *)integer);
}

static void *
uim_scm_c_int_internal(void *uim_lisp_integer)
{
  long c_int;
  uim_lisp integer;

  integer = (uim_lisp)uim_lisp_integer;

  if (!SCM_INTP((ScmObj)integer))
    uim_scm_error_obj("uim_scm_c_int: number required but got ", integer);

  c_int = SCM_INT_VALUE((ScmObj)integer);
  return (void *)(intptr_t)c_int;
}

uim_lisp
uim_scm_make_int(long integer)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)uim_scm_call_with_gc_ready_stack(uim_scm_make_int_internal,
                                                    (void *)(intptr_t)integer);
}

static void *
uim_scm_make_int_internal(void *integer)
{
  return (void *)SCM_MAKE_INT((intptr_t)integer);
}

long
uim_scm_c_char(uim_lisp ch)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(ch));

  return (long)(intptr_t)uim_scm_call_with_gc_ready_stack(uim_scm_c_char_internal, (void *)ch);
}

static void *
uim_scm_c_char_internal(void *uim_lisp_ch)
{
  scm_ichar_t ch;
  uim_lisp ch_;

  ch_ = (uim_lisp)uim_lisp_ch;

  if (!SCM_CHARP((ScmObj)ch_))
    uim_scm_error_obj("uim_scm_c_char: char required but got ", ch_);

  ch = SCM_CHAR_VALUE((ScmObj)ch_);
  return (void *)(intptr_t)ch;
}

uim_lisp
uim_scm_make_char(long ch)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_make_char_internal, (void *)(intptr_t)ch);
}

static void *
uim_scm_make_char_internal(intptr_t ch)
{
  return (void *)SCM_MAKE_CHAR((scm_ichar_t)ch);
}

char *
uim_scm_c_str(uim_lisp str)
{
  const char *c_str;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(str));

  c_str = uim_scm_refer_c_str(str);

  return (c_str) ? scm_strdup(c_str) : NULL;
}

const char *
uim_scm_refer_c_str(uim_lisp str)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(str));

  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_refer_c_str_internal, (void *)str);
}

static const char *
uim_scm_refer_c_str_internal(void *uim_lisp_str)
{
  char *c_str;
  uim_lisp str;

  str = (uim_lisp)uim_lisp_str;

  if (SCM_STRINGP((ScmObj)str)) {
    c_str = SCM_STRING_STR((ScmObj)str);
  } else if (SCM_SYMBOLP((ScmObj)str)) {
    c_str = SCM_SYMBOL_NAME((ScmObj)str);
  } else {
    uim_scm_error_obj("uim_scm_refer_c_str: string or symbol required but got ",
		      str);
    SCM_NOTREACHED;
  }

  return c_str;
}

uim_lisp
uim_scm_make_str(const char *str)
{
  assert(uim_scm_gc_any_contextp());
  assert(str);

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_make_str_internal, (void *)str);
}

static void *
uim_scm_make_str_internal(const char *str)
{
  return (void *)SCM_MAKE_STRING_COPYING(str, SCM_STRLEN_UNKNOWN);
}

uim_lisp
uim_scm_make_str_directly(char *str)
{
  assert(uim_scm_gc_any_contextp());
  assert(str);

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_make_str_directly_internal, (void *)str);
}

static void *
uim_scm_make_str_directly_internal(char *str)
{
  return (void *)SCM_MAKE_STRING(str, SCM_STRLEN_UNKNOWN);
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(symbol));

  return scm_strdup((char *)SCM_SYMBOL_NAME((ScmObj)symbol));
}

uim_lisp
uim_scm_make_symbol(const char *name)
{
  assert(uim_scm_gc_any_contextp());
  assert(name);

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_make_symbol_internal, (void *)name);
}

static void *
uim_scm_make_symbol_internal(const char *name)
{
  return (void *)scm_intern(name);
}

void *
uim_scm_c_ptr(uim_lisp ptr)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(ptr));

  if (!SCM_C_POINTERP((ScmObj)ptr))
    uim_scm_error_obj("uim_scm_c_ptr: C pointer required but got ", ptr);

  return SCM_C_POINTER_VALUE((ScmObj)ptr);
}

void
uim_scm_nullify_c_ptr(uim_lisp ptr)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(ptr));

  if (!SCM_C_POINTERP((ScmObj)ptr))
    uim_scm_error_obj("uim_scm_nullify_c_ptr: C pointer required but got ",
		      ptr);

  SCM_C_POINTER_SET_VALUE((ScmObj)ptr, NULL);
}

uim_lisp
uim_scm_make_ptr(void *ptr)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)uim_scm_call_with_gc_ready_stack(uim_scm_make_ptr_internal,
                                                    ptr);
}

static void *
uim_scm_make_ptr_internal(void *ptr)
{
  return (void *)SCM_MAKE_C_POINTER(ptr);
}

uim_func_ptr
uim_scm_c_func_ptr(uim_lisp func_ptr)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(func_ptr));

  if (!SCM_C_FUNCPOINTERP((ScmObj)func_ptr))
    uim_scm_error_obj("uim_scm_c_func_ptr: C function pointer required but got ", func_ptr);

  return SCM_C_FUNCPOINTER_VALUE((ScmObj)func_ptr);
}

uim_lisp
uim_scm_make_func_ptr(uim_func_ptr func_ptr)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_make_func_ptr_internal, (void *)(uintptr_t)func_ptr);
}

static void *
uim_scm_make_func_ptr_internal(uim_func_ptr func_ptr)
{
  return (void *)SCM_MAKE_C_FUNCPOINTER((ScmCFunc)func_ptr);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  assert(uim_scm_gc_any_contextp());
  assert(location);

  scm_gc_protect((ScmObj *)location);
}

void
uim_scm_gc_unprotect(uim_lisp *location)
{
  assert(uim_scm_gc_any_contextp());
  assert(location);

  scm_gc_unprotect((ScmObj *)location);
}

void *
uim_scm_call_with_gc_ready_stack(uim_gc_gate_func_ptr func, void *arg)
{
  assert(uim_scm_gc_any_contextp());
  assert(func);

  return scm_call_with_gc_ready_stack(func, arg);
}

uim_bool
uim_scm_gc_protectedp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return scm_gc_protectedp((ScmObj)obj);
}

uim_bool
uim_scm_gc_protected_contextp(void)
{
  return (initialized && scm_gc_protected_contextp());
}

uim_bool
uim_scm_is_initialized(void)
{
  return initialized;
}

void
uim_scm_set_lib_path(const char *path)
{
  assert(uim_scm_gc_any_contextp());

  scm_set_lib_path(path);
}

/* temporary solution for getting an value from Scheme world */
uim_lisp
uim_scm_symbol_value(const char *symbol_str)
{
  assert(uim_scm_gc_any_contextp());
  assert(symbol_str);

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_symbol_value_internal, (void *)symbol_str);
}

static void *
uim_scm_symbol_value_internal(const char *symbol_str)
{
  ScmObj symbol;

  symbol = scm_intern(symbol_str);
  if (SCM_TRUEP(scm_p_symbol_boundp(symbol, SCM_NULL))) {
    return (void *)(uim_lisp)scm_p_symbol_value(symbol);
  } else {
    return (void *)uim_scm_f();
  }
}

uim_bool
uim_scm_symbol_value_bool(const char *symbol_str)
{
  uim_bool val;

  assert(uim_scm_gc_any_contextp());
  assert(symbol_str);

  val = uim_scm_c_bool(uim_scm_symbol_value(symbol_str));

  return val;
}

long
uim_scm_symbol_value_int(const char *symbol_str)
{
  assert(uim_scm_gc_any_contextp());
  assert(symbol_str);

  return (long)(intptr_t)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_symbol_value_int_internal, (void *)symbol_str);
}

static void *
uim_scm_symbol_value_int_internal(const char *symbol_str)
{
  uim_lisp val_;
  long val;

  val_ = uim_scm_symbol_value(symbol_str);
  val = (uim_scm_truep(val_)) ? uim_scm_c_int(val_) : 0;

  return (void *)(intptr_t)val;
}

char *
uim_scm_symbol_value_str(const char *symbol_str)
{
  assert(uim_scm_gc_any_contextp());
  assert(symbol_str);

  return uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_symbol_value_str_internal, (void *)symbol_str);
}

static char *
uim_scm_symbol_value_str_internal(const char *symbol_str)
{
  uim_lisp val_;
  char *val;

  val_ = uim_scm_symbol_value(symbol_str);
  val = (uim_scm_truep(val_)) ? uim_scm_c_str(val_) : NULL;

  return val;
}

uim_bool
uim_scm_load_file(const char *fn)
{
  uim_lisp ok;

  assert(uim_scm_gc_any_contextp());
  assert(fn);

  /* (guard (err (else #f)) (load "<fn>")) */
  protected = ok = uim_scm_callf_with_guard(uim_scm_f(), "load", "s", fn);

  return uim_scm_c_bool(ok);
}

uim_lisp
uim_scm_t(void)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)SCM_TRUE;
}

uim_lisp
uim_scm_f(void)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)SCM_FALSE;
}

uim_lisp
uim_scm_null(void)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)SCM_NULL;
}

uim_lisp
uim_scm_eof(void)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)SCM_EOF;
}

uim_lisp
uim_scm_quote(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(obj));

  return (uim_lisp)uim_scm_call_with_gc_ready_stack(uim_scm_quote_internal,
                                                    (void *)obj);
}

static void *
uim_scm_quote_internal(void *obj)
{
  return (void *)SCM_LIST_2(SCM_SYM_QUOTE, (ScmObj)obj);
}

uim_lisp
uim_scm_list1(uim_lisp elm1)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(elm1));

  return uim_scm_cons(elm1, uim_scm_null());
}

uim_lisp
uim_scm_list2(uim_lisp elm1, uim_lisp elm2)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(elm1));
  assert(uim_scm_gc_protectedp(elm2));

  return uim_scm_cons(elm1, uim_scm_list1(elm2));
}

uim_lisp
uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(elm1));
  assert(uim_scm_gc_protectedp(elm2));
  assert(uim_scm_gc_protectedp(elm3));

  return uim_scm_cons(elm1, uim_scm_list2(elm2, elm3));
}

uim_lisp
uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(elm1));
  assert(uim_scm_gc_protectedp(elm2));
  assert(uim_scm_gc_protectedp(elm3));
  assert(uim_scm_gc_protectedp(elm4));

  return uim_scm_cons(elm1, uim_scm_list3(elm2, elm3, elm4));
}

uim_lisp
uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4,
              uim_lisp elm5)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(elm1));
  assert(uim_scm_gc_protectedp(elm2));
  assert(uim_scm_gc_protectedp(elm3));
  assert(uim_scm_gc_protectedp(elm4));
  assert(uim_scm_gc_protectedp(elm5));

  return uim_scm_cons(elm1, uim_scm_list4(elm2, elm3, elm4, elm5));
}

/* Pass through uim_lisp if (conv == NULL). */
uim_lisp
uim_scm_array2list(void **ary, size_t len, uim_lisp (*conv)(void *))
{
  struct array2list_args args;

  assert(uim_scm_gc_any_contextp());
  assert(len < SCM_INT_T_MAX);
  assert(conv || !conv);

  args.ary = ary;
  args.len = len;
  args.conv = conv;

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_array2list_internal, &args);
}

static void *
uim_scm_array2list_internal(struct array2list_args *args)
{
  return (void *)scm_array2list(args->ary, args->len,
				(ScmObj (*)(void *))args->conv);
}

/* Only accepts proper list. */
void **
uim_scm_list2array(uim_lisp lst, size_t *len, void *(*conv)(uim_lisp))
{
  struct list2array_args args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(lst));
  assert(conv || !conv);

  args.lst = lst;
  args.len = len;
  args.conv = conv;

  return (void **)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_list2array_internal, &args);
}
  
static void *
uim_scm_list2array_internal(struct list2array_args *args)
{
  return (void *)scm_list2array((ScmObj)args->lst, args->len,
				(void *(*)(ScmObj))args->conv);
}

/* Pass through uim_lisp if (conv == NULL). */
uim_lisp
uim_scm_array2vector(void **ary, size_t len, uim_lisp (*conv)(void *))
{
  struct array2vector_args args;

  assert(uim_scm_gc_any_contextp());
  assert(ary);
  assert(len < SCM_INT_T_MAX);
  assert(conv || !conv);

  args.ary = ary;
  args.len = len;
  args.conv = conv;

  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_array2vector_internal, &args);
}

static void *
uim_scm_array2vector_internal(struct array2vector_args *args)
{
  ScmObj *vec;
  size_t i;

  vec = scm_malloc(args->len * sizeof(ScmObj));
  for (i = 0; i < args->len; i++)
    vec[i] = (ScmObj)args->conv(args->ary[i]);

  return (void *)(uintptr_t)SCM_MAKE_VECTOR(vec, args->len);
}

/* Only accepts proper list. */
void **
uim_scm_vector2array(uim_lisp vec, size_t *len, void *(*conv)(uim_lisp))
{
  struct vector2array_args args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(vec));
  assert(len);
  assert(conv || !conv);

  UIM_SCM_ENSURE_TYPE(vector, vec);

  args.vec = vec;
  args.len = len;
  args.conv = conv;

  return (void **)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_vector2array_internal, &args);
}
  
static void *
uim_scm_vector2array_internal(struct vector2array_args *args)
{
  void **ary;
  ScmObj vec_, *vec;
  size_t len, i;

  vec_ = (ScmObj)args->vec;
  vec = SCM_VECTOR_VEC(vec_);
  len = (size_t)SCM_VECTOR_LEN(vec_);
  *args->len = len;

  ary = scm_malloc(len * sizeof(void *));
  for (i = 0; i < len; i++)
    ary[i] = args->conv((uim_lisp)vec[i]);

  return ary;
}

/* (if obj #t #f) */
uim_bool
uim_scm_truep(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_TRUEP((ScmObj)obj));
}

uim_bool
uim_scm_falsep(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_FALSEP((ScmObj)obj));
}

uim_bool
uim_scm_nullp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_NULLP((ScmObj)obj));
}

uim_bool
uim_scm_consp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_CONSP((ScmObj)obj));
}

uim_bool
uim_scm_listp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  /* does not detect circular list */
  return (SCM_NULLP((ScmObj)obj) || SCM_CONSP((ScmObj)obj));
}

uim_bool
uim_scm_intp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_INTP((ScmObj)obj));
}

uim_bool
uim_scm_charp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_CHARP((ScmObj)obj));
}

uim_bool
uim_scm_vectorp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_VECTORP((ScmObj)obj));
}

uim_bool
uim_scm_strp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_STRINGP((ScmObj)obj));
}

uim_bool
uim_scm_symbolp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_SYMBOLP((ScmObj)obj));
}

uim_bool
uim_scm_ptrp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_C_POINTERP((ScmObj)obj));
}

uim_bool
uim_scm_func_ptrp(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());

  return (SCM_C_FUNCPOINTERP((ScmObj)obj));
}

uim_bool
uim_scm_eq(uim_lisp a, uim_lisp b)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(a));
  assert(uim_scm_gc_protectedp(b));

  return (SCM_EQ((ScmObj)a, (ScmObj)b));
}

uim_lisp
uim_scm_eval(uim_lisp obj)
{
  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(obj));

  return (uim_lisp)uim_scm_call_with_gc_ready_stack(uim_scm_eval_internal,
						    (void *)obj);
}

static void *
uim_scm_eval_internal(void *uim_lisp_obj)
{
  uim_lisp obj;

  obj = (uim_lisp)uim_lisp_obj;

  return (void *)scm_p_eval((ScmObj)obj, SCM_NULL);
}

uim_lisp
uim_scm_eval_c_string(const char *str)
{
  assert(uim_scm_gc_any_contextp());

  return (uim_lisp)scm_eval_c_string(str);
}

uim_lisp
uim_scm_call(uim_lisp proc, uim_lisp args)
{
  struct call_args _args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(proc));
  assert(uim_scm_gc_protectedp(args));

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

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(failed));
  assert(uim_scm_gc_protectedp(proc));
  assert(uim_scm_gc_protectedp(args));

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
uim_scm_callf(const char *proc, const char *args_fmt, ...)
{
  uim_lisp ret;
  struct callf_args args;

  assert(uim_scm_gc_any_contextp());
  assert(proc);
  assert(args_fmt);

  va_start(args.args, args_fmt);

  args.proc = proc;
  args.args_fmt = args_fmt;
  args.with_guard = UIM_FALSE;
  ret = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_callf_internal, &args);

  va_end(args.args);

  return ret;
}

static void *
uim_scm_callf_internal(struct callf_args *args)
{
  ScmObj proc, scm_args, arg;
  ScmQueue argq;
  const char *fmtp;

  proc = scm_eval(scm_intern(args->proc), SCM_INTERACTION_ENV);
  scm_args = SCM_NULL;
  SCM_QUEUE_POINT_TO(argq, scm_args);
  for (fmtp = args->args_fmt; *fmtp; fmtp++) {
    switch (*fmtp) {
    case 'b':
      arg = SCM_MAKE_BOOL(va_arg(args->args, int));
      break;

    case 'i':
      arg = SCM_MAKE_INT(va_arg(args->args, int));
      break;

    case 'l':
      arg = SCM_MAKE_INT(va_arg(args->args, long));
      break;

    case 'j':
      arg = SCM_MAKE_INT(va_arg(args->args, intmax_t));
      break;

    case 'c':
      arg = SCM_MAKE_CHAR(va_arg(args->args, int));
      break;

    case 's':
      arg = SCM_MAKE_STRING_COPYING(va_arg(args->args, const char *),
                                    SCM_STRLEN_UNKNOWN);
      break;

    case 'y':
      arg = scm_intern(va_arg(args->args, const char *));
      break;

    case 'p':
      arg = SCM_MAKE_C_POINTER(va_arg(args->args, void *));
      break;

    case 'f':
      arg = SCM_MAKE_C_FUNCPOINTER(va_arg(args->args, ScmCFunc));
      break;

    case 'o':
      arg = (ScmObj)va_arg(args->args, uim_lisp);
      assert(scm_gc_protectedp(arg));
      break;

    case 'v':
      arg = scm_symbol_value(scm_intern(va_arg(args->args, const char *)),
			     SCM_INTERACTION_ENV);
      break;

    default:
      SCM_NOTREACHED;
    }
    SCM_QUEUE_ADD(argq, arg);
  }

  if (args->with_guard)
    return (void *)uim_scm_call_with_guard(args->failed,
                                           (uim_lisp)proc, (uim_lisp)scm_args);
  else
    return (void *)(uim_lisp)scm_call(proc, scm_args);
}

uim_lisp
uim_scm_callf_with_guard(uim_lisp failed,
                         const char *proc, const char *args_fmt, ...)
{
  uim_lisp ret;
  struct callf_args args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(failed));
  assert(proc);
  assert(args_fmt);

  va_start(args.args, args_fmt);

  args.proc = proc;
  args.args_fmt = args_fmt;
  args.with_guard = UIM_TRUE;
  args.failed = failed;
  ret = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_callf_internal, &args);

  va_end(args.args);

  return ret;
}

uim_lisp
uim_scm_car(uim_lisp pair)
{
  assert(uim_scm_gc_protected_contextp());

  return (uim_lisp)scm_p_car((ScmObj)pair);
}

uim_lisp
uim_scm_cdr(uim_lisp pair)
{
  assert(uim_scm_gc_protected_contextp());

  return (uim_lisp)scm_p_cdr((ScmObj)pair);
}

void
uim_scm_set_car(uim_lisp pair, uim_lisp car)
{
  assert(uim_scm_gc_protected_contextp());

  scm_p_set_carx((ScmObj)pair, (ScmObj)car);
}

void
uim_scm_set_cdr(uim_lisp pair, uim_lisp cdr)
{
  assert(uim_scm_gc_protected_contextp());

  scm_p_set_cdrx((ScmObj)pair, (ScmObj)cdr);
}

uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr)
{
  struct cons_args args;

  assert(uim_scm_gc_any_contextp());
  assert(uim_scm_gc_protectedp(car));
  assert(uim_scm_gc_protectedp(cdr));

  args.car = car;
  args.cdr = cdr;
  return (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)uim_scm_cons_internal, &args);
}

static void *
uim_scm_cons_internal(struct cons_args *args)
{
  return (void *)SCM_CONS((ScmObj)args->car, (ScmObj)args->cdr);
}

long
uim_scm_length(uim_lisp lst)
{
  uim_lisp len;

  assert(uim_scm_gc_protected_contextp());
  assert(uim_scm_gc_protectedp(lst));

  protected = len = (uim_lisp)scm_p_length((ScmObj)lst);
  return uim_scm_c_int(len);
}

uim_lisp
uim_scm_vector_ref(uim_lisp vec, long i)
{
  assert(uim_scm_gc_protected_contextp());
  assert(uim_scm_gc_protectedp(vec));

  return (uim_lisp)scm_p_vector_ref((ScmObj)vec, SCM_MAKE_INT(i));
}

void
uim_scm_vector_set(uim_lisp vec, long i, uim_lisp elm)
{
  assert(uim_scm_gc_protected_contextp());
  assert(uim_scm_gc_protectedp(vec));
  assert(uim_scm_gc_protectedp(elm));

  scm_p_vector_setx((ScmObj)vec, SCM_MAKE_INT(i), (ScmObj)elm);
}

long
uim_scm_vector_length(uim_lisp vec)
{
  assert(uim_scm_gc_protected_contextp());
  assert(uim_scm_gc_protectedp(vec));

  /* To add type check for vec, SCM_VECTOR_LEN() is not directly used. */
  return uim_scm_c_int((uim_lisp)scm_p_vector_length((ScmObj)vec));
}

uim_bool
uim_scm_require_file(const char *fn)
{
  uim_lisp ok;

  assert(uim_scm_gc_any_contextp());
  assert(fn);

  /* (guard (err (else #f)) (require "<fn>")) */
  protected = ok = uim_scm_callf_with_guard(uim_scm_f(), "require", "s", fn);

  return uim_scm_c_bool(ok);
}

void
uim_scm_init_proc0(const char *name, uim_lisp (*func)(void))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_0)func, SCM_PROCEDURE_FIXED_0);
}

void
uim_scm_init_proc1(const char *name, uim_lisp (*func)(uim_lisp))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_1)func, SCM_PROCEDURE_FIXED_1);
}

void
uim_scm_init_proc2(const char *name, uim_lisp (*func)(uim_lisp, uim_lisp))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_2)func, SCM_PROCEDURE_FIXED_2);
}

void
uim_scm_init_proc3(const char *name,
		   uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_3)func, SCM_PROCEDURE_FIXED_3);
}

void
uim_scm_init_proc4(const char *name,
		   uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp, uim_lisp))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_4)func, SCM_PROCEDURE_FIXED_4);
}

void
uim_scm_init_proc5(const char *name,
		   uim_lisp (*func)(uim_lisp, uim_lisp, uim_lisp, uim_lisp,
				    uim_lisp))
{
  assert(uim_scm_gc_protected_contextp());
  assert(name);
  assert(func);

  scm_register_func(name, (scm_procedure_fixed_5)func, SCM_PROCEDURE_FIXED_5);
}

void
uim_scm_init(const char *system_load_path)
{
  ScmStorageConf storage_conf;
  char **argp, *argv[8];

  if (initialized)
    return;

  argp = argv;
  *argp++ = "dummy";  /* command name */
#if SCM_USE_MULTIBYTE_CHAR
  /*
   * Set the raw unibyte codec which accepts all (multi)byte sequence
   * although it slashes a multibyte character on Scheme-level
   * character processing. Since current uim implementation treats a
   * multibyte character as string, it is not a problem. The name
   * "ISO-8859-1" is a dummy name for the codec.
   */
  *argp++ = "-C";
  *argp++ = "ISO-8859-1";
#endif
  if (system_load_path) {
    *argp++ = "--system-load-path";
    *argp++ = (char *)system_load_path;  /* safe */
  }
  *argp++ = NULL;

  /* 128KB/heap, max 0.99GB on 32-bit systems. Since maximum length of list can
   * be represented by a Scheme integer, SCM_INT_MAX limits the number of cons
   * cells. */
  storage_conf.heap_size            = 16384;
  storage_conf.heap_alloc_threshold = 16384;
  storage_conf.n_heaps_max          = SCM_INT_MAX / storage_conf.heap_size;
  storage_conf.n_heaps_init         = 1;
  storage_conf.symbol_hash_size     = 1024;
  scm_initialize(&storage_conf, (const char *const *)&argv);
  initialized = UIM_TRUE;  /* init here for uim_scm_gc_protect() */

  protected = (uim_lisp)SCM_FALSE;
  uim_scm_gc_protect(&protected);

#ifdef DEBUG_SCM
  /* required by test-im.scm */
  uim_scm_callf("provide", "s", "debug");
#endif

  scm_require_module("srfi-34");
}

void
uim_scm_quit(void)
{
  if (!initialized)
    return;

  scm_finalize();
  initialized = UIM_FALSE;
}
