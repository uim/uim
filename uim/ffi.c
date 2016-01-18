/*

  Copyright (c) 2009-2013 uim Project https://github.com/uim/uim

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>
#include <dlfcn.h>
#include <ffi.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "dynlib.h"

static const char *ffi_strerr_;

typedef struct {
  int flag;
  char *arg;
} opt_args;

static uim_lisp
make_arg_cons(const opt_args *arg)
{
  return CONS(MAKE_SYM(arg->arg), MAKE_INT(arg->flag));
}

static uim_lisp
make_arg_list(const opt_args *list)
{
  uim_lisp ret_;
  int i = 0;

  ret_ = uim_scm_null();
  while (list[i].arg != 0) {
    ret_ = CONS((uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)make_arg_cons,
                                                           (void *)&list[i]), ret_);
    i++;
  }
  return ret_;
}

static const opt_args dlopen_mode[] = {
#ifdef RTLD_LAZY
  { RTLD_LAZY,   "$RTLD_LAZY" },
#endif
#ifdef RTLD_NOW
  { RTLD_NOW,    "$RTLD_NOW" },
#endif
#ifdef RTLD_GLOBAL
  { RTLD_GLOBAL, "$RTLD_GLOBAL" },
#endif
#ifdef RTLD_LOCAL
  { RTLD_LOCAL,  "$RTLD_LOCAL" },
#endif
#ifdef DL_LAZY
  { DL_LAZY,     "$DL_LAZY" },
#endif
#ifdef RTLD_TRACE
  { RTLD_TRACE,  "$RTLD_TRACE" },
#endif
  { 0, 0 }
};

static uim_lisp uim_lisp_dlopen_mode_;
static uim_lisp
c_uim_lisp_dlopen_mode(void)
{
  return uim_lisp_dlopen_mode_;
}

static uim_lisp
c_dlstrerr(void)
{
  if (ffi_strerr_)
    return MAKE_STR(ffi_strerr_);
  return MAKE_STR("");
}

static uim_lisp
c_dlopen(uim_lisp path_, uim_lisp mode_)
{
  const char *s;
  void *handle = dlopen(REFER_C_STR(path_), C_INT(mode_));

  if ((s = dlerror()) != NULL) {
    ffi_strerr_ = s;
    return uim_scm_f();
  }
  ffi_strerr_ = NULL;
  return MAKE_PTR(handle);
}

static uim_lisp
c_dlclose(uim_lisp handle_)
{
  if (!PTRP(handle_))
    return uim_scm_f();
  dlclose(C_PTR(handle_));
  ffi_strerr_ = NULL;
  return uim_scm_t();
}

static uim_lisp
c_dlsym(uim_lisp handle_, uim_lisp symbol_)
{
  const char *s;
  void *fun;

  fun = dlsym(C_PTR(handle_), REFER_C_STR(symbol_));
  if ((s = dlerror()) != NULL) {
    ffi_strerr_ = s;
    return uim_scm_f();
  }
  ffi_strerr_ = NULL;
  return MAKE_PTR(fun);
}

typedef enum {
  RET_UNKNOWN,
  RET_VOID,
  RET_UCHAR, RET_SCHAR,
  RET_USHORT, RET_SSHORT,
  RET_UINT, RET_SINT,
  RET_ULONG, RET_SLONG,
  RET_FLOAT, RET_DOUBLE,
  RET_STR,
  RET_PTR,
  RET_SCM
} object_type;

#define FFI_STRERR_BAD_TYPEDEF 0
#define FFI_STRERR_BAD_ABI     1
#define FFI_STRERR_UNKOWN      2

static const char *ffi_strerr_messages[] = {
  "ffi_prep_cif: FFI_BAD_TYPEDEF\n",
  "ffi_prep_cif: FFI_BAD_ABI\n",
  "ffi_prep_cif: unkown error\n"
};

static object_type
select_object_type(uim_lisp type_)
{
  if (strcmp(REFER_C_STR(type_), "void") == 0)
    return RET_VOID;
  if (strcmp(REFER_C_STR(type_), "unsigned-char") == 0)
    return RET_UCHAR;
  if (strcmp(REFER_C_STR(type_), "signed-char") == 0)
    return RET_SCHAR;
  if (strcmp(REFER_C_STR(type_), "char") == 0)
    return RET_SCHAR;
  if (strcmp(REFER_C_STR(type_), "unsigned-short") == 0)
    return RET_USHORT;
  if (strcmp(REFER_C_STR(type_), "signed-short") == 0)
    return RET_SSHORT;
  if (strcmp(REFER_C_STR(type_), "short") == 0)
    return RET_SSHORT;
  if (strcmp(REFER_C_STR(type_), "unsigned-int") == 0)
    return RET_SINT;
  if (strcmp(REFER_C_STR(type_), "signed-int") == 0)
    return RET_SINT;
  if (strcmp(REFER_C_STR(type_), "int") == 0)
    return RET_SINT;
  if (strcmp(REFER_C_STR(type_), "unsigned-long") == 0)
    return RET_ULONG;
  if (strcmp(REFER_C_STR(type_), "signed-long") == 0)
    return RET_SLONG;
  if (strcmp(REFER_C_STR(type_), "long") == 0)
    return RET_SLONG;
  if (strcmp(REFER_C_STR(type_), "float") == 0)
    return RET_FLOAT;
  if (strcmp(REFER_C_STR(type_), "double") == 0)
    return RET_DOUBLE;
  if (strcmp(REFER_C_STR(type_), "string") == 0)
    return RET_STR;
  if (strcmp(REFER_C_STR(type_), "pointer") == 0)
    return RET_PTR;
  if (strcmp(REFER_C_STR(type_), "scheme-object") == 0)
    return RET_SCM;

  ERROR_OBJ("unknown object type", type_);
  return RET_UNKNOWN;
}

static uim_lisp
c_ffi_call(uim_lisp result_, uim_lisp fun_, uim_lisp argv_)
{
  ffi_cif cif;
  ffi_type **arg_types;
  void **arg_values;
  ffi_status status;
  ffi_type *result_type = NULL;
  void *result;
  int args;
  int i;
  void *p;
  uim_lisp ret_;
  object_type return_object_type;
  int input_void = 0;

  args = uim_scm_length(argv_);
  arg_types = uim_malloc(args * sizeof(void *));
  arg_values = uim_malloc(args * sizeof(ffi_type *));

  return_object_type = select_object_type(result_);

  switch (return_object_type) {
  case RET_UNKNOWN:
    break;
  case RET_VOID:
    result_type = &ffi_type_void;
    break;
  case RET_UCHAR:
    result_type = &ffi_type_uchar;
    break;
  case RET_SCHAR:
    result_type = &ffi_type_schar;
    break;
  case RET_USHORT:
    result_type = &ffi_type_ushort;
    break;
  case RET_SSHORT:
    result_type = &ffi_type_sshort;
    break;
  case RET_ULONG:
    result_type = &ffi_type_ulong;
    break;
  case RET_SLONG:
    result_type = &ffi_type_slong;
    break;
  case RET_UINT:
    result_type = &ffi_type_uint;
    break;
  case RET_SINT:
    result_type = &ffi_type_sint;
    break;
  case RET_FLOAT:
    result_type = &ffi_type_float;
    break;
  case RET_DOUBLE:
    result_type = &ffi_type_double;
    break;
  case RET_STR:
    result_type = &ffi_type_pointer;
    break;
  case RET_PTR:
    result_type = &ffi_type_pointer;
    break;
  case RET_SCM:
    result_type = &ffi_type_pointer;
    break;
  }

  result = uim_malloc(1024); /* huge? */

  for (i = 0; i < args; i++) {
    uim_lisp arg_ = CAR(argv_);

    switch (select_object_type(CAR(arg_))) {
    case RET_UNKNOWN:
      break;
    case RET_VOID:
      input_void = 1;
      break;
    case RET_UCHAR:
      p = uim_malloc(sizeof(unsigned char));
      *((unsigned char *)p) = C_CHAR(CDR(arg_));
      arg_types[i] = &ffi_type_uchar;
      arg_values[i] = p;
      break;
    case RET_SCHAR:
      p = uim_malloc(sizeof(signed char));
      *((signed char *)p) = C_CHAR(CDR(arg_));
      arg_types[i] = &ffi_type_schar;
      arg_values[i] = p;
      break;
    case RET_USHORT:
      p = uim_malloc(sizeof(unsigned short));
      *((unsigned short *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_ushort;
      arg_values[i] = p;
      break;
    case RET_SSHORT:
      p = uim_malloc(sizeof(unsigned short));
      *((signed short *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_sshort;
      arg_values[i] = p;
      break;
    case RET_UINT:
      p = uim_malloc(sizeof(unsigned int));
      *((unsigned int *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_uint;
      arg_values[i] = p;
      break;
    case RET_SINT:
      p = uim_malloc(sizeof(signed int));
      *((signed int *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_sint;
      arg_values[i] = p;
      break;
    case RET_ULONG:
      p = uim_malloc(sizeof(unsigned long));
      *((unsigned long *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_ulong;
      arg_values[i] = p;
      break;
    case RET_SLONG:
      p = uim_malloc(sizeof(signed long));
      *((signed long *)p) = C_INT(CDR(arg_));
      arg_types[i] = &ffi_type_slong;
      arg_values[i] = p;
      break;
    case RET_FLOAT:
      {
	char *endptr;
	p = uim_malloc(sizeof(float));
	*((double *)p) = strtof(REFER_C_STR(CDR(arg_)), &endptr);
	arg_types[i] = &ffi_type_float;
	arg_values[i] = p;
      }
      break;
    case RET_DOUBLE:
      {
	char *endptr;
	p = uim_malloc(sizeof(double));
	*((double *)p) = strtod(REFER_C_STR(CDR(arg_)), &endptr);
	arg_types[i] = &ffi_type_double;
	arg_values[i] = p;
      }
      break;
    case RET_STR:
      p = uim_malloc(sizeof(void *));
      *((void **)p) = (void *)REFER_C_STR(CDR(arg_));
      arg_types[i] = &ffi_type_pointer;
      arg_values[i] = p;
      break;
    case RET_PTR:
      p = uim_malloc(sizeof(void *));
      if (NULLP(CDR(arg_)))
	*((void **)p) = NULL;
      else
	*((void **)p) = C_PTR(CDR(arg_));
      arg_types[i] = &ffi_type_pointer;
      arg_values[i] = p;
      break;
    case RET_SCM:
      p = uim_malloc(sizeof(void *));
      *((void **)p) = CDR(arg_);
      arg_types[i] = &ffi_type_pointer;
      arg_values[i] = p;
    }
    argv_ = CDR(argv_);
  }

  if (input_void)
    args = 0;
  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, args, result_type, arg_types);
  switch (status) {
  case FFI_OK:
    break;
  case FFI_BAD_TYPEDEF:
    ffi_strerr_ = ffi_strerr_messages[FFI_STRERR_BAD_TYPEDEF];
    break;
  case FFI_BAD_ABI:
    ffi_strerr_ = ffi_strerr_messages[FFI_STRERR_BAD_ABI];
    break;
  default:
    ffi_strerr_ = ffi_strerr_messages[FFI_STRERR_UNKOWN];
  }

  if (status == FFI_OK)
    ffi_call(&cif, (void (*)(void))C_PTR(fun_), result, arg_values);

  for (i = 0; i < args; i++)
    free(arg_values[i]);
  free(arg_types);
  free(arg_values);

  if (status != FFI_OK) {
    free(result);
    return uim_scm_f();
  }
  ret_ = uim_scm_f();

  switch (return_object_type) {
  case RET_UNKNOWN:
  case RET_VOID:
    break;
  case RET_UCHAR:
    ret_ = MAKE_CHAR(*(unsigned char *)result);
    break;
  case RET_SCHAR:
    ret_ = MAKE_CHAR(*(signed char *)result);
    break;
  case RET_USHORT:
    ret_ = MAKE_INT(*(unsigned short *)result);
    break;
  case RET_SSHORT:
    ret_ = MAKE_INT(*(signed short *)result);
    break;
  case RET_UINT:
    ret_ = MAKE_INT(*(unsigned int *)result);
    break;
  case RET_SINT:
    ret_ = MAKE_INT(*(signed int *)result);
    break;
  case RET_ULONG:
    ret_ = MAKE_INT(*(unsigned long *)result);
    break;
  case RET_SLONG:
    ret_ = MAKE_INT(*(signed long *)result);
    break;
  case RET_FLOAT:
    {
      char str[1024];
      snprintf(str, sizeof(str), "%f", *((float *)result));
      ret_ = MAKE_STR(str);
    }
    break;
  case RET_DOUBLE:
    {
      char str[1024];
      snprintf(str, sizeof(str), "%f", *((double *)result));
      ret_ = MAKE_STR(str);
    }
    break;
  case RET_STR:
    ret_ = MAKE_STR(*((char **)result));
    break;
  case RET_PTR:
    ret_ = MAKE_PTR(*((void **)result));
    break;
  case RET_SCM:
    ret_ = *(uim_lisp *)result;
    break;
  }

  free(result);

  ffi_strerr_ = NULL;

  return ret_;
}

static uim_lisp
c_ffi_function(uim_lisp handler_, uim_lisp result_, uim_lisp funstr_, uim_lisp argv_)
{
  uim_lisp fun_ = c_dlsym(handler_, funstr_);
  if (PTRP(fun_))
    return c_ffi_call(result_, fun_, argv_);
  return uim_scm_f();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc0("dlstrerr", c_dlstrerr);

  uim_lisp_dlopen_mode_ = make_arg_list(dlopen_mode);
  uim_scm_gc_protect(&uim_lisp_dlopen_mode_);
  uim_scm_init_proc0("dlopen-mode", c_uim_lisp_dlopen_mode);
  uim_scm_init_proc2("dlopen", c_dlopen);

  uim_scm_init_proc1("dlclose", c_dlclose);
  uim_scm_init_proc2("dlsym", c_dlsym);

  uim_scm_init_proc3("ffi-call", c_ffi_call);
  uim_scm_init_proc4("ffi-function", c_ffi_function);
}

void
uim_plugin_instance_quit(void)
{
  uim_scm_gc_unprotect(&uim_lisp_dlopen_mode_);
}
