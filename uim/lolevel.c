/*

  Copyright (c) 2009 uim Project http://code.google.com/p/uim/

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

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "dynlib.h"

static uim_lisp
c_allocate(uim_lisp len_)
{
  return MAKE_PTR(malloc(C_INT(len_)));
}
static uim_lisp
c_free(uim_lisp pointer_)
{
  free(C_PTR(pointer_));
  return uim_scm_t();
}
static uim_lisp

c_memory_fill(uim_lisp pointer_, uim_lisp c_, uim_lisp len_)
{
  memset(C_PTR(pointer_), C_INT(c_), C_INT(len_));
  return uim_scm_t();
}
static uim_lisp
c_memory_move(uim_lisp dest_, uim_lisp src_, uim_lisp len_)
{
  memmove(C_PTR(dest_), C_PTR(src_), C_INT(len_));
  return uim_scm_t();
}

static uim_lisp
c_null_pointer(void)
{
  return MAKE_PTR(NULL);
}

static uim_lisp
c_pointer_offset(uim_lisp pointer_, uim_lisp nth_)
{
  return MAKE_PTR((char *)C_PTR(pointer_) + C_INT(nth_));
}

#define c_pointer_X_ref(X, type)		\
  static uim_lisp				\
  c_pointer_ ## X ## _ref(uim_lisp pointer_)	\
  {						\
    type *p = C_PTR(pointer_);			\
    return MAKE_INT(*p);			\
  }

c_pointer_X_ref(u8,  uint8_t)
c_pointer_X_ref(s8,  int8_t)
c_pointer_X_ref(u16, uint16_t)
c_pointer_X_ref(s16, int16_t)
c_pointer_X_ref(u32, uint32_t)
c_pointer_X_ref(s32, int32_t)
c_pointer_X_ref(u64, uint64_t)
c_pointer_X_ref(s64, int64_t)

#define c_pointer_X_set(X, type)				\
  static uim_lisp						\
  c_pointer_ ## X ## _set(uim_lisp pointer_, uim_lisp val_)	\
  {								\
    type *p = C_PTR(pointer_);					\
    *p = C_INT(val_);						\
    return uim_scm_t();						\
  }

c_pointer_X_set(u8,  uint8_t)
c_pointer_X_set(s8,  int8_t)
c_pointer_X_set(u16, uint16_t)
c_pointer_X_set(s16, int16_t)
c_pointer_X_set(u32, uint32_t)
c_pointer_X_set(s32, int32_t)
c_pointer_X_set(u64, uint64_t)
c_pointer_X_set(s64, int64_t)

static uim_lisp
c_string_to_pointer(uim_lisp str_)
{
  return MAKE_PTR(C_STR(str_));
}
static uim_lisp
c_pointer_to_string(uim_lisp pointer_)
{
  return MAKE_STR(C_PTR(pointer_));
}


#define c_Xlist_to_pointer(X, type)					\
  static uim_lisp							\
  c_ ## X ## list_to_pointer(uim_lisp l_)				\
  {									\
    type *p;								\
    int i, len = uim_scm_length(l_);					\
    									\
    p = malloc(len * sizeof(type));					\
    for (i = 0; i < len; i++) {						\
      uim_lisp h_ = CAR(l_);						\
      									\
      p[i] = (type)C_INT(h_);						\
      l_ = CDR(l_);							\
    }									\
    return MAKE_PTR(p);							\
  }

c_Xlist_to_pointer(s8,  int8_t)
c_Xlist_to_pointer(u8,  uint8_t)
c_Xlist_to_pointer(s16, int16_t)
c_Xlist_to_pointer(u16, uint16_t)
c_Xlist_to_pointer(s32, int32_t)
c_Xlist_to_pointer(u32, uint32_t)
c_Xlist_to_pointer(s64, int64_t)
c_Xlist_to_pointer(u64, uint64_t)

#define c_pointer_to_Xlist(X, type)					\
  static uim_lisp							\
  c_pointer_to_ ## X ## list(uim_lisp pointer_, uim_lisp len_)		\
  {									\
    uim_lisp ret_ = uim_scm_null();					\
    type *p = C_PTR(pointer_);						\
    int len = C_INT(len_);						\
    int i;								\
									\
    for (i = 0; i < len; i++)						\
      ret_ = CONS(MAKE_INT(*p++), ret_);				\
    return uim_scm_callf("reverse", "o", ret_);				\
  }

c_pointer_to_Xlist(u8,  uint8_t)
c_pointer_to_Xlist(s8,  int8_t)
c_pointer_to_Xlist(u16, uint16_t)
c_pointer_to_Xlist(s16, int16_t)
c_pointer_to_Xlist(u32, uint32_t)
c_pointer_to_Xlist(s32, int32_t)
c_pointer_to_Xlist(u64, uint64_t)
c_pointer_to_Xlist(s64, int64_t)


void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc1("allocate", c_allocate);
  uim_scm_init_proc1("free",     c_free);

  uim_scm_init_proc3("memory-fill!", c_memory_fill);
  uim_scm_init_proc3("memory-move!", c_memory_move);

  uim_scm_init_proc0("null-pointer", c_null_pointer);

  uim_scm_init_proc2("pointer-offset", c_pointer_offset);

  uim_scm_init_proc1("pointer-u8-ref",  c_pointer_u8_ref);
  uim_scm_init_proc1("pointer-s8-ref",  c_pointer_s8_ref);
  uim_scm_init_proc1("pointer-u16-ref", c_pointer_u16_ref);
  uim_scm_init_proc1("pointer-s16-ref", c_pointer_s16_ref);
  uim_scm_init_proc1("pointer-u32-ref", c_pointer_u32_ref);
  uim_scm_init_proc1("pointer-s32-ref", c_pointer_s32_ref);
  uim_scm_init_proc1("pointer-u64-ref", c_pointer_u64_ref);
  uim_scm_init_proc1("pointer-s64-ref", c_pointer_s64_ref);

  uim_scm_init_proc2("pointer-u8-set!",  c_pointer_u8_set);
  uim_scm_init_proc2("pointer-s8-set!",  c_pointer_s8_set);
  uim_scm_init_proc2("pointer-u16-set!", c_pointer_u16_set);
  uim_scm_init_proc2("pointer-s16-set!", c_pointer_s16_set);
  uim_scm_init_proc2("pointer-u32-set!", c_pointer_u32_set);
  uim_scm_init_proc2("pointer-s32-set!", c_pointer_s32_set);
  uim_scm_init_proc2("pointer-u64-set!", c_pointer_u64_set);
  uim_scm_init_proc2("pointer-s64-set!", c_pointer_s64_set);

  uim_scm_init_proc1("string->pointer",  c_string_to_pointer);
  uim_scm_init_proc1("pointer->string",  c_pointer_to_string);

  uim_scm_init_proc1("u8list->pointer",  c_u8list_to_pointer);
  uim_scm_init_proc1("s8list->pointer",  c_s8list_to_pointer);
  uim_scm_init_proc1("u16list->pointer", c_u16list_to_pointer);
  uim_scm_init_proc1("s16list->pointer", c_s16list_to_pointer);
  uim_scm_init_proc1("u32list->pointer", c_u32list_to_pointer);
  uim_scm_init_proc1("s32list->pointer", c_s32list_to_pointer);
  uim_scm_init_proc1("u64list->pointer", c_u64list_to_pointer);
  uim_scm_init_proc1("s64list->pointer", c_s64list_to_pointer);

  uim_scm_init_proc2("pointer->u8list",  c_pointer_to_u8list);
  uim_scm_init_proc2("pointer->s8list",  c_pointer_to_s8list);
  uim_scm_init_proc2("pointer->u16list", c_pointer_to_u16list);
  uim_scm_init_proc2("pointer->s16list", c_pointer_to_s16list);
  uim_scm_init_proc2("pointer->u32list", c_pointer_to_u32list);
  uim_scm_init_proc2("pointer->s32list", c_pointer_to_s32list);
  uim_scm_init_proc2("pointer->u64list", c_pointer_to_u64list);
  uim_scm_init_proc2("pointer->s64list", c_pointer_to_s64list);
}

void
uim_plugin_instance_quit(void)
{
}
