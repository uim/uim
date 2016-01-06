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
#include <sys/types.h>
#ifdef HAVE_MMAP
#include <sys/mman.h>
#endif

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-posix.h"
#include "uim-notify.h"
#include "dynlib.h"

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
  int c;

  if (CHARP(c_))
    c = C_CHAR(c_);
  else if (STRP(c_))
    c = C_STR(c_)[0];
  else
    c = C_INT(c_);

  memset(C_PTR(pointer_), c, C_INT(len_));
  return uim_scm_t();
}
static uim_lisp
c_memory_move(uim_lisp dest_, uim_lisp src_, uim_lisp len_)
{
  if (STRP(src_))
    strlcpy(C_PTR(dest_), REFER_C_STR(src_), C_INT(len_));
  else
    memmove(C_PTR(dest_), C_PTR(src_), C_INT(len_));
  return uim_scm_t();
}

#ifdef HAVE_MMAP

const static opt_args mmap_prot_flags[] = {
  { PROT_EXEC,  "$PROT_EXEC"  },
  { PROT_READ,  "$PROT_READ"  },
  { PROT_WRITE, "$PROT_WRITE" },
  { PROT_NONE,  "$PROT_NONE"  },
  { 0, 0 }
};

static uim_lisp uim_lisp_mmap_prot_flags;
static uim_lisp
c_mmap_prot_flags(void)
{
  return uim_lisp_mmap_prot_flags;
}

const static opt_args mmap_flags[] = {
#ifdef MAP_ANON
  { MAP_ANON,      "$MAP_ANON"  },
#endif
#ifdef MAP_ANONYMOUS
  { MAP_ANONYMOUS, "$MAP_ANONYMOUS" },
#endif
#ifdef MAP_FILE
  { MAP_FILE,      "$MAP_FILE"  },
#endif
#ifdef MAP_FIXED
  { MAP_FIXED,     "$MAP_FIXED" },
#endif
  { MAP_PRIVATE,   "$MAP_PRIVATE"  },
  { MAP_SHARED,    "$MAP_SHARED"  },
#ifdef MAP_COPY
  { MAP_COPY,      "$MAP_COPY"  },
#endif
  { 0, 0 }
};

static uim_lisp uim_lisp_mmap_flags;
static uim_lisp
c_mmap_flags(void)
{
  return uim_lisp_mmap_flags;
}

static uim_lisp
c_mmap(uim_lisp addr_, uim_lisp len_, uim_lisp prot_flags_, uim_lisp fd_, uim_lisp offset_)
{
  void *p = mmap(C_PTR(addr_), C_INT(len_), C_INT(CAR(prot_flags_)), C_INT(CDR(prot_flags_)), C_INT(fd_), C_INT(offset_));

  if (p == MAP_FAILED)
    return uim_scm_f();
  return MAKE_PTR(p);
}

static uim_lisp
c_munmap(uim_lisp addr_, uim_lisp len_)
{
  return MAKE_INT(munmap(C_PTR(addr_), C_INT(len_)));
}

#endif

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
  struct c__pointer_to_ ## X ## list_args {				\
    type *pointer;							\
    int len;								\
  };									\
									\
  static uim_lisp							\
  c_pointer_to_ ## X ## list_internal(struct c__pointer_to_ ## X ## list_args *args) \
  {									\
      uim_lisp ret_ = uim_scm_null();					\
      int i;								\
      type *p = args->pointer;						\
      									\
      for (i = 0; i < args->len; i++)					\
	ret_ = CONS(MAKE_INT(*p++), ret_);				\
      return ret_;							\
  }									\
  static uim_lisp							\
  c_pointer_to_ ## X ## list(uim_lisp pointer_, uim_lisp len_)		\
  {									\
    uim_lisp ret_;							\
    struct c__pointer_to_ ## X ## list_args args;			\
									\
    args.pointer = C_PTR(pointer_);					\
    args.len = C_INT(len_);						\
    ret_ = (uim_lisp)uim_scm_call_with_gc_ready_stack((uim_gc_gate_func_ptr)c_pointer_to_ ## X ## list_internal, (void *)&args); \
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


static uim_lisp
c_htons(uim_lisp u16_)
{
  return MAKE_INT(htons(C_INT(u16_)));
}
static uim_lisp
c_htonl(uim_lisp u32_)
{
  return MAKE_INT(htonl(C_INT(u32_)));
}

static uim_lisp
c_ntohs(uim_lisp u16_)
{
  return MAKE_INT(ntohs(C_INT(u16_)));
}
static uim_lisp
c_ntohl(uim_lisp u32_)
{
  return MAKE_INT(ntohl(C_INT(u32_)));
}


static uim_lisp
c_u16_to_u8list(uim_lisp u16_)
{
  uint16_t u16 = htons(C_INT(u16_));

  return LIST2(MAKE_INT(u16 & 0xff),
	       MAKE_INT((u16 >> 8) & 0xff));
}
static uim_lisp
c_u32_to_u8list(uim_lisp u32_)
{
  uint32_t u32 = htonl(C_INT(u32_));

  return LIST4(MAKE_INT(u32 & 0xff),
	       MAKE_INT((u32 >> 8) & 0xff),
	       MAKE_INT((u32 >> 16) & 0xff),
	       MAKE_INT((u32 >> 24) & 0xff));
}

/* (map char->integer (string->list str)) */
static uim_lisp
c_string_to_u8list(uim_lisp str_)
{
  const char *str = REFER_C_STR(str_);
  uim_lisp ret_ = uim_scm_null();

  while (*str) {
    ret_ = CONS(MAKE_INT(*str & 0xff), ret_);
    str++;
  }
  ret_ = CONS(MAKE_INT(0), ret_);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
c_u8list_to_u16(uim_lisp u8list_)
{
  uint8_t u8_1, u8_2;

  u8_1 = C_INT(CAR(u8list_));
  u8_2 = C_INT(CAR(CDR(u8list_)));
  return MAKE_INT(ntohs(u8_1 |
			(u8_2 << 8)));
}
static uim_lisp
c_u8list_to_u32(uim_lisp u8list_)
{
  uint8_t u8_1, u8_2, u8_3, u8_4;

  u8_1 = C_INT(CAR(u8list_));
  u8_2 = C_INT(CAR(CDR(u8list_)));
  u8_3 = C_INT(CAR(CDR(CDR(u8list_))));
  u8_4 = C_INT(CAR(CDR(CDR(CDR(u8list_)))));
  return MAKE_INT(ntohl(u8_1 |
			(u8_2 << 8) |
			(u8_3 << 16) |
			(u8_4 << 24)));
}
static uim_lisp
c_u8list_to_string(uim_lisp u8list_)
{
  int len = uim_scm_length(u8list_);
  int i;
  char *str = uim_malloc(len + 1);

  for (i = 0; i < len; i++) {
    str[i] = (char)C_INT(CAR(u8list_));
    u8list_ = CDR(u8list_);
  }
  str[len] = '\0';
  return MAKE_STR_DIRECTLY(str);
}

static uim_lisp
c_u32_to_s32(uim_lisp u32_)
{
  uint32_t u32 = C_INT(u32_);

  return MAKE_INT((int32_t)u32);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc1("allocate", c_allocate);
  uim_scm_init_proc1("free",     c_free);

  uim_scm_init_proc3("memory-fill!", c_memory_fill);
  uim_scm_init_proc3("memory-move!", c_memory_move);

#ifdef HAVE_MMAP
  uim_lisp_mmap_prot_flags = make_arg_list(mmap_prot_flags);
  uim_scm_gc_protect(&uim_lisp_mmap_prot_flags);
  uim_scm_init_proc0("mmap-prot-flags?", c_mmap_prot_flags);
  uim_lisp_mmap_flags = make_arg_list(mmap_flags);
  uim_scm_gc_protect(&uim_lisp_mmap_flags);
  uim_scm_init_proc0("mmap-flags?", c_mmap_flags);

  uim_scm_init_proc5("mmap", c_mmap);
  uim_scm_init_proc2("munmap", c_munmap);
#endif

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

  uim_scm_init_proc1("htons", c_htons);
  uim_scm_init_proc1("htonl", c_htonl);
  uim_scm_init_proc1("ntohs", c_ntohs);
  uim_scm_init_proc1("ntohl", c_ntohl);

  uim_scm_init_proc1("u16->u8list",    c_u16_to_u8list);
  uim_scm_init_proc1("u32->u8list",    c_u32_to_u8list);
  uim_scm_init_proc1("string->u8list", c_string_to_u8list);

  uim_scm_init_proc1("u8list->u16",    c_u8list_to_u16);
  uim_scm_init_proc1("u8list->u32",    c_u8list_to_u32);
  uim_scm_init_proc1("u8list->string", c_u8list_to_string);

  uim_scm_init_proc1("u32->s32", c_u32_to_s32);
}

void
uim_plugin_instance_quit(void)
{
#ifdef HAVE_MMAP
  uim_scm_gc_unprotect(&uim_lisp_mmap_prot_flags);
  uim_scm_gc_unprotect(&uim_lisp_mmap_flags);
#endif
}
