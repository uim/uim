/*===========================================================================
 *  Filename : gcroots.h
 *  About    : SigScheme-specific implementation of libgcroots API
 *
 *  Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 *  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 *  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
===========================================================================*/
#ifndef _GCROOTS_H
#define _GCROOTS_H

#include <stdlib.h>

#include "global.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GCROOTS_VERSION_MAJOR      (0)
#define GCROOTS_VERSION_MINOR      (1)
#define GCROOTS_VERSION_PATCHLEVEL (1)
#define GCROOTS_API_REVISION       (0)

#define GCROOTS_VERSION_REQUIRE(major, minor, patchlevel)                    \
  ((major) < GCROOTS_VERSION_MAJOR                                           \
   || ((major) == GCROOTS_VERSION_MAJOR && (minor) < GCROOTS_VERSION_MINOR)  \
   || ((major) == GCROOTS_VERSION_MAJOR && (minor) == GCROOTS_VERSION_MINOR  \
       && (patchlevel) <= GCROOTS_VERSION_PATCHLEVEL))

typedef struct _GCROOTS_context GCROOTS_context;

typedef void (*GCROOTS_mark_proc)(void *start, void *end,
                                  int is_certain, int is_aligned);
typedef void *(*GCROOTS_user_proc)(void *arg);
typedef void *(*GCROOTS_context_alloc_proc)(size_t ctx_size);


SCM_EXPORT GCROOTS_context *GCROOTS_init(GCROOTS_context_alloc_proc allocator,
                                         GCROOTS_mark_proc marker,
                                         int scan_entire_system_stack);
SCM_EXPORT void GCROOTS_fin(GCROOTS_context *ctx);

SCM_EXPORT void *GCROOTS_call_with_gc_ready_stack(GCROOTS_context *ctx,
                                                  GCROOTS_user_proc proc,
                                                  void *arg);
SCM_EXPORT void GCROOTS_mark(GCROOTS_context *ctx);


#ifdef __cplusplus
}
#endif

#endif /* _GCROOTS_H */
