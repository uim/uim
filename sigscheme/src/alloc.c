/*===========================================================================
 *  Filename : alloc.c
 *  About    : memory allocators
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

#include <config.h>

#include <stdlib.h>
#include <string.h>
#if HAVE_GETPAGESIZE
#include <unistd.h>
#endif

#include "sigscheme-stdint.h"
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define ALIGN_CELL (sizeof(ScmCell))
/* 8-bytes alignment (not ScmCell alignment) is required by storage-compact. */
#define ALIGN_HEAP 8

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
/* Allocates ScmCell-aligned (32-bit pointer) or ScmObj-aligned (64-bit
 * pointer) memory for heaps. 8-bytes alignment (not ScmCell alignment) is
 * required by storage-compact. */
SCM_EXPORT void *
scm_malloc_aligned(size_t size)
{
    void *p;

#if HAVE_POSIX_MEMALIGN
    /*
     * Cited from manpage of posix_memalign(3) of glibc:
     *
     * CONFORMING TO
     *     The function valloc() appeared in 3.0 BSD. It is documented as being
     *     obsolete in BSD 4.3, and as legacy in SUSv2. It no longer occurs in
     *     SUSv3.  The function memalign() appears in SunOS 4.1.3 but not in
     *     BSD 4.4.  The function posix_memalign() comes from POSIX 1003.1d.
     */
    posix_memalign(&p, ALIGN_CELL, size);
#elif (HAVE_PAGE_ALIGNED_MALLOC && HAVE_GETPAGESIZE)
    if ((size_t)getpagesize() <= size)
        p = scm_malloc(size);
    else
        PLAIN_ERR("cannot ensure memory alignment");
#elif defined(__APPLE__)
    /*
     * malloc in Mac OS X guarantees 16 byte alignment.  And large
     * memory allocations are guaranteed to be page-aligned.  See
     * http://developer.apple.com/documentation/Performance/Conceptual/
     * ManagingMemory/Articles/MemoryAlloc.html
     * -- 
     * ekato Jan 23 2006
     */
    if (ALIGN_HEAP <= 16)
        p = malloc(size);
    else
        PLAIN_ERR("cannot ensure memory alignment");
#else
    /* Assumes that malloc(3) returns at least 8-bytes aligned pointer. */
    if (ALIGN_HEAP <= 8)
        p = malloc(size);
    else
        PLAIN_ERR("cannot ensure memory alignment");
#endif
    SCM_ENSURE_ALLOCATED(p);
    /* heaps must be aligned to sizeof(ScmCell) */
    SCM_ASSERT(!((uintptr_t)p % ALIGN_HEAP));

    return p;
}

SCM_EXPORT void *
scm_malloc(size_t size)
{
    void *p;

    p = malloc(size);
    ENSURE_ALLOCATED(p);

    return p;
}

SCM_EXPORT void *
scm_calloc(size_t number, size_t size)
{
    void *p;

    p = calloc(number, size);
    ENSURE_ALLOCATED(p);

    return p;
}

SCM_EXPORT void *
scm_realloc(void *ptr, size_t size)
{
    void *p;

    p = realloc(ptr, size);
    ENSURE_ALLOCATED(p);

    return p;
}

SCM_EXPORT char *
scm_strdup(const char *str)
{
    char *copied;

#if HAVE_STRDUP
    copied = strdup(str);
    ENSURE_ALLOCATED(copied);
#else
    size_t size;

    size = strlen(str) + sizeof("");
    copied = scm_malloc(size);
    strcpy(copied, str);
#endif

    return copied;
}

#if 0
/* For 'name' slot of symbol object on storage-compact. If your malloc(3) does
 * not ensure 8-bytes alignment, Complete this function and hook this into
 * symbol object creation and modification.  -- YamaKen 2006-05-30 */
SCM_EXPORT char *
scm_align_str(char *str)
{
    char *copied;
    size_t size;

    /* Use ScmCell-alignment to ensure at least 8-bytes aligned. */
    if ((uintptr_t)ptr % ALIGN_CELL) {
        size = strlen(str) + sizeof("");
        copied = scm_malloc_aligned8(size);
        strcpy(copied, str);
        free(str);
        return copied;
    } else {
        return ptr;
    }
}
#endif

/*=======================================
   Extendable Local Buffer
=======================================*/
SCM_EXPORT void
scm_lbuf_init(struct ScmLBuf_void_ *lbuf, void *init_buf, size_t init_size)
{
    lbuf->buf  = lbuf->init_buf  = init_buf;
    lbuf->size = lbuf->init_size = init_size;
    lbuf->extended_cnt = 0;
}

SCM_EXPORT void
scm_lbuf_free(struct ScmLBuf_void_ *lbuf)
{
    if (lbuf->buf != lbuf->init_buf)
        free(lbuf->buf);
}

SCM_EXPORT void
scm_lbuf_alloc(struct ScmLBuf_void_ *lbuf, size_t size)
{
    lbuf->buf = scm_malloc(size);
    lbuf->size = size;
}

SCM_EXPORT void
scm_lbuf_realloc(struct ScmLBuf_void_ *lbuf, size_t size)
{
    if (lbuf->buf == lbuf->init_buf) {
        if (size < lbuf->size)
            lbuf->size = size;
        lbuf->buf = memcpy(scm_malloc(size), lbuf->buf, lbuf->size);
    } else {
        lbuf->buf = scm_realloc(lbuf->buf, size);
    }
    lbuf->size = size;
}

SCM_EXPORT void
scm_lbuf_extend(struct ScmLBuf_void_ *lbuf,
                size_t (*f)(struct ScmLBuf_void_ *), size_t least_size)
{
    size_t new_size;

    if (lbuf->size < least_size) {
        new_size = (*f)(lbuf);
        if (new_size < lbuf->size)
            PLAIN_ERR("local buffer exceeded");
        if (new_size < least_size)
            new_size = least_size;
        scm_lbuf_realloc(lbuf, new_size);
        lbuf->extended_cnt++;
    }
}

SCM_EXPORT size_t
scm_lbuf_f_linear(struct ScmLBuf_void_ *lbuf)
{
    return (lbuf->size + lbuf->init_size);
}

SCM_EXPORT size_t
scm_lbuf_f_exponential(struct ScmLBuf_void_ *lbuf)
{
    return (lbuf->size << 1);
}
