/*===========================================================================
 *  Filename : utils.c
 *  About    : utilities that may be #include's in tests
 *
 *  Copyright (C) 2006 Jun Inoue <jun.lambda@gmail.com>
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

#define ALIGN_HEAP 8

static void *malloc_aligned_8(size_t size) SCM_UNUSED;
static void *aligned_dup(void *p, size_t size) SCM_UNUSED;

static void *
malloc_aligned_8(size_t size)
{
    void *ret;
#if HAVE_PAGE_ALIGNED_MALLOC
    /* workaround for page-sized scm_malloc_aligned() */
    ret = scm_malloc(size);
    SCM_ASSERT(!((uintptr_t)ret % ALIGN_HEAP));
#else
    ret = scm_malloc_aligned(size);
#endif
    return ret;
}

static void *
aligned_dup(void *p, size_t size)
{
    void *ret;

    ret = malloc_aligned_8(size);
    memcpy(ret, p, size);
    return ret;
}
