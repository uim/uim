/*===========================================================================
 *  Filename : test-gc.c
 *  About    : garbage collector test
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
#include <sigscheme/config.h>
#if !(SCM_USE_STORAGE_COMPACT || SCM_USE_STORAGE_FATTY)
#define TST_EXCLUDE_THIS
#endif

#include "sscm-test.h"
#include "sigscheme.h"
#include "sigschemeinternal.h"
#define HEAP_SIZE    SCM_DEFAULT_HEAP_SIZE


void saturate_heap(size_t heap_size);
ScmObj alloc (TST_PARAMS_DECL);
void verify (ScmObj obj, TST_PARAMS_DECL);

void
saturate_heap (size_t size)
{
    /* We overallocate objects to force the GC to kick in.  This is
     * problematic if the new objects occupy cells that were
     * inadvertently freed due to a bug in GC.  To guard against that
     * possibility, we allocate closures (which the caller doesn't use
     * by convention) so that type predicates that the caller will do
     * on its objects won't accidentally succeed. */
    while (size--)
        SCM_MAKE_CLOSURE(SCM_NULL, SCM_NULL);
}


ScmObj
alloc (TST_PARAMS_DECL)
{
    return CONS(LIST_1(MAKE_INT(1)), scm_intern("aaa"));
}

#define TST(cond) TST_COND(cond, #cond)
void
verify (ScmObj obj, TST_PARAMS_DECL)
{
    TST_ASSERT(TST(CONSP(obj)));
    TST_ASSERT(TST(SYMBOLP(CDR(obj))));
    obj = CAR(obj);
    TST_ASSERT(TST(CONSP(obj)));
    TST_ASSERT(TST(NULLP(CDR(obj))));
    TST_ASSERT(TST(INTP(CAR(obj))));
    TST_ASSERT(TST_COND(EQVP(CAR(obj), MAKE_INT(1)),
                        "integer=? after GC"));
}

#define ALLOC()   alloc(TST_PARAMS)
#define VERIFY(o) verify((o), TST_PARAMS)

#define TEST(o)                                                           \
    do {                                                                  \
        /* We run it twice to see if the contents of the stack makes a */ \
        /* difference. */                                                 \
        saturate_heap (HEAP_SIZE);                                        \
        VERIFY(o);                                                        \
        saturate_heap (HEAP_SIZE);                                        \
        VERIFY(o);                                                        \
    } while (0)

TST_CASE("GC mark stack")
{
    ScmObj obj;

    obj = ALLOC ();
    TEST (obj);
}


TST_CASE("GC mark protected var")
{
    ScmObj *var;

    var = malloc (sizeof (*var));
    scm_gc_protect (var);

    *var = ALLOC ();
    TEST (*var);

    scm_gc_unprotect (var);
    free (var);
}

volatile scm_intobj_t false_ptr;

TST_CASE("GC pointer predicate false positives")
{
    ScmObj obj;
    size_t i;

    obj = ALLOC ();
    false_ptr = (scm_intobj_t)obj;
    for (i = 0; i < sizeof (ScmCell); i++) {
        ++false_ptr;
        TEST(obj);
    }
}
