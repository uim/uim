/*===========================================================================
 *  Filename : test-storage-compact.c
 *  About    : storage layer tests specific to storage-compact
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

#include <sigscheme/config.h>
#if !SCM_USE_STORAGE_COMPACT
#define TST_EXCLUDE_THIS
#endif

#include "sscm-test.h"
#include "sigschemeinternal.h"
#include "utils.c"


#if SCM_USE_STORAGE_COMPACT

/* temporary workaround. see the comment of storage.c */
#if 1
#define SCM_CONS_INIT(obj, kar, kdr)                    \
    SCM_TYPESAFE_MACRO_VOID(SCM_SAL_CONS_INIT,          \
                            (ScmObj, ScmObj, ScmObj),   \
                            ((obj), (kar), (kdr)))

#define SCM_SYMBOL_INIT(obj, nam, val)                  \
    SCM_TYPESAFE_MACRO_VOID(SCM_SAL_SYMBOL_INIT,        \
                            (ScmObj, char*, ScmObj),    \
                            ((obj), (nam), (val)))
#endif

TST_CASE("tag-consistent?")
{
    ScmCell *cell;
    ScmObj obj;

    cell = malloc_aligned_8(sizeof(*cell));
    obj = (ScmObj)cell;
    SCM_SYMBOL_INIT(obj, NULL, SCM_NULL);
    TST_COND(SCM_CELL_MISCP(*cell), "cell-misc?");
    TST_COND(SCM_CELL_SYMBOLP(*cell), "cell-symbol?");
    TST_COND(SCM_SYMBOLP(obj), "init -> symbol?");
    TST_COND(SCM_TAG_CONSISTENTP(obj, *cell),
             "consistent? (ptag = misc, cell = misc)");
    SCM_PTAG_SET(obj, SCM_PTAG_CONS);
    TST_COND(CONSP(obj), "set ptag -> pair?");
    TST_COND(!SCM_TAG_CONSISTENTP(obj, *cell),
             "inconsistent? (ptag = pair, cell = misc)");
    SCM_PTAG_SET(obj, SCM_PTAG_CLOSURE);
    TST_COND(SCM_CLOSUREP(obj), "set ptag -> closure?");
    TST_COND(!SCM_TAG_CONSISTENTP(obj, *cell),
             "inconsistent? (ptag = closure, cell = misc)");
    /* Immediate objects pointing to misc cells are harmless. */

    obj = (ScmObj)cell;
    SCM_CONS_INIT(obj, SCM_TRUE, SCM_FALSE);
    TST_COND(!SCM_CELL_MISCP(*cell), "not cell-misc?");
    TST_COND(SCM_CONSP(obj), "init -> pair?");
    TST_COND(SCM_TAG_CONSISTENTP(obj, *cell),
             "consistent? (ptag = pair, cell = pair)");
    SCM_PTAG_SET(obj, SCM_PTAG_CLOSURE);
    TST_COND(SCM_CLOSUREP(obj), "set ptag -> closure?");
    /* Pair and closure have the same memory layout. */
    TST_COND(SCM_TAG_CONSISTENTP(obj, *cell),
             "consistent? (ptag = closure, cell = pair)");
    SCM_PTAG_SET(obj, SCM_PTAG_MISC);
    TST_COND(SCM_MISCP(obj), "set ptag -> misc?");
    TST_COND(!SCM_TAG_CONSISTENTP(obj, *cell),
             "consistent? (ptag = misc, cell = pair)");
    /* Immediate objects pointing to misc cells are harmless. */

    free(cell);
}


static scm_bool
cell_types_disjunct(ScmCell *cell)
{
    int tested_true = 0;
    tested_true += !!SCM_CELL_SYMBOLP(*cell);
    tested_true += !!SCM_CELL_STRINGP(*cell);
    tested_true += !!SCM_CELL_VECTORP(*cell);
    tested_true += !!SCM_CELL_PORTP(*cell);
    tested_true += !!SCM_CELL_CONTINUATIONP(*cell);
    return tested_true == 1;
}

TST_CASE("cell type predicates")
{
    ScmObj obj;

    ScmObj *vec;

#define TYPE_TST(typ)                                           \
    TST_COND(SCM_CELL_##typ##P(*SCM_UNTAG_PTR(obj))             \
             && cell_types_disjunct(SCM_UNTAG_PTR(obj)),        \
             "CELL_" #typ "P()")

    vec = malloc_aligned_8(sizeof(ScmObj) * 3);
    vec[0] = SCM_NULL;
    vec[1] = SCM_MAKE_INT(8);
    vec[2] = SCM_FALSE;
    obj = SCM_MAKE_VECTOR(vec, 3);
    TYPE_TST(VECTOR);

    obj = scm_p_current_input_port();
    TYPE_TST(PORT);

    obj = SCM_SYM_QUOTE;
    TYPE_TST(SYMBOL);
    obj = SCM_MAKE_SYMBOL(NULL, SCM_NULL);
    TYPE_TST(SYMBOL);

    {
        char str[] = "some string";
        char *p;
        p = aligned_dup(str, sizeof(str));
        obj = SCM_MAKE_STRING(p, sizeof(str)-1);
        TYPE_TST(STRING);
    }

    /* TODO: continuation */
}


/* TODO: add tests for the GC algorithm (perhaps by #include'ing a
 * part of storage-gc.c extracted with sed -n '/^gc_mark/,/^}/ p' ) */
#endif /* SCM_USE_STORAGE_COMPACT */
