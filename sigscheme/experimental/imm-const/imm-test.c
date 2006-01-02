/*===========================================================================
 *  FileName : imm-test.c
 *  About    : Efficiency evaluation for immediate constant values (temporary)
 *
 *  Copyright (C) 2005-2006 YamaKen
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/

#define NULL ((void *)0)

#define SCM_VALUE_MASK     (~0 ^ (SCM_GCBIT_MASK | SCM_TAG_MASK))

#define SCM_GCBIT_MASK     0x1
#define SCM_GCBIT_UNMARKED 0x0
#define SCM_GCBIT_MARKED   0x1

#define SCM_TAG_MASK    0x6
#define SCM_TAG_CONS    0x0
#define SCM_TAG_CLOSURE 0x2
#define SCM_TAG_OTHERS  0x4
#define SCM_TAG_IMM     0x6

#define SCM_IMM_TAG_MASK     0x18
#define SCM_IMM_TAG_CONST    0x00
#define SCM_IMM_TAG_CHAR     0x10
#define SCM_IMM_TAG_INT_EVEN 0x08
#define SCM_IMM_TAG_INT_ODD  0x18

#define SCM_IMM_CONST_OFFSET 5

#define SCM_IMMCONST(id)                                                     \
    ((ScmObj)((id << SCM_IMM_CONST_OFFSET) | SCM_IMM_TAG_CONST | SCM_TAG_IMM))

#define SCM_FALSE   SCM_IMMCONST(0x0)
#define SCM_NULL    SCM_IMMCONST(0x1)
#define SCM_UNBOUND SCM_IMMCONST(0x2)
#define SCM_EOF     SCM_IMMCONST(0x3)
#define SCM_UNDEF   SCM_IMMCONST(0x4)
#define SCM_TRUE    SCM_IMMCONST(0x5)

/* NULL|tag style const representation */
#define SCM_NULLTAG_CONST_FALSE   ((ScmObj)((int)NULL | SCM_TAG_CONS))
#define SCM_NULLTAG_CONST_NULL    ((ScmObj)((int)NULL | SCM_TAG_CLOSURE))
#define SCM_NULLTAG_CONST_UNBOUND ((ScmObj)((int)NULL | SCM_TAG_OTHERS))
#define SCM_NULLTAG_CONST_TRUE    scm_ntc_true

#define FALSEP(obj) (obj == SCM_FALSE)
#define NULLTAG_CONST_FALSEP(obj) (obj == SCM_NULLTAG_CONST_FALSE)

#define NULLP(obj) (obj == SCM_NULL)
#define NULLTAG_CONST_NULLP(obj) (obj == SCM_NULLTAG_CONST_NULL)

#define CONSP(obj) ((((int)obj) & SCM_TAG_MASK) == SCM_TAG_CONS)
#define NULLTAG_CONST_CONSP(obj)                                             \
    (((((int)obj) & SCM_TAG_MASK) == SCM_TAG_CONS)                           \
     && (obj != SCM_NULLTAG_CONST_FALSE))

#define CAR(cell) (((ScmCell *)cell)->car)
#define CDR(cell) (((ScmCell *)cell)->cdr)

typedef struct ScmCell_ ScmCell;
struct ScmCell_ {
    void *car;
    void *cdr;
};

typedef ScmCell *ScmObj;

ScmObj scm_ntc_true;

int
falsep(ScmObj obj)
{
    return (obj == SCM_FALSE);
}

int
ntc_falsep(ScmObj obj)
{
    return (obj == SCM_NULLTAG_CONST_FALSE);
}

int
nullp(ScmObj obj)
{
    return (obj == SCM_NULL);
}

int
ntc_nullp(ScmObj obj)
{
    return (obj == SCM_NULLTAG_CONST_NULL);
}

int
consp(ScmObj obj)
{
    return CONSP(obj);
}

int
ntc_consp(ScmObj obj)
{
    return NULLTAG_CONST_CONSP(obj);
}

ScmObj
memq(ScmObj key, ScmObj lst)
{
    ScmObj rest;

    for (rest = lst; CONSP(rest); rest = CDR(rest)) {
        if (CAR(rest) == key)
            return rest;
    }
    return SCM_FALSE;
}

ScmObj
ntc_memq(ScmObj key, ScmObj lst)
{
    ScmObj rest;

    for (rest = lst; NULLTAG_CONST_CONSP(rest); rest = CDR(rest)) {
        if (CAR(rest) == key)
            return rest;
    }
    return SCM_NULLTAG_CONST_FALSE;
}

ScmObj
and(ScmObj lst)
{
    ScmObj rest;

    for (rest = lst; CONSP(rest); rest = CDR(rest)) {
        if (FALSEP(rest))
            return SCM_FALSE;
    }
    return SCM_TRUE;
}

ScmObj
ntc_and(ScmObj lst)
{
    ScmObj rest;

    for (rest = lst; NULLTAG_CONST_CONSP(rest); rest = CDR(rest)) {
        if (NULLTAG_CONST_FALSEP(rest))
            return SCM_NULLTAG_CONST_FALSE;
    }
    return SCM_NULLTAG_CONST_TRUE;
}
