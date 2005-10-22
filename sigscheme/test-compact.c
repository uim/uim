/*===========================================================================
 *  FileName : test-compact.c
 *  About    : scheme object compacting test
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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
#include "sigschemetype-compact.h"


#define SCM_ASSERT(cond) \
    ((cond) || die(__FILE__, __LINE__))
static int die(const char *filename, int line)
{
    printf("assertion faled. (file : %s, line : %d)\n", filename, line);
    return -1;
}

static void check_entype(void);
static ScmObj check_int(void);

int main(void)
{
    check_entype();

    check_int();
}

static void check_entype(void)
{
    ScmObj var;

    SCM_ENTYPE_CONS(var);
    SCM_ASSERT(SCM_CONSP(var));

    SCM_ENTYPE_CLOSURE(var);
    SCM_ASSERT(SCM_CLOSUREP(var));

    SCM_ENTYPE_SYMBOL(var);
    SCM_ASSERT(SCM_SYMBOLP(var));

    SCM_ENTYPE_STRING(var);
    SCM_ASSERT(SCM_STRINGP(var));

    SCM_ENTYPE_VECTOR(var);
    SCM_ASSERT(SCM_VECTORP(var));

    SCM_ENTYPE_VALUES(var);
    SCM_ASSERT(SCM_VALUEPACKETP(var));

    SCM_ENTYPE_FUNC(var);
    SCM_ASSERT(SCM_FUNCP(var));//

    SCM_ENTYPE_PORT(var);
    SCM_ASSERT(SCM_PORTP(var));//

    SCM_ENTYPE_CONTINUATION(var);
    SCM_ASSERT(SCM_CONTINUATIONP(var));//

    SCM_ENTYPE_C_POINTER(var);
    SCM_ASSERT(SCM_C_POINTERP(var));//

    SCM_ENTYPE_C_FUNCPOINTER(var);
    SCM_ASSERT(SCM_C_FUNCPOINTERP(var));//
}

static ScmObj check_int(void)
{
    ScmObj var;

    /* entyping */
    SCM_ENTYPE_INT(var);
    SCM_ASSERT(SCM_INTP(var));

    /* value */
    SCM_INT_SET_VALUE(var, 1);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == 1);

    SCM_INT_SET_VALUE(var, 0);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == 0);

    SCM_INT_SET_VALUE(var, -10);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == -10);
}
