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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "sigschemetype-compact.h"

typedef void (*ScmCFunc)(void);

#define SCM_ASSERT(cond) \
    ((cond) || die(__FILE__, __LINE__))

#define ASSERT_TYPE(pred, typename, obj) \
    (SCM_ASSERT(pred(obj)))

#define ASSERT_INTP(obj)          ASSERT_TYPE(SCM_INTP, "integer", (obj))
#define ASSERT_CONSP(obj)         ASSERT_TYPE(SCM_CONSP, "pair", (obj))
#define ASSERT_SYMBOLP(obj)       ASSERT_TYPE(SCM_SYMBOLP, "symbol", (obj))
#define ASSERT_CHARP(obj)         ASSERT_TYPE(SCM_CHARP, "character", (obj))
#define ASSERT_STRINGP(obj)       ASSERT_TYPE(SCM_STRINGP, "string", (obj))
#define ASSERT_FUNCP(obj)         ASSERT_TYPE(SCM_FUNCP, "function", (obj))
#define ASSERT_CLOSUREP(obj)      ASSERT_TYPE(SCM_CLOSUREP, "closure", (obj))
#define ASSERT_VECTORP(obj)       ASSERT_TYPE(SCM_VECTORP, "vector", (obj))
#define ASSERT_PORTP(obj)         ASSERT_TYPE(SCM_PORTP, "port", (obj))
#define ASSERT_CONTINUATIONP(obj) ASSERT_TYPE(SCM_CONTINUATIONP, "continuation", (obj))
#define ASSERT_PROCEDUREP(obj)    ASSERT_TYPE(SCM_PROCEDUREP, "procedure", (obj))
#define ASSERT_ENVP(obj)          ASSERT_TYPE(SCM_ENVP, "environment specifier", (obj))

static int die(const char *filename, int line)
{
    printf("assertion faled. (file : %s, line : %d)\n", filename, line);
    return -1;
}

static void *malloc_aligned(size_t size)
{
    void *p;
    posix_memalign(&p, 32, size);
    return p;
}

static void* aligned_strdup(const char *str)
{
    char *ret = (char*)malloc_aligned(sizeof(char) * strlen(str) + 1);
    strcpy(ret, str);

    return ret;
}

ScmObj Scm_CheckInt(int val)
{
    ScmObj var;

    SCM_ENTYPE_INT(var);
    SCM_ASSERT(SCM_INTP(var));

    SCM_INT_SET_VALUE(var, 1);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == 1);

    SCM_INT_SET_VALUE(var, 0);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == 0);

    SCM_INT_SET_VALUE(var, -10);
    SCM_ASSERT(SCM_INTP(var));
    SCM_ASSERT(SCM_INT_VALUE(var) == -10);

    SCM_INT_SET_VALUE(var, val);

    return var;
}

ScmObj Scm_CheckCons()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj car = Scm_CheckInt(1);
    ScmObj cdr = Scm_CheckInt(2);

    SCM_ENTYPE_CONS(obj);
    SCM_ASSERT(SCM_CONSP(obj));

    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_CONSP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CAR(obj), car));
    SCM_ASSERT(SCM_INTP(SCM_CAR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CAR(obj)) == 1);

    SCM_CONS_SET_CDR(obj, cdr);
    SCM_ASSERT(SCM_CONSP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CDR(obj), cdr));
    SCM_ASSERT(SCM_INTP(SCM_CDR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CDR(obj)) == 2);

    return obj;
}

ScmObj Scm_CheckSymbol(const char *name)
{
    ScmObj obj   = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj vcell = Scm_CheckInt(1);

    SCM_ENTYPE_SYMBOL(obj);
    SCM_ASSERT(SCM_SYMBOLP(obj));

    SCM_SYMBOL_SET_NAME(obj, aligned_strdup(name));
    SCM_ASSERT(SCM_SYMBOLP(obj));
    SCM_ASSERT(strcmp(SCM_SYMBOL_NAME(obj), name) == 0);

    SCM_SYMBOL_SET_VCELL(obj, vcell);
    SCM_ASSERT(SCM_SYMBOLP(obj));
    SCM_ASSERT(SCM_EQ(SCM_SYMBOL_VCELL(obj), vcell));
    SCM_ASSERT(SCM_INTP(SCM_SYMBOL_VCELL(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_SYMBOL_VCELL(obj)) == 1);

    return obj;
}

ScmObj Scm_CheckChar(char *ch)
{
#define SCM_MB_MAX_LEN 4

    ScmObj obj;
    char *val = aligned_strdup(ch);

    SCM_ASSERT(strlen(ch) <= SCM_MB_MAX_LEN);

    SCM_ENTYPE_CHAR(obj);
    SCM_ASSERT(SCM_CHARP(obj));

    SCM_CHAR_SET_VALUE(obj, val);
    SCM_ASSERT(SCM_CHARP(obj));
    SCM_ASSERT(SCM_CHAR_VALUE(obj) == val);
    SCM_ASSERT(strcmp(SCM_CHAR_VALUE(obj), ch) == 0);

    return obj;
}

ScmObj Scm_CheckStringCopying(char *str)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    SCM_ENTYPE_STRING(obj);
    SCM_ASSERT(SCM_STRINGP(obj));

    SCM_STRING_SET_STR(obj, aligned_strdup(str));
    SCM_ASSERT(SCM_STRINGP(obj));
    SCM_ASSERT(strcmp(SCM_STRING_STR(obj), str) == 0);

    SCM_STRING_SET_LEN(obj, strlen(str));
    SCM_ASSERT(SCM_STRINGP(obj));
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    return obj;
}

ScmObj Scm_CheckFunc()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    SCM_ENTYPE_FUNC(obj);
    SCM_ASSERT(SCM_FUNCP(obj));

    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED);
    SCM_ASSERT(SCM_FUNCP(obj));
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED);

    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_FUNCP(obj));
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED_TAIL_REC);

    SCM_FUNC_SET_CFUNC(obj, Scm_CheckFunc);
    SCM_ASSERT(SCM_FUNCP(obj));
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == Scm_CheckFunc);

    SCM_FUNC_SET_CFUNC(obj, Scm_CheckCons);
    SCM_ASSERT(SCM_FUNCP(obj));
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == Scm_CheckCons);

    return obj;
}

ScmObj Scm_CheckClosure()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj exp = Scm_CheckCons();
    ScmObj env = Scm_CheckCons();

    SCM_ENTYPE_CLOSURE(obj);
    SCM_ASSERT(SCM_CLOSUREP(obj));

    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_ASSERT(SCM_CONSP(SCM_CLOSURE_EXP(obj)));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_EXP(obj), exp));
    SCM_ASSERT(SCM_INTP(SCM_CAR(SCM_CLOSURE_EXP(obj))));
    SCM_ASSERT(SCM_INTP(SCM_CDR(SCM_CLOSURE_EXP(obj))));

    SCM_CLOSURE_SET_ENV(obj, env);
    SCM_ASSERT(SCM_CONSP(SCM_CLOSURE_ENV(obj)));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_ENV(obj), env));
    SCM_ASSERT(SCM_INTP(SCM_CAR(SCM_CLOSURE_ENV(obj))));
    SCM_ASSERT(SCM_INTP(SCM_CDR(SCM_CLOSURE_ENV(obj))));

    return obj;
}

ScmObj Scm_CheckVector(int len)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj *vec = (ScmObj*)malloc(sizeof(ScmObj) * len);

    SCM_ENTYPE_VECTOR(obj);
    SCM_ASSERT(SCM_VECTORP(obj));

    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_ASSERT(SCM_VECTORP(obj));
    SCM_ASSERT(SCM_VECTOR_VEC(obj) == vec);

    SCM_VECTOR_SET_LEN(obj, len);
    SCM_ASSERT(SCM_VECTORP(obj));
    SCM_ASSERT(SCM_VECTOR_LEN(obj) == len);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(11));
    SCM_ASSERT(SCM_VECTORP(obj));
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 11);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(3));
    SCM_ASSERT(SCM_VECTORP(obj));
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 3);

    return obj;
}


ScmObj Scm_CheckPort()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmCharPort port;

    SCM_ENTYPE_PORT(obj);
    SCM_ASSERT(SCM_PORTP(obj));

    SCM_PORT_SET_FLAG(obj, SCM_PORTFLAG_INPUT);
    SCM_ASSERT(SCM_PORTP(obj));
    SCM_ASSERT(SCM_PORT_FLAG(obj) == SCM_PORTFLAG_INPUT);

    SCM_PORT_SET_IMPL(obj, &port);
    SCM_ASSERT(SCM_PORTP(obj));
    SCM_ASSERT(SCM_PORT_IMPL(obj) == &port);

    return obj;
}


ScmObj Scm_CheckContinuation(void)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    void *val = (void*)0x20;

    SCM_ENTYPE_CONTINUATION(obj);
    SCM_ASSERT(SCM_CONTINUATIONP(obj));

    SCM_CONTINUATION_SET_OPAQUE(obj, val);
    SCM_ASSERT(SCM_CONTINUATIONP(obj));
    SCM_ASSERT(SCM_CONTINUATION_OPAQUE(obj) == val);

    SCM_CONTINUATION_SET_TAG(obj, 10);
    SCM_ASSERT(SCM_CONTINUATIONP(obj));
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 10);

    SCM_CONTINUATION_SET_TAG(obj, 0);
    SCM_ASSERT(SCM_CONTINUATIONP(obj));
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 0);

    return obj;
}

ScmObj Scm_CheckValuePacket()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj values = Scm_CheckCons();

    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_ASSERT(SCM_VALUEPACKETP(obj));

    SCM_VALUEPACKET_SET_VALUES(obj, values);
    SCM_ASSERT(SCM_VALUEPACKETP(obj));
    SCM_ASSERT(SCM_EQ(SCM_VALUEPACKET_VALUES(obj), values));
    SCM_ASSERT(SCM_CONSP(SCM_VALUEPACKET_VALUES(obj)));

    return obj;
}

ScmObj Scm_CheckCPointer()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    void *data = (void*)0x10;

    SCM_ENTYPE_C_POINTER(obj);
    SCM_ASSERT(SCM_C_POINTERP(obj));
    
    SCM_C_POINTER_SET_VALUE(obj, data);
    SCM_ASSERT(SCM_C_POINTERP(obj));
    SCM_ASSERT(SCM_C_POINTER_VALUE(obj) == data);

    return obj;
}

static void test_func()
{
    ;
}

ScmObj Scm_CheckCFuncPointer()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_ASSERT(SCM_C_FUNCPOINTERP(obj));

    SCM_C_FUNCPOINTER_SET_VALUE(obj, test_func);
    SCM_ASSERT(SCM_C_FUNCPOINTERP(obj));
    SCM_ASSERT(SCM_C_FUNCPOINTER_VALUE(obj) == (void*)test_func);

    return obj;
}

int main(void)
{
    Scm_CheckInt(0);
    Scm_CheckCons();
    Scm_CheckSymbol("aiueo");
    Scm_CheckChar("a");
    Scm_CheckStringCopying("aiueo");
    Scm_CheckFunc();
    Scm_CheckVector(5);
    Scm_CheckValuePacket();
    Scm_CheckContinuation();
    Scm_CheckPort();
    Scm_CheckCPointer();
    Scm_CheckCFuncPointer();
}
