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

#include "sigscheme.h"
#include "sigschemetype-compact.h"

#define PRINT_SECTION(SECTIONNAME)                      \
    do {                                                \
        printf("-------- Check %s --------\n", SECTIONNAME);  \
    } while (/*CONSTCOND*/ 0)

#undef SCM_ASSERT
#define SCM_ASSERT(cond)                      \
    ((cond) || die(__FILE__, __LINE__))

#define ASSERT_TYPE(expected, actual, pred)                             \
    do {                                                                \
        if (!(pred)) {                                                  \
            printf("expected \"%s\" but judged as \"%s\"\n",            \
                   typecode2typestr(expected),                          \
                   typecode2typestr(actual));                           \
        }                                                               \
    } while(0)

static int die(const char *filename, int line)
{
    printf("assertion faled. (file : %s, line : %d)\n", filename, line);
    return -1;
}

static const char* typecode2typestr(enum ScmObjType type)
{
    switch (type) {
    case ScmInt: return "Int";
    case ScmCons: return "Cons";
    case ScmSymbol: return "Symbol";
    case ScmChar: return "Char";
    case ScmString: return "String";
    case ScmFunc: return "Func";
    case ScmClosure: return "Closure";
    case ScmVector: return "Vector";
    case ScmPort: return "Port";
    case ScmContinuation: return "Continuation";
    case ScmConstant: return "Constant";
    case ScmValuePacket: return "ValuePacket";
    case ScmFreeCell: return "FreeCell";
    case ScmCPointer: return "CPointer";
    case ScmCFuncPointer: return "CFuncPointer";
    default:
        break;
    }

    return "Invalid";
}

static void check_type(enum ScmObjType type, ScmObj obj)
{
    if (type == ScmInt)
        SCM_ASSERT(SCM_INTP(obj));
    else
        ASSERT_TYPE(type, ScmInt, !SCM_INTP(obj));

    if (type == ScmCons)
        SCM_ASSERT(SCM_CONSP(obj));
    else
        ASSERT_TYPE(type, ScmCons, !SCM_CONSP(obj));

    if (type == ScmSymbol)
        SCM_ASSERT(SCM_SYMBOLP(obj));
    else
        ASSERT_TYPE(type, ScmSymbol, !SCM_SYMBOLP(obj));

    if (type == ScmChar)
        SCM_ASSERT(SCM_CHARP(obj));
    else
        ASSERT_TYPE(type, ScmChar, !SCM_CHARP(obj));

    if (type == ScmString)
        SCM_ASSERT(SCM_STRINGP(obj));
    else
        ASSERT_TYPE(type, ScmString, !SCM_STRINGP(obj));

    if (type == ScmFunc)
        SCM_ASSERT(SCM_FUNCP(obj));
    else
        ASSERT_TYPE(type, ScmFunc, !SCM_FUNCP(obj));

    if (type == ScmClosure)
        SCM_ASSERT(SCM_CLOSUREP(obj));
    else
        ASSERT_TYPE(type, ScmClosure, !SCM_CLOSUREP(obj));

    if (type == ScmVector)
        SCM_ASSERT(SCM_VECTORP(obj));
    else
        ASSERT_TYPE(type, ScmVector, !SCM_VECTORP(obj));

    if (type == ScmPort)
        SCM_ASSERT(SCM_PORTP(obj));
    else
        ASSERT_TYPE(type, ScmPort, !SCM_PORTP(obj));

    if (type == ScmContinuation)
        SCM_ASSERT(SCM_CONTINUATIONP(obj));
    else
        ASSERT_TYPE(type, ScmContinuation, !SCM_CONTINUATIONP(obj));

    if (type == ScmConstant)
        SCM_ASSERT(SCM_CONSTANTP(obj));
    else
        ASSERT_TYPE(type, ScmConstant, !SCM_CONSTANTP(obj));

    if (type == ScmValuePacket)
        SCM_ASSERT(SCM_VALUEPACKETP(obj));
    else
        ASSERT_TYPE(type, ScmValuePacket, !SCM_VALUEPACKETP(obj));

    if (type == ScmCPointer)
        SCM_ASSERT(SCM_C_POINTERP(obj));
    else
        ASSERT_TYPE(type, ScmCPointer, !SCM_C_POINTERP(obj));

    if (type == ScmCFuncPointer)
        SCM_ASSERT(SCM_C_FUNCPOINTERP(obj));
    else
        ASSERT_TYPE(type, ScmCFuncPointer, !SCM_C_FUNCPOINTERP(obj));
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
    ScmObj obj;

    PRINT_SECTION("Int");

    SCM_ENTYPE_INT(obj);
    check_type(ScmInt, obj);

    SCM_INT_SET_VALUE(obj, 1);
    check_type(ScmInt, obj);
    SCM_ASSERT(SCM_INT_VALUE(obj) == 1);

    SCM_INT_SET_VALUE(obj, 0);
    check_type(ScmInt, obj);
    SCM_ASSERT(SCM_INT_VALUE(obj) == 0);

    SCM_INT_SET_VALUE(obj, -10);
    check_type(ScmInt, obj);
    SCM_ASSERT(SCM_INT_VALUE(obj) == -10);

    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj Scm_CheckCons()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj car = Scm_CheckInt(1);
    ScmObj cdr = Scm_CheckInt(2);

    PRINT_SECTION("Cons");

    /* entyping */
    SCM_ENTYPE_CONS(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCons, obj);

    /* unmarked state */
    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCons, obj);

    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_EQ(SCM_CAR(obj), car));
    SCM_ASSERT(SCM_INTP(SCM_CAR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CAR(obj)) == 1);

    SCM_CONS_SET_CDR(obj, cdr);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CDR(obj), cdr));
    SCM_ASSERT(SCM_INTP(SCM_CDR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CDR(obj)) == 2);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CAR(obj), car));
    SCM_ASSERT(SCM_INTP(SCM_CAR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CAR(obj)) == 1);

    SCM_CONS_SET_CDR(obj, cdr);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CDR(obj), cdr));
    SCM_ASSERT(SCM_INTP(SCM_CDR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CDR(obj)) == 2);

    return obj;
}

ScmObj Scm_CheckSymbol(const char *name)
{
    ScmObj obj   = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj vcell = Scm_CheckInt(1);

    PRINT_SECTION("Symbol");

    /* entyping */
    SCM_ENTYPE_SYMBOL(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmSymbol, obj);

    /* unmarked state */
    SCM_SYMBOL_SET_NAME(obj, aligned_strdup(name));
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(strcmp(SCM_SYMBOL_NAME(obj), name) == 0);

    SCM_SYMBOL_SET_VCELL(obj, vcell);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(SCM_EQ(SCM_SYMBOL_VCELL(obj), vcell));
    SCM_ASSERT(SCM_INTP(SCM_SYMBOL_VCELL(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_SYMBOL_VCELL(obj)) == 1);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_SYMBOL_SET_NAME(obj, aligned_strdup(name));
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(strcmp(SCM_SYMBOL_NAME(obj), name) == 0);

    SCM_SYMBOL_SET_VCELL(obj, vcell);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(SCM_EQ(SCM_SYMBOL_VCELL(obj), vcell));
    SCM_ASSERT(SCM_INTP(SCM_SYMBOL_VCELL(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_SYMBOL_VCELL(obj)) == 1);

    return obj;
}

ScmObj Scm_CheckChar(char *ch)
{
    ScmObj obj;
    PRINT_SECTION("Char");

    /* entyping */
    SCM_ENTYPE_CHAR(obj);
    check_type(ScmChar, obj);

    SCM_CHAR_SET_VALUE(obj, 0);
    check_type(ScmChar, obj);
    SCM_ASSERT(SCM_CHAR_VALUE(obj) == 0);

    SCM_CHAR_SET_VALUE(obj, 255);
    check_type(ScmChar, obj);
    SCM_ASSERT(SCM_CHAR_VALUE(obj) == 255);

    return obj;
}

#if 0
ScmObj Scm_CheckStringCopying(char *str)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    PRINT_SECTION("String");

    /* entyping */
    SCM_ENTYPE_STRING(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmString, obj);

    /* unmarked state */
    SCM_STRING_SET_STR(obj, aligned_strdup(str));
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strcmp(SCM_STRING_STR(obj), str) == 0);

    SCM_STRING_SET_LEN(obj, strlen(str));
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_STRING_SET_STR(obj, aligned_strdup(str));
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strcmp(SCM_STRING_STR(obj), str) == 0);

    SCM_STRING_SET_LEN(obj, strlen(str));
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    return obj;
}
#endif

ScmObj Scm_CheckFunc()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    PRINT_SECTION("Func");

    /* entyping */
    SCM_ENTYPE_FUNC(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmFunc, obj);

    /* unmarked state */
    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED_TAIL_REC);

    SCM_FUNC_SET_CFUNC(obj, Scm_CheckFunc);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == Scm_CheckFunc);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED_TAIL_REC);

    SCM_FUNC_SET_CFUNC(obj, Scm_CheckFunc);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == Scm_CheckFunc);

    return obj;
}

ScmObj Scm_CheckClosure()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj exp = Scm_CheckCons();
    ScmObj env = Scm_CheckCons();

    PRINT_SECTION("Closure");

    /* entyping */
    SCM_ENTYPE_CLOSURE(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmClosure, obj);

    /* unmarked state */
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_EXP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_EXP(obj), exp));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_EXP(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_EXP(obj)));

    SCM_CLOSURE_SET_ENV(obj, env);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_ENV(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_ENV(obj), env));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_ENV(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_ENV(obj)));

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_EXP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_EXP(obj), exp));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_EXP(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_EXP(obj)));

    SCM_CLOSURE_SET_ENV(obj, env);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_ENV(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_ENV(obj), env));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_ENV(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_ENV(obj)));

    return obj;
}

ScmObj Scm_CheckVector(int len)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj *vec = (ScmObj*)malloc(sizeof(ScmObj) * len);

    PRINT_SECTION("Vector");

    /* entyping */
    SCM_ENTYPE_VECTOR(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmVector, obj);

    /* unmarked state */
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_VEC(obj) == vec);

    SCM_VECTOR_SET_LEN(obj, len);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_LEN(obj) == len);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(11));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 11);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(3));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 3);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_VEC(obj) == vec);

    SCM_VECTOR_SET_LEN(obj, len);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_LEN(obj) == len);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(11));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 11);

    SCM_VECTOR_SET_CREF(obj, 0, Scm_CheckInt(3));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_CREF(obj, 0)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_CREF(obj, 0)) == 3);

    return obj;
}


ScmObj Scm_CheckPort()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmCharPort *port = (ScmCharPort*)0x20;

    PRINT_SECTION("Port");

    /* entyping */
    SCM_ENTYPE_PORT(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmPort, obj);

    /* unmarked state */
    SCM_PORT_SET_FLAG(obj, SCM_PORTFLAG_INPUT);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_FLAG(obj) == SCM_PORTFLAG_INPUT);

    SCM_PORT_SET_IMPL(obj, port);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_IMPL(obj) == port);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_PORT_SET_FLAG(obj, SCM_PORTFLAG_INPUT);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_FLAG(obj) == SCM_PORTFLAG_INPUT);

    SCM_PORT_SET_IMPL(obj, port);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_IMPL(obj) == port);

    return obj;
}


ScmObj Scm_CheckContinuation(void)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    void *val = (void*)0x20;

    PRINT_SECTION("Continuation");

    /* entyping */
    SCM_ENTYPE_CONTINUATION(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmContinuation, obj);

    /* unmarked state */
    SCM_CONTINUATION_SET_OPAQUE(obj, val);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_OPAQUE(obj) == val);

    SCM_CONTINUATION_SET_TAG(obj, 10);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 10);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_CONTINUATION_SET_OPAQUE(obj, val);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_OPAQUE(obj) == val);

    SCM_CONTINUATION_SET_TAG(obj, 10);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 10);

    return obj;
}

ScmObj Scm_CheckValuePacket()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj values = Scm_CheckCons();

    PRINT_SECTION("ValuePacket");

    /* entyping */
    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmValuePacket, obj);

    /* unmarked state */
    SCM_VALUEPACKET_SET_VALUES(obj, values);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmValuePacket, obj);
    SCM_ASSERT(SCM_EQ(SCM_VALUEPACKET_VALUES(obj), values));
    SCM_ASSERT(SCM_CONSP(SCM_VALUEPACKET_VALUES(obj)));

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_VALUEPACKET_SET_VALUES(obj, values);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmValuePacket, obj);
    SCM_ASSERT(SCM_EQ(SCM_VALUEPACKET_VALUES(obj), values));
    SCM_ASSERT(SCM_CONSP(SCM_VALUEPACKET_VALUES(obj)));

    return obj;
}

ScmObj Scm_CheckCPointer()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    void *data = (void*)0x10;

    PRINT_SECTION("CPointer");

    /* entyping state */
    SCM_ENTYPE_C_POINTER(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCPointer, obj);

    /* unmarked state */
    SCM_C_POINTER_SET_VALUE(obj, data);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCPointer, obj);
    SCM_ASSERT(SCM_C_POINTER_VALUE(obj) == data);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_C_POINTER_SET_VALUE(obj, data);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmCPointer, obj);
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

    PRINT_SECTION("CFuncPointer");

    /* entyping */
    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCFuncPointer, obj);

    /* unmarked state */
    SCM_C_FUNCPOINTER_SET_VALUE(obj, test_func);
    SCM_ASSERT(SCM_IS_UNMARKED(obj));
    check_type(ScmCFuncPointer, obj);
    SCM_ASSERT(SCM_C_FUNCPOINTER_VALUE(obj) == (ScmCFunc)test_func);

    /* marked state */
    SCM_DO_MARK(obj);
    SCM_ASSERT(SCM_IS_MARKED(obj));

    SCM_C_FUNCPOINTER_SET_VALUE(obj, test_func);
    SCM_ASSERT(SCM_IS_MARKED(obj));
    check_type(ScmCFuncPointer, obj);
    SCM_ASSERT(SCM_C_FUNCPOINTER_VALUE(obj) == (ScmCFunc)test_func);

    return obj;
}

ScmObj Scm_CheckConstant()
{
    PRINT_SECTION("Constant");

    SCM_ASSERT(SCM_CONSTANTP(SCM_NULL));
    SCM_ASSERT(SCM_NULLP(SCM_NULL));
    SCM_ASSERT(!SCM_INTP(SCM_NULL));
    check_type(ScmConstant, SCM_NULL);

    SCM_ASSERT(SCM_CONSTANTP(SCM_INVALID));
    SCM_ASSERT(SCM_INVALIDP(SCM_INVALID));
    SCM_ASSERT(!SCM_INTP(SCM_INVALID));
    check_type(ScmConstant, SCM_INVALID);

    SCM_ASSERT(SCM_CONSTANTP(SCM_UNBOUND));
    SCM_ASSERT(SCM_TAG_IMM_UNBOUNDP(SCM_UNBOUND));
    check_type(ScmConstant, SCM_UNBOUND);

    SCM_ASSERT(SCM_CONSTANTP(SCM_FALSE));
    SCM_ASSERT(SCM_FALSEP(SCM_FALSE));
    check_type(ScmConstant, SCM_FALSE);

    SCM_ASSERT(SCM_CONSTANTP(SCM_TRUE));
    SCM_ASSERT(SCM_TAG_IMM_TRUEP(SCM_TRUE));
    check_type(ScmConstant, SCM_TRUE);

    SCM_ASSERT(SCM_CONSTANTP(SCM_EOF));
    SCM_ASSERT(SCM_EOFP(SCM_EOF));
    check_type(ScmConstant, SCM_EOF);

    SCM_ASSERT(SCM_CONSTANTP(SCM_UNDEF));
    SCM_ASSERT(SCM_TAG_IMM_UNDEFP(SCM_UNDEF));
    check_type(ScmConstant, SCM_UNDEF);

    return SCM_NULL;
}

int main(void)
{
    Scm_CheckInt(0);
    Scm_CheckCons();
    Scm_CheckSymbol("aiueo");
    Scm_CheckChar("a");
#if 0
    Scm_CheckStringCopying("aiueo");
#endif
    Scm_CheckClosure();
    Scm_CheckFunc();
    Scm_CheckVector(5);
    Scm_CheckValuePacket();
    Scm_CheckContinuation();
    Scm_CheckPort();
    Scm_CheckCPointer();
    Scm_CheckCFuncPointer();
    Scm_CheckConstant();
}
