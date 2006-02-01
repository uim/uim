/*===========================================================================
 *  FileName : test-compact.c
 *  About    : scheme object compacting test
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

#include "config.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

ScmObj scm_check_cons();
ScmObj scm_check_closure();
ScmObj scm_check_symbol();
ScmObj scm_check_string();
ScmObj scm_check_vector();
ScmObj scm_check_port();
ScmObj scm_check_continuation();
ScmObj scm_check_value_packet();
ScmObj scm_check_freecell();
ScmObj scm_check_int(int val);
ScmObj scm_check_char(unsigned int val);
ScmObj scm_check_constant();
ScmObj scm_check_ref();


#define PRINT_SECTION(SECTIONNAME)                      \
    do {                                                \
        printf("-------- Check %s --------\n", SECTIONNAME);  \
    } while (/*CONSTCOND*/ 0)

#undef SCM_ASSERT
#define SCM_ASSERT(cond)                      \
    ((cond) || die(__FILE__, __LINE__))

#undef ASSERT_TYPE
#define ASSERT_TYPE(expected, actual, pred)                             \
    do {                                                                \
        if (!(pred)) {                                                  \
            printf("expected \"%s\" but judged as \"%s\"\n",            \
                   typecode2typestr(expected),                          \
                   typecode2typestr(actual));                           \
        }                                                               \
    } while (/* CONSTCOND */ 0)

static int
die(const char *filename, int line)
{
    printf("assertion faled. (file: %s, line: %d)\n", filename, line);
    return -1;
}

static const char *
typecode2typestr(enum ScmObjType type)
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

static void
check_type(enum ScmObjType type, ScmObj obj)
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

    if (type == ScmFreeCell)
        SCM_ASSERT(SCM_FREECELLP(obj));
    else
        ASSERT_TYPE(type, ScmFreeCell, !SCM_FREECELLP(obj));
}

static void *
malloc_aligned(size_t size)
{
    void *p;
    posix_memalign(&p, 32, size);
    return p;
}

static void *
aligned_strdup(const char *str)
{
    char *ret = NULL;
    if (str) {
        ret = (char *)malloc_aligned(sizeof(char) * strlen(str) + 1);
        strcpy(ret, str);
    }

    return ret;
}

ScmObj
scm_check_cons()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj car = scm_check_int(1);
    ScmObj cdr = scm_check_int(2);

    PRINT_SECTION("Cons");

    /* entyping */
    SCM_ENTYPE_CONS(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCons, obj);

    /* unmarked state */
    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCons, obj);

    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_EQ(SCM_CAR(obj), car));
    SCM_ASSERT(SCM_INTP(SCM_CAR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CAR(obj)) == 1);

    SCM_CONS_SET_CDR(obj, cdr);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CDR(obj), cdr));
    SCM_ASSERT(SCM_INTP(SCM_CDR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CDR(obj)) == 2);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_CONS_SET_CAR(obj, car);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CAR(obj), car));
    SCM_ASSERT(SCM_INTP(SCM_CAR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CAR(obj)) == 1);

    SCM_CONS_SET_CDR(obj, cdr);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmCons, obj);
    SCM_ASSERT(SCM_EQ(SCM_CDR(obj), cdr));
    SCM_ASSERT(SCM_INTP(SCM_CDR(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_CDR(obj)) == 2);

    return obj;
}

ScmObj
scm_check_closure()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj exp = scm_check_cons();
    ScmObj env = scm_check_cons();

    PRINT_SECTION("Closure");

    /* entyping */
    SCM_ENTYPE_CLOSURE(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmClosure, obj);

    /* unmarked state */
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_EXP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_EXP(obj), exp));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_EXP(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_EXP(obj)));

    SCM_CLOSURE_SET_ENV(obj, env);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_ENV(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_ENV(obj), env));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_ENV(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_ENV(obj)));

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_EXP(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_EXP(obj), exp));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_EXP(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_EXP(obj)));

    SCM_CLOSURE_SET_ENV(obj, env);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmClosure, obj);
    check_type(ScmCons, SCM_CLOSURE_ENV(obj));
    SCM_ASSERT(SCM_EQ(SCM_CLOSURE_ENV(obj), env));
    check_type(ScmInt, SCM_CAR(SCM_CLOSURE_ENV(obj)));
    check_type(ScmInt, SCM_CDR(SCM_CLOSURE_ENV(obj)));

    return obj;
}

ScmObj
scm_check_symbol(const char *name)
{
    ScmObj obj   = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj vcell = scm_check_int(1);

    PRINT_SECTION("Symbol");

    /* entyping */
    SCM_ENTYPE_SYMBOL(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmSymbol, obj);

    /* unmarked state */
    SCM_SYMBOL_SET_NAME(obj, aligned_strdup(name));
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmSymbol, obj);
    if (name)
        SCM_ASSERT(strcmp(SCM_SYMBOL_NAME(obj), name) == 0);

    SCM_SYMBOL_SET_VCELL(obj, vcell);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(SCM_EQ(SCM_SYMBOL_VCELL(obj), vcell));
    SCM_ASSERT(SCM_INTP(SCM_SYMBOL_VCELL(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_SYMBOL_VCELL(obj)) == 1);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_SYMBOL_SET_NAME(obj, aligned_strdup(name));
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmSymbol, obj);
    if (name)
        SCM_ASSERT(strcmp(SCM_SYMBOL_NAME(obj), name) == 0);

    SCM_SYMBOL_SET_VCELL(obj, vcell);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmSymbol, obj);
    SCM_ASSERT(SCM_EQ(SCM_SYMBOL_VCELL(obj), vcell));
    SCM_ASSERT(SCM_INTP(SCM_SYMBOL_VCELL(obj)));
    SCM_ASSERT(SCM_INT_VALUE(SCM_SYMBOL_VCELL(obj)) == 1);

    return obj;
}

ScmObj
scm_check_string_copying(char *str)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    PRINT_SECTION("String");

    /* entyping */
    SCM_ENTYPE_STRING(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmString, obj);

    /* unmarked state */
    SCM_STRING_SET_STR(obj, aligned_strdup(str));
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmString, obj);
    if (str)
        SCM_ASSERT(strcmp(SCM_STRING_STR(obj), str) == 0);

    SCM_STRING_SET_LEN(obj, strlen(str));
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    SCM_STRING_SET_MUTABLE(obj);
    SCM_ASSERT(SCM_STRING_MUTATION_TYPE(obj) == SCM_STR_MUTABLE);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    SCM_STRING_SET_IMMUTABLE(obj);
    SCM_ASSERT(SCM_STRING_MUTATION_TYPE(obj) == SCM_STR_IMMUTABLE);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_STRING_SET_STR(obj, aligned_strdup(str));
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmString, obj);
    if (str)
        SCM_ASSERT(strcmp(SCM_STRING_STR(obj), str) == 0);

    SCM_STRING_SET_LEN(obj, strlen(str));
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    SCM_STRING_SET_MUTABLE(obj);
    SCM_ASSERT(SCM_STRING_MUTATION_TYPE(obj) == SCM_STR_MUTABLE);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    SCM_STRING_SET_IMMUTABLE(obj);
    SCM_ASSERT(SCM_STRING_MUTATION_TYPE(obj) == SCM_STR_IMMUTABLE);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmString, obj);
    SCM_ASSERT(strlen(str) == SCM_STRING_LEN(obj));

    return obj;
}

ScmObj
scm_check_vector(unsigned int len)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj *vec = (ScmObj *)malloc(sizeof(ScmObj) * len);

    PRINT_SECTION("Vector");

    /* entyping */
    SCM_ENTYPE_VECTOR(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmVector, obj);

    /* unmarked state */
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_VEC(obj) == vec);

    SCM_VECTOR_SET_LEN(obj, len);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_LEN(obj) == len);

    SCM_VECTOR_VEC(obj)[0] = scm_check_int(11);
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_VEC(obj)[0]));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_VEC(obj)[0]) == 11);

    SCM_VECTOR_VEC(obj)[0] = scm_check_int(3);
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_VEC(obj)[0]));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_VEC(obj)[0]) == 3);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_VEC(obj) == vec);

    SCM_VECTOR_SET_LEN(obj, len);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_VECTOR_LEN(obj) == len);

    SCM_VECTOR_VEC(obj)[0] = scm_check_int(11);
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_VEC(obj)[0]));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_VEC(obj)[0]) == 11);

    SCM_VECTOR_VEC(obj)[0] = scm_check_int(3);
    check_type(ScmVector, obj);
    SCM_ASSERT(SCM_INTP(SCM_VECTOR_VEC(obj)[0]));
    SCM_ASSERT(SCM_INT_VALUE(SCM_VECTOR_VEC(obj)[0]) == 3);

    return obj;
}

ScmObj
scm_check_func(void *funcptr)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    PRINT_SECTION("Func");

    /* entyping */
    SCM_ENTYPE_FUNC(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmFunc, obj);

    /* unmarked state */
    SCM_FUNC_SET_CFUNC(obj, funcptr);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == funcptr);

    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == funcptr);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_FUNC_SET_CFUNC(obj, funcptr);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == funcptr);

    SCM_FUNC_SET_TYPECODE(obj, SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmFunc, obj);
    SCM_ASSERT(SCM_FUNC_TYPECODE(obj) == SCM_PROCEDURE_FIXED_TAIL_REC);
    SCM_ASSERT(SCM_FUNC_CFUNC(obj) == funcptr);

    return obj;
}

ScmObj
scm_check_port()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmCharPort *port = (ScmCharPort *)0x20;

    PRINT_SECTION("Port");

    /* entyping */
    SCM_ENTYPE_PORT(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmPort, obj);

    /* unmarked state */
    SCM_PORT_SET_FLAG(obj, SCM_PORTFLAG_INPUT);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_FLAG(obj) == SCM_PORTFLAG_INPUT);

    SCM_PORT_SET_IMPL(obj, port);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_IMPL(obj) == port);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_PORT_SET_FLAG(obj, SCM_PORTFLAG_INPUT);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_FLAG(obj) == SCM_PORTFLAG_INPUT);

    SCM_PORT_SET_IMPL(obj, port);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmPort, obj);
    SCM_ASSERT(SCM_PORT_IMPL(obj) == port);

    return obj;
}

ScmObj
scm_check_continuation(void *val)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    PRINT_SECTION("Continuation");

    /* entyping */
    SCM_ENTYPE_CONTINUATION(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmContinuation, obj);

    /* unmarked state */
    SCM_CONTINUATION_SET_OPAQUE(obj, val);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_OPAQUE(obj) == val);

    SCM_CONTINUATION_SET_TAG(obj, 10);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 10);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_CONTINUATION_SET_OPAQUE(obj, val);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_OPAQUE(obj) == val);

    SCM_CONTINUATION_SET_TAG(obj, 10);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmContinuation, obj);
    SCM_ASSERT(SCM_CONTINUATION_TAG(obj) == 10);

    return obj;
}

ScmObj
scm_check_value_packet()
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj values = scm_check_cons();

    PRINT_SECTION("ValuePacket");

    /* entyping */
    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmValuePacket, obj);

    /* unmarked state */
    SCM_VALUEPACKET_SET_VALUES(obj, values);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmValuePacket, obj);
    SCM_ASSERT(SCM_EQ(SCM_VALUEPACKET_VALUES(obj), values));
    SCM_ASSERT(SCM_CONSP(SCM_VALUEPACKET_VALUES(obj)));

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_VALUEPACKET_SET_VALUES(obj, values);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmValuePacket, obj);
    SCM_ASSERT(SCM_EQ(SCM_VALUEPACKET_VALUES(obj), values));
    SCM_ASSERT(SCM_CONSP(SCM_VALUEPACKET_VALUES(obj)));

    return obj;
}

ScmObj
scm_check_freecell()
{
    ScmObj obj  = (ScmObj)malloc(sizeof(ScmCell));
    ScmObj next = scm_check_cons();

    /* entyping */
    SCM_ENTYPE_FREECELL(obj);
    check_type(ScmFreeCell, obj);

    SCM_FREECELL_SET_NEXT(obj, next);
    check_type(ScmFreeCell, obj);
    SCM_ASSERT(SCM_EQ(SCM_FREECELL_NEXT(obj), next));
    SCM_ASSERT(SCM_CONSP(SCM_FREECELL_NEXT(obj)));
}

ScmObj
scm_check_int(int val)
{
    ScmObj obj;

    PRINT_SECTION("Int");

    SCM_ENTYPE_INT(obj);
    check_type(ScmInt, obj);

    SCM_INT_SET_VALUE(obj, val);
    check_type(ScmInt, obj);
    SCM_ASSERT(SCM_INT_VALUE(obj) == val);

    return obj;
}

ScmObj
scm_check_char(unsigned int val)
{
    ScmObj obj;
    PRINT_SECTION("Char");

    /* entyping */
    SCM_ENTYPE_CHAR(obj);
    check_type(ScmChar, obj);

    SCM_CHAR_SET_VALUE(obj, val);
    check_type(ScmChar, obj);
    SCM_ASSERT(SCM_CHAR_VALUE(obj) == val);

    return obj;
}

ScmObj
scm_check_cpointer(void *data)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    PRINT_SECTION("CPointer");

    /* entyping state */
    SCM_ENTYPE_C_POINTER(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCPointer, obj);

    /* unmarked state */
    SCM_C_POINTER_SET_VALUE(obj, data);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCPointer, obj);
    SCM_ASSERT(SCM_C_POINTER_VALUE(obj) == data);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_C_POINTER_SET_VALUE(obj, data);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmCPointer, obj);
    SCM_ASSERT(SCM_C_POINTER_VALUE(obj) == data);

    return obj;
}

ScmObj
scm_check_cfunc_pointer(ScmCFunc funcptr)
{
    ScmObj obj = (ScmObj)malloc(sizeof(ScmCell));

    PRINT_SECTION("CFuncPointer");

    /* entyping */
    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCFuncPointer, obj);

    /* unmarked state */
    SCM_C_FUNCPOINTER_SET_VALUE(obj, funcptr);
    SCM_ASSERT(SCM_UNMARKEDP(obj));
    check_type(ScmCFuncPointer, obj);
    SCM_ASSERT(SCM_C_FUNCPOINTER_VALUE(obj) == (ScmCFunc)funcptr);

    /* marked state */
    SCM_MARK(obj);
    SCM_ASSERT(SCM_MARKEDP(obj));

    SCM_C_FUNCPOINTER_SET_VALUE(obj, funcptr);
    SCM_ASSERT(SCM_MARKEDP(obj));
    check_type(ScmCFuncPointer, obj);
    SCM_ASSERT(SCM_C_FUNCPOINTER_VALUE(obj) == (ScmCFunc)funcptr);

    return obj;
}

ScmObj
scm_check_constant()
{
    PRINT_SECTION("Constant");

    SCM_ASSERT(SCM_CONSTANTP(SCM_NULL));
    SCM_ASSERT(SCM_NULLP(SCM_NULL));
    SCM_ASSERT(!SCM_INTP(SCM_NULL));
    check_type(ScmConstant, SCM_NULL);

    SCM_ASSERT(SCM_CONSTANTP(SCM_INVALID));
    SCM_ASSERT(!VALIDP(SCM_INVALID));
    check_type(ScmConstant, SCM_INVALID);

    SCM_ASSERT(SCM_CONSTANTP(SCM_UNBOUND));
    SCM_ASSERT(SCM_IMM_TAG_UNBOUNDP(SCM_UNBOUND));
    check_type(ScmConstant, SCM_UNBOUND);

    SCM_ASSERT(SCM_CONSTANTP(SCM_FALSE));
    SCM_ASSERT(SCM_FALSEP(SCM_FALSE));
    check_type(ScmConstant, SCM_FALSE);

    SCM_ASSERT(SCM_CONSTANTP(SCM_TRUE));
    SCM_ASSERT(SCM_IMM_TAG_TRUEP(SCM_TRUE));
    check_type(ScmConstant, SCM_TRUE);

    SCM_ASSERT(SCM_CONSTANTP(SCM_EOF));
    SCM_ASSERT(SCM_EOFP(SCM_EOF));
    check_type(ScmConstant, SCM_EOF);

    SCM_ASSERT(SCM_CONSTANTP(SCM_UNDEF));
    SCM_ASSERT(SCM_IMM_TAG_UNDEFP(SCM_UNDEF));
    check_type(ScmConstant, SCM_UNDEF);

    return SCM_NULL;
}

ScmObj
scm_check_ref()
{
    PRINT_SECTION("REF");

    ScmObj cons = scm_check_cons();
    ScmObj tmp  = scm_check_cons();
    ScmRef ref_car = SCM_REF_CAR(cons);
    ScmRef ref_cdr = SCM_REF_CDR(cons);

    SCM_ASSERT(SCM_EQ(cons, SCM_DEREF(SCM_REF_OFF_HEAP(cons))));
    SCM_ASSERT(SCM_EQ(SCM_CAR(cons), SCM_DEREF(ref_car)));
    SCM_ASSERT(SCM_EQ(SCM_CDR(cons), SCM_DEREF(ref_cdr)));

    SCM_SET(ref_car, tmp);
    SCM_SET(ref_cdr, tmp);
    SCM_ASSERT(SCM_EQ(cons, SCM_DEREF(SCM_REF_OFF_HEAP(cons))));
    SCM_ASSERT(SCM_EQ(SCM_CAR(cons), SCM_DEREF(ref_car)));
    SCM_ASSERT(SCM_EQ(SCM_CDR(cons), SCM_DEREF(ref_cdr)));
}

int
main(void)
{
    scm_check_int(0);
    scm_check_int(1);
    scm_check_int(-1);
    scm_check_cons();
    scm_check_symbol("aiueo");
    scm_check_symbol(NULL);
    scm_check_char(0);
    scm_check_char(255);
    scm_check_string_copying("aiueo");
/*
    scm_check_string_copying(NULL);
*/
    scm_check_closure();
    scm_check_func((void *)0x00000000);
    scm_check_func((void *)0xfffffffe);
    scm_check_func((void *)0xffffffff);
    scm_check_vector(0);
    scm_check_vector(5);
    scm_check_continuation((void *)NULL);
    scm_check_continuation((void *)0x20);
    scm_check_value_packet();
    scm_check_port();
    scm_check_cpointer((void *)0x00000000);
    scm_check_cpointer((void *)0xfffffffe);
    scm_check_cpointer((void *)0xffffffff);
    scm_check_cfunc_pointer((ScmCFunc)0x00000000);
    scm_check_cfunc_pointer((ScmCFunc)0xfffffffe);
    scm_check_cfunc_pointer((ScmCFunc)0xffffffff);
    scm_check_freecell();
    scm_check_constant();
    scm_check_ref();
}
