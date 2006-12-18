/*===========================================================================
 *  Filename : test-storage.c
 *  About    : scheme object representation and accessor test
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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#if (!SCM_USE_CHAR || !SCM_USE_VECTOR)
#define TST_EXCLUDE_THIS
#endif

#ifndef EXPAND
#include "sscm-test.h"
#include "sigschemeinternal.h"
#endif

#ifndef TST_EXCLUDE_THIS

#include "utils.c"

#define STR(s) #s
#define TST TST_COND

typedef ScmObj OBJ;
typedef void *PTR;
typedef char *STR;
typedef scm_int_t INT;

#define TST1(o, typ, field, ftyp, fval, context)                        \
    do {                                                                \
        ftyp _fv = (fval);                                              \
        TST_COND(SCM_##typ##P(o), STR(SCM_##typ##P()) " " context);     \
        TST_ASSERT(!TST_FAILED);                                        \
        TST_EQ_##ftyp(_fv, SCM_##typ##_##field(o),                      \
                      STR(SCM_##typ##_##field()) " " context);          \
    } while (0)

#define TST2(o, typ, f1, f1typ, f1val, f2, f2typ, f2val, context)       \
    do {                                                                \
        f1typ _f1v = (f1val);                                           \
        f2typ _f2v = (f2val);                                           \
        TST1(o, typ, f1, f1typ, _f1v, context);                         \
        TST1(o, typ, f2, f2typ, _f2v, context);                         \
    } while (0)

#define TST_INIT2(o, typ, f1, f1typ, f1val, f2, f2typ, f2val)   \
    do {                                                        \
        f1typ f1v = (f1val);                                    \
        f2typ f2v = (f2val);                                    \
        (o) = SCM_MAKE_##typ(f1v, f2v);                         \
        TST2(o, typ, f1, f1typ, f1v,                            \
                     f2, f2typ, f2v,                            \
             "on fresh " #typ);                                 \
    } while (0)

#define TST_INIT1(o, typ, field, ftyp, fval)                    \
    do {                                                        \
        ftyp fv = (fval);                                       \
        (o) = SCM_MAKE_##typ(fv);                               \
        TST1(o, typ, field, ftyp, fv, "on fresh " #typ);        \
    } while (0)

#define TST_SET1(o, typ, field, ftyp, fval)                             \
    do {                                                                \
        ftyp fnew = (fval);                                             \
        SCM_##typ##_SET_##field((o), fnew);                             \
        TST1(o, typ, field, ftyp, fnew, "after setting " #field);       \
    } while (0)

#define TST_SET2(o, typ, f1, f1typ, f1val, f2, f2typ, f2val)    \
    do {                                                        \
        f2typ f2orig = SCM_##typ##_##f2(o);                     \
        f1typ f1new  = (f1val);                                 \
        f2typ f2new  = (f2val);                                 \
        SCM_##typ##_SET_##f1((o), f1new);                       \
        TST2(o, typ, f1, f1typ, f1new,                          \
                     f2, f2typ, f2orig,                         \
             "after setting " #f1);                             \
        SCM_##typ##_SET_##f2((o), f2new);                       \
        TST2(o, typ, f1, f1typ, f1new,                          \
                     f2, f2typ, f2new,                          \
             "after setting " #f2);                             \
    } while (0)

#define TST_EXPR(expr) TST_COND((expr), #expr)


TST_CASE("eq? and constants")
{
    ScmObj obj;
    TST_EQ(SCM_NULL, SCM_NULL, "(eq? '() '())");
    TST_ASSERT(!TST_FAILED);
    TST_EXPR(NULLP(SCM_NULL));
    TST_EXPR(!VALIDP(SCM_INVALID));
    TST_EXPR(VALIDP(SCM_NULL));
    TST_EXPR(VALIDP(SCM_FALSE));
    TST_EXPR(FALSEP(SCM_FALSE));
    TST_EXPR(!FALSEP(SCM_TRUE));
#if SCM_COMPAT_SIOD_BUGS
    TST_EXPR(NULLP(SCM_FALSE));
    TST_EXPR(FALSEP(SCM_NULL));
#else
    TST_EXPR(!FALSEP(SCM_NULL));
#endif
    TST_EXPR(!FALSEP(SCM_EOF));
    TST_EXPR(EOFP(SCM_EOF));

    obj = LIST_1(SCM_FALSE);
    TST_NEQ(obj, LIST_1(SCM_FALSE), "equal? but not eq?");
    TST_EQ(obj, obj, "eq?");
}

TST_CASE("pair")
{
    ScmObj obj;

#define CONS_TST(tst, kar, kdr)                 \
        tst(obj, CONS,                          \
            CAR, OBJ, kar,                      \
            CDR, OBJ, kdr)

/* These interfere with token generation. */
#undef CONS
#undef CAR
#undef CDR

    CONS_TST(TST_INIT2, SCM_EOF, SCM_NULL);
    CONS_TST(TST_SET2, SCM_NULL, SCM_TRUE);

#define CONS SCM_CONS
#define CAR SCM_CAR
#define CDR SCM_CDR
}


TST_CASE("closure")
{
    ScmObj obj;
    ScmObj exp, env;

    exp = LIST_1(SCM_SYM_QUOTE);
    env = SCM_NULL_ENV;

#define CLOSURE_TST(tst, xp, nv)                \
    tst(obj, CLOSURE,                           \
        EXP, OBJ, xp,                           \
        ENV, OBJ, nv)

    CLOSURE_TST(TST_INIT2, exp, env);
    CLOSURE_TST(TST_SET2, SCM_NULL, CONS(SCM_NULL, SCM_NULL));
}


TST_CASE("int")
{
    ScmObj obj;
    /* for suppressing compiler warning on intentional overflowed/underflowed
     * value */
    volatile scm_int_t scm_int_min = SCM_INT_MIN;
    volatile scm_int_t scm_int_max = SCM_INT_MAX;

#define INT_TST(tst, val)                       \
    tst(obj, INT,                               \
        VALUE, INT, val)

    INT_TST(TST_INIT1, 1);
    INT_TST(TST_INIT1, SCM_INT_MIN);
    INT_TST(TST_INIT1, SCM_INT_MAX);
    obj = SCM_MAKE_INT(scm_int_min - 1);
    TST_COND(INTP(obj), "INTP() on underflowed int");
    obj = SCM_MAKE_INT(scm_int_max + 1);
    TST_COND(INTP(obj), "INTP() on overflowed int");
}

TST_CASE("char")
{
    ScmObj obj;
    /* for suppressing compiler warning on intentional overflowed/underflowed
     * value */
    volatile scm_ichar_t scm_char_min = SCM_CHAR_MIN;
    volatile scm_ichar_t scm_char_max = SCM_CHAR_MAX;

#define CHAR_TST(tst, val)                      \
    tst(obj, CHAR,                              \
        VALUE, INT, val)

    CHAR_TST(TST_INIT1, 0);
    CHAR_TST(TST_INIT1, SCM_CHAR_MIN);
    CHAR_TST(TST_INIT1, SCM_CHAR_MAX);
    obj = SCM_MAKE_CHAR(scm_char_min - 1);
    TST_COND(CHARP(obj), "CHARP() on underflowed char");
    obj = SCM_MAKE_CHAR(scm_char_max + 1);
    TST_COND(CHARP(obj), "CHARP() on overflowed char");
}


TST_CASE("symbol")
{
    ScmObj obj;
    char *p = "abcdefghijklmnopqrstuv";
    p = (char*)(((intptr_t)p + 7)& (-8));

#define SYMBOL_TST(tst, nam, val)               \
    tst(obj, SYMBOL,                            \
        NAME, PTR, nam,                         \
        VCELL, OBJ, val)

    SYMBOL_TST(TST_INIT2, p, LIST_1(SCM_NULL));
    SYMBOL_TST(TST_SET2, NULL, SCM_NULL);
}


TST_CASE("string")
{
    ScmObj obj;
    char buf[] = "abcdefghijklmnopqrstuv", *p;
    size_t len = sizeof (buf) / sizeof (*buf);

#define STRING_TST(tst, str, len)               \
    tst(obj, STRING,                            \
        STR, PTR, str,                          \
        LEN, INT, len)

    STRING_TST(TST_INIT2, aligned_dup(buf, sizeof(buf)), len);

#if SCM_HAS_IMMUTABLE_STRING
    TST_COND(SCM_STRING_MUTABLEP(obj), "MAKE_STRING -> mutable?");
    obj = SCM_MAKE_IMMUTABLE_STRING_COPYING(buf, len);
    TST_COND(!SCM_STRING_MUTABLEP(obj),
             "MAKE_IMMUTABLE_STRING -> immutable?");
    SCM_STRING_SET_MUTABLE(obj);
    TST_COND(SCM_STRING_MUTABLEP(obj), "STRING_SET_MUTABLE -> mutable?");
    SCM_STRING_SET_IMMUTABLE(obj);
    TST_COND(!SCM_STRING_MUTABLEP(obj), "STRING_SET_IMMUTABLE -> immutable?");
    SCM_STRING_SET_MUTABLE(obj);
#endif /* have immutable string */

    p = SCM_STRING_STR(obj);
    STRING_TST(TST_SET2, aligned_dup(buf, sizeof(buf)), len - 8);
    free(p);
#if SCM_HAS_IMMUTABLE_STRING
    TST_COND(SCM_STRING_MUTABLEP(obj), "string-mutable? after set");
#endif
}

TST_CASE(vector, "vector")
{
    ScmObj obj;
    ScmObj buf[4];
    ScmObj *p;
    size_t len;
    len = sizeof (buf) / sizeof (*buf);

    buf[0] = SCM_NULL;
    buf[1] = SCM_TRUE;
    buf[2] = SCM_MAKE_INT(0);
    buf[3] = LIST_1(SCM_FALSE);

#define VECTOR_TST(tst, vec, len)               \
    tst(obj, VECTOR,                            \
        VEC, PTR, vec,                          \
        LEN, INT, len)

    VECTOR_TST(TST_INIT2, aligned_dup(buf, sizeof(buf)), len);

#if SCM_HAS_IMMUTABLE_VECTOR
    TST_COND(SCM_VECTOR_MUTABLEP(obj), "MAKE_VECTOR -> mutable?");
    obj = SCM_MAKE_IMMUTABLE_VECTOR(NULL, len);
    TST_COND(!SCM_VECTOR_MUTABLEP(obj),
             "MAKE_IMMUTABLE_VECTOR -> immutable?");
    SCM_VECTOR_SET_MUTABLE(obj);
    TST_COND(SCM_VECTOR_MUTABLEP(obj), "VECTOR_SET_MUTABLE -> mutable?");
    SCM_VECTOR_SET_IMMUTABLE(obj);
    TST_COND(!SCM_VECTOR_MUTABLEP(obj), "VECTOR_SET_IMMUTABLE -> immutable?");
    SCM_VECTOR_SET_MUTABLE(obj);
#endif /* have immutable string */

    p = SCM_VECTOR_VEC(obj);
    VECTOR_TST(TST_SET2, aligned_dup(buf, sizeof(buf)), len - 8);
    free(p);
#if SCM_HAS_IMMUTABLE_VECTOR
    TST_COND(SCM_VECTOR_MUTABLEP(obj), "vector-mutable? after set");
#endif
}

TST_CASE("values")
{
#if !SCM_USE_VALUECONS
    ScmObj obj;

#define VALS_TST(tst, vals)                      \
    tst(obj, VALUEPACKET,                        \
        VALUES, OBJ, vals)

    VALS_TST(TST_INIT1, LIST_2(SCM_TRUE, SCM_FALSE));
    VALS_TST(TST_SET1, SCM_NULL);
#endif
}

TST_CASE("func")
{
    ScmObj obj;

#define FUNC_TST(tst, typ, fun)                 \
    tst(obj, FUNC,                              \
        TYPECODE, INT, typ,                     \
        CFUNC, FPTR, fun)

    typedef ScmFuncType FPTR;
    FUNC_TST(TST_INIT2, SCM_SYNTAX_VARIADIC_1, (ScmFuncType)0xdeadbeef);
    FUNC_TST(TST_SET2, SCM_PROCEDURE_FIXED_4, (ScmFuncType)0);
#if (SIZEOF_SCMOBJ == SIZEOF_INT64_T)
    FUNC_TST(TST_INIT2, SCM_SYNTAX_VARIADIC_1, (ScmFuncType)0xdeadbeeffeed);
#endif
}

TST_CASE(port, "port")
{
    /* TODO; currently passes but crashes at the end upon GC.  Also
     * reliant on the implementation details of SCM_MAKE_PORT(). */
#if 0
    ScmObj obj;
    enum ScmPortFlag f;
#define PORT_TST(tst, impl, flag)               \
    tst(obj, PORT,                              \
        IMPL, PTR, impl,                        \
        FLAG, INT, flag)
    f = SCM_PORTFLAG_OUTPUT | SCM_PORTFLAG_LIVE_OUTPUT; /* FIXME */
    PORT_TST(TST_INIT2, NULL, f);
    f = SCM_PORTFLAG_INPUT | SCM_PORTFLAG_LIVE_INPUT; /* FIXME */
    PORT_TST(TST_SET2, NULL, f);
#endif
}

TST_CASE("continuation")
{
    ScmObj obj;

#define CONT_TST(tst, op, tag)                  \
    tst(obj, CONTINUATION,                      \
        OPAQUE, PTR, op,                        \
        TAG, INT, tag)
    obj = SCM_MAKE_CONTINUATION();
    TST_COND(SCM_CONTINUATIONP(obj), "CONTINUATIONP() on fresh CONTINUATION");
    CONT_TST(TST_SET2, (void*)0x0deadbee, 0xf00f);
    CONT_TST(TST_SET2, INVALID_CONTINUATION_OPAQUE, 0);
#if (SIZEOF_SCMOBJ == SIZEOF_INT64_T)
    CONT_TST(TST_SET2, (void*)0x0deadbeefee, 0xf00f);
#endif
}

#if SCM_USE_SSCM_EXTENSIONS
TST_CASE("C ptr")
{
    ScmObj obj;

#define CPTR_TST(tst, p)                        \
    tst(obj, C_POINTER,                         \
        VALUE, PTR, p)

    CPTR_TST(TST_INIT1, (void*)0xdeadbeef);
    CPTR_TST(TST_SET1, (void*)0xbaddeed);
#if (SIZEOF_SCMOBJ == SIZEOF_INT64_T)
    CPTR_TST(TST_INIT1, (void*)0xdeadbeeffeedee);
    CPTR_TST(TST_SET1, (void*)0xbaddeedbed);
#endif
}

TST_CASE("C func ptr")
{
    ScmObj obj;

#define CFPTR_TST(tst, p)                       \
    tst(obj, C_FUNCPOINTER,                     \
        VALUE, FPTR, p)

    typedef ScmCFunc FPTR;
    CFPTR_TST(TST_INIT1, (ScmCFunc)0xdeadbeef);
    CFPTR_TST(TST_SET1, (ScmCFunc)0xbaddeed);
#if (SIZEOF_SCMOBJ == SIZEOF_INT64_T)
    /* both MSB and LSB are set */
    CFPTR_TST(TST_INIT1, (ScmCFunc)0xadeadbeeffedbeef);
    CFPTR_TST(TST_SET1, (ScmCFunc)0xbaddeedbeddad);
#endif
}
#endif /* use sscm extension mechanism */

#if SCM_USE_HYGIENIC_MACRO
#if SCM_USE_UNHYGIENIC_MACRO
#error "No test implemented."
#endif

TST_CASE("subpat")
{
    ScmObj obj;
#define SUBPAT_TST(tst, pat, meta)              \
    tst(obj, SUBPAT,                            \
        OBJ, OBJ, pat,                          \
        META, INT, meta)

    SUBPAT_TST(TST_INIT2, LIST_1(SCM_NULL), -1);
    SUBPAT_TST(TST_SET2, SCM_NULL, 5);
}

TST_CASE("far symbol")
{
    ScmObj obj;
#if SCM_USE_UNHYGIENIC_MACRO
#error "Packed env handling must be revised."
#endif
#define FARSYMBOL_TST(tst, sym, env)            \
    tst(obj, FARSYMBOL,                         \
        SYM, OBJ, sym,                          \
        ENV, INT, env)          /* ScmPackedEnv == scm_int_t */
    ScmPackedEnv null;
    ScmObj env;

    null = scm_pack_env(SCM_NULL_ENV);
    env = scm_extend_environment(SCM_SYM_QUOTE, SCM_NULL, SCM_INTERACTION_ENV);

    FARSYMBOL_TST(TST_INIT2, SCM_SYM_QUOTE, null);
    FARSYMBOL_TST(TST_SET2, SCM_MAKE_FARSYMBOL(SCM_SYM_QUOTE, null),
                  scm_pack_env(env));
}

TST_CASE(hmacro, "hmacro")
{
    ScmObj obj;
    ScmObj env, rules;

#if SCM_USE_UNHYGIENIC_MACRO
#define TST_EQ_PENV TST_EQ_OBJ
#else
#define TST_EQ_PENV TST_EQ_INT
#endif
    typedef ScmPackedEnv PENV;

#define HMACRO_TST(tst, rules, env, context)    \
    tst(obj, HMACRO,                            \
        RULES, OBJ, rules,                      \
        ENV, PENV, env,                         \
        context)

    obj = SCM_MAKE_HMACRO(SCM_NULL, SCM_INTERACTION_ENV);
    HMACRO_TST(TST2, SCM_NULL, scm_pack_env(SCM_INTERACTION_ENV),
               "on fresh HMACRO");

    rules = LIST_1(SCM_NULL);
    SCM_HMACRO_SET_RULES(obj, rules);
    HMACRO_TST(TST2, rules, scm_pack_env(SCM_INTERACTION_ENV),
               "after SET_RULES()");

    env = scm_extend_environment(SCM_SYM_QUOTE, SCM_NULL,
                                 SCM_NULL_ENV);
    SCM_HMACRO_SET_ENV(obj, scm_pack_env(env));
    HMACRO_TST(TST2, rules, scm_pack_env(env),
               "after SET_ENV");
}

#endif /* use hygienic macro */
#endif /* !TST_EXCLUDE_THIS */
