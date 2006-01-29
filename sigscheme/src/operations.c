/*===========================================================================
 *  FileName : operations.c
 *  About    : basic scheme procedure
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
/* FIXME: remove this for direct inclusion of operations-srfi6.c and
 * strport.c */
#include "config-asprintf.h"

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#define EQVP(a, b)   (NFALSEP(scm_p_eqvp((a), (b))))
#define EQUALP(a, b) (NFALSEP(scm_p_equalp((a), (b))))

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj list_tail(ScmObj lst, scm_int_t k);
static ScmObj map_single_arg(ScmObj proc, ScmObj args);
static ScmObj map_multiple_args(ScmObj proc, ScmObj args);

/*=======================================
  Function Implementations
=======================================*/
/*===========================================================================
  R5RS : 6.1 Equivalence predicates
===========================================================================*/
ScmObj
scm_p_eqp(ScmObj obj1, ScmObj obj2)
{
    DECLARE_FUNCTION("eq?", procedure_fixed_2);

    return MAKE_BOOL(EQ(obj1, obj2));
}

ScmObj
scm_p_eqvp(ScmObj obj1, ScmObj obj2)
{
#if (!(SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY))
    enum ScmObjType type;
#endif
    DECLARE_FUNCTION("eqv?", procedure_fixed_2);

    if (EQ(obj1, obj2))
        return SCM_TRUE;

#if (!(SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY))
    type = SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
#if !SCM_HAS_IMMEDIATE_INT_ONLY
    case ScmInt:
        return MAKE_BOOL(SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2));
#endif

#if !SCM_HAS_IMMEDIATE_CHAR_ONLY
    case ScmChar:
        return MAKE_BOOL(SCM_CHAR_VALUE(obj1) == SCM_CHAR_VALUE(obj2));
#endif

    default:
        break;
    }
#endif /* (!(SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY)) */

    return SCM_FALSE;
}

ScmObj
scm_p_equalp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type;
    ScmObj elm1, elm2, *v1, *v2;
    scm_int_t i, len;
    DECLARE_FUNCTION("equal?", procedure_fixed_2);

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    type = SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
#if !SCM_HAS_IMMEDIATE_INT_ONLY
    case ScmInt:
        return MAKE_BOOL(SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2));
#endif

#if !SCM_HAS_IMMEDIATE_CHAR_ONLY
    case ScmChar:
        return MAKE_BOOL(SCM_CHAR_VALUE(obj1) == SCM_CHAR_VALUE(obj2));
#endif

    case ScmString:
        return MAKE_BOOL(STRING_EQUALP(obj1, obj2));

    case ScmCons:
        for (; CONSP(obj1) && CONSP(obj2); obj1 = CDR(obj1), obj2 = CDR(obj2))
        {
            elm1 = CAR(obj1);
            elm2 = CAR(obj2);
            if (!EQ(elm1, elm2)
                && (SCM_TYPE(elm1) != SCM_TYPE(elm2)
                    || !EQUALP(elm1, elm2)))
                return SCM_FALSE;
        }
        /* compare last cdr */
        return (EQ(obj1, obj2)) ? SCM_TRUE : scm_p_equalp(obj1, obj2);

    case ScmVector:
        len = SCM_VECTOR_LEN(obj1);
        if (len != SCM_VECTOR_LEN(obj2))
            return SCM_FALSE;

        v1 = SCM_VECTOR_VEC(obj1);
        v2 = SCM_VECTOR_VEC(obj2);
        for (i = 0; i < len; i++) {
            elm1 = v1[i];
            elm2 = v2[i];
            if (!EQ(elm1, elm2)
                && (SCM_TYPE(elm1) != SCM_TYPE(elm2)
                    || !EQUALP(elm1, elm2)))
                return SCM_FALSE;
        }
        return SCM_TRUE;

#if SCM_USE_NONSTD_FEATURES
    case ScmCPointer:
        return MAKE_BOOL(SCM_C_POINTER_VALUE(obj1)
                         == SCM_C_POINTER_VALUE(obj2));

    case ScmCFuncPointer:
        return MAKE_BOOL(SCM_C_FUNCPOINTER_VALUE(obj1)
                         == SCM_C_FUNCPOINTER_VALUE(obj2));
#endif

    default:
        break;
    }

    return SCM_FALSE;
}

/*===================================
  R5RS : 6.3 Other data types
===================================*/
/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.1 Booleans
===========================================================================*/
ScmObj
scm_p_not(ScmObj obj)
{
    DECLARE_FUNCTION("not", procedure_fixed_1);

    return MAKE_BOOL(FALSEP(obj));
}

ScmObj
scm_p_booleanp(ScmObj obj)
{
    DECLARE_FUNCTION("boolean?", procedure_fixed_1);

    return MAKE_BOOL(EQ(obj, SCM_FALSE) || EQ(obj, SCM_TRUE));
}

/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
===========================================================================*/
ScmObj
scm_p_car(ScmObj obj)
{
    DECLARE_FUNCTION("car", procedure_fixed_1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ENSURE_CONS(obj);

    return CAR(obj);
}

ScmObj
scm_p_cdr(ScmObj obj)
{
    DECLARE_FUNCTION("cdr", procedure_fixed_1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ENSURE_CONS(obj);

    return CDR(obj);
}

ScmObj
scm_p_pairp(ScmObj obj)
{
    DECLARE_FUNCTION("pair?", procedure_fixed_1);

    return MAKE_BOOL(CONSP(obj));
}

ScmObj
scm_p_cons(ScmObj car, ScmObj cdr)
{
    DECLARE_FUNCTION("cons", procedure_fixed_2);

    return CONS(car, cdr);
}

ScmObj
scm_p_set_card(ScmObj pair, ScmObj car)
{
    DECLARE_FUNCTION("set-car!", procedure_fixed_2);

    ENSURE_CONS(pair);
    ENSURE_MUTABLE_CONS(pair);

    SET_CAR(pair, car);

#if SCM_COMPAT_SIOD
    return car;
#else
    return SCM_UNDEF;
#endif
}

ScmObj
scm_p_set_cdrd(ScmObj pair, ScmObj cdr)
{
    DECLARE_FUNCTION("set-cdr!", procedure_fixed_2);

    ENSURE_CONS(pair);
    ENSURE_MUTABLE_CONS(pair);

    SET_CDR(pair, cdr);

#if SCM_COMPAT_SIOD
    return cdr;
#else
    return SCM_UNDEF;
#endif
}

ScmObj
scm_p_caar(ScmObj lst)
{
    DECLARE_FUNCTION("caar", procedure_fixed_1);

    return scm_p_car( scm_p_car(lst) );
}

ScmObj
scm_p_cadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadr", procedure_fixed_1);

    return scm_p_car( scm_p_cdr(lst) );
}

ScmObj
scm_p_cdar(ScmObj lst)
{
    DECLARE_FUNCTION("cdar", procedure_fixed_1);

    return scm_p_cdr( scm_p_car(lst) );
}

ScmObj
scm_p_cddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddr", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr(lst) );
}

ScmObj
scm_p_caddr(ScmObj lst)
{
    DECLARE_FUNCTION("caddr", procedure_fixed_1);

    return scm_p_car( scm_p_cdr( scm_p_cdr(lst) ));
}

ScmObj
scm_p_cdddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdddr", procedure_fixed_1);

    return scm_p_cdr( scm_p_cdr( scm_p_cdr(lst) ));
}

ScmObj
scm_p_list(ScmObj args)
{
    DECLARE_FUNCTION("list", procedure_variadic_0);

    return args;
}

ScmObj
scm_p_nullp(ScmObj obj)
{
    DECLARE_FUNCTION("null?", procedure_fixed_1);

    return MAKE_BOOL(NULLP(obj));
}

ScmObj
scm_p_listp(ScmObj obj)
{
    DECLARE_FUNCTION("list?", procedure_fixed_1);

    /* fast path */
    if (NULLP(obj))
        return SCM_TRUE;
    if (!CONSP(obj))
        return SCM_FALSE;

    return MAKE_BOOL(PROPER_LISTP(obj));
}

#define TERMINATOR_LEN 1

/* scm_length() for non-circular list */
scm_int_t
scm_finite_length(ScmObj lst)
{
    scm_int_t len;

    for (len = 0; CONSP(lst); lst = CDR(lst))
        len++;

    if (NULLP(lst))
        return len;
    else
        return SCM_LISTLEN_ENCODE_DOTTED(len + TERMINATOR_LEN);
}

/*
 * Notice
 *
 * This function is ported from Gauche, by Shiro Kawai(shiro@acm.org)
 */
/* FIXME: Insert its copyright and license into this file properly */
/*
 * ChangeLog:
 *
 * 2006-01-05 YamaKen  Return dot list length and circular indication.
 *
 */
/* Returns -1 as one length improper list for non-list obj. */
scm_int_t
scm_length(ScmObj lst)
{
    ScmObj slow;
    scm_int_t proper_len;

    for (proper_len = 0, slow = lst;;) {
        if (NULLP(lst)) break;
        if (!CONSP(lst))
            return SCM_LISTLEN_ENCODE_DOTTED(proper_len + TERMINATOR_LEN);
        if (proper_len != 0 && lst == slow)
            return SCM_LISTLEN_ENCODE_CIRCULAR(proper_len);

        lst = CDR(lst);
        proper_len++;
        if (NULLP(lst)) break;
        if (!CONSP(lst))
            return SCM_LISTLEN_ENCODE_DOTTED(proper_len + TERMINATOR_LEN);
        if (lst == slow)
            return SCM_LISTLEN_ENCODE_CIRCULAR(proper_len);

        lst = CDR(lst);
        slow = CDR(slow);
        proper_len++;
    }

    return proper_len;
}

#undef TERMINATOR_LEN

ScmObj
scm_p_length(ScmObj obj)
{
    scm_int_t len;
    DECLARE_FUNCTION("length", procedure_fixed_1);

    len = scm_length(obj);
    if (!SCM_LISTLEN_PROPERP(len))
        ERR_OBJ("proper list required but got", obj);

    return MAKE_INT(len);
}

ScmObj
scm_p_append(ScmObj args)
{
    ScmQueue q;
    ScmObj lst, elm, res;
    DECLARE_FUNCTION("append", procedure_variadic_0);

    if (NULLP(args))
        return SCM_NULL;

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    /* duplicate and merge all but the last argument */
    FOR_EACH_BUTLAST (lst, args) {
        FOR_EACH (elm, lst)
            SCM_QUEUE_ADD(q, elm);
        ENSURE_PROPER_LIST_TERMINATION(lst, args);
    }
    /* append the last argument */
    SCM_QUEUE_SLOPPY_APPEND(q, lst);

    return res;
}

ScmObj
scm_p_reverse(ScmObj lst)
{
    ScmObj ret, elm;
    DECLARE_FUNCTION("reverse", procedure_fixed_1);

    ret = SCM_NULL;
    FOR_EACH (elm, lst)
        ret = CONS(elm, ret);

    return ret;
}

static ScmObj
list_tail(ScmObj lst, scm_int_t k)
{
    while (k--) {
        if (!CONSP(lst))
            return SCM_INVALID;
        lst = CDR(lst);
    }

    return lst;
}

ScmObj
scm_p_list_tail(ScmObj lst, ScmObj k)
{
    ScmObj ret;
    DECLARE_FUNCTION("list-tail", procedure_fixed_2);

    ENSURE_INT(k);

    ret = list_tail(lst, SCM_INT_VALUE(k));
    if (!VALIDP(ret))
        ERR_OBJ("out of range or invalid list", LIST_2(lst, k));

    return ret;
}

ScmObj
scm_p_list_ref(ScmObj lst, ScmObj k)
{
    ScmObj tail;
    DECLARE_FUNCTION("list-ref", procedure_fixed_2);

    ENSURE_INT(k);

    tail = list_tail(lst, SCM_INT_VALUE(k));
    if (!VALIDP(tail) || NULLP(tail))
        ERR_OBJ("out of range or invalid list", LIST_2(lst, k));

    return CAR(tail);
}

#define MEMBER_BODY(obj, lst, cmp)                                           \
    do {                                                                     \
        for (; CONSP(lst); lst = CDR(lst))                                   \
            if (cmp(obj, CAR(lst)))                                          \
                return lst;                                                  \
        CHECK_PROPER_LIST_TERMINATION(lst, lst);                             \
        return SCM_FALSE;                                                    \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_memq(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memq", procedure_fixed_2);

    MEMBER_BODY(obj, lst, EQ);
}

ScmObj
scm_p_memv(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memv", procedure_fixed_2);

#if (SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY)
    MEMBER_BODY(obj, lst, EQ);
#else
    MEMBER_BODY(obj, lst, EQVP);
#endif
}

ScmObj
scm_p_member(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("member", procedure_fixed_2);

    MEMBER_BODY(obj, lst, EQUALP);
}

#undef MEMBER_BODY

#define ASSOC_BODY(obj, alist, cmp)                                          \
    do {                                                                     \
        ScmObj pair, key;                                                    \
                                                                             \
        FOR_EACH (pair, alist) {                                             \
            ENSURE_CONS(pair);                                               \
            key = CAR(pair);                                                 \
            if (cmp(key, obj))                                               \
                return pair;                                                 \
        }                                                                    \
        CHECK_PROPER_LIST_TERMINATION(alist, alist);                         \
        return SCM_FALSE;                                                    \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_assq(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assq", procedure_fixed_2);

    ASSOC_BODY(obj, alist, EQ);
}

ScmObj
scm_p_assv(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assv", procedure_fixed_2);

#if (SCM_HAS_IMMEDIATE_NUMBER_ONLY && SCM_HAS_IMMEDIATE_CHAR_ONLY)
    ASSOC_BODY(obj, alist, EQ);
#else
    ASSOC_BODY(obj, alist, EQVP);
#endif
}

ScmObj
scm_p_assoc(ScmObj obj, ScmObj alist)
{
    DECLARE_FUNCTION("assoc", procedure_fixed_2);

    ASSOC_BODY(obj, alist, EQUALP);
}

#undef ASSOC_BODY

/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.3 Symbols
===========================================================================*/
ScmObj
scm_p_symbolp(ScmObj obj)
{
    DECLARE_FUNCTION("symbol?", procedure_fixed_1);

    return MAKE_BOOL(SYMBOLP(obj));
}

ScmObj
scm_p_symbol2string(ScmObj sym)
{
    DECLARE_FUNCTION("symbol->string", procedure_fixed_1);

    ENSURE_SYMBOL(sym);

    return CONST_STRING(SCM_SYMBOL_NAME(sym));
}

ScmObj
scm_p_string2symbol(ScmObj str)
{
    DECLARE_FUNCTION("string->symbol", procedure_fixed_1);

    ENSURE_STRING(str);

    return scm_intern(SCM_STRING_STR(str));
}

/*===========================================================================
  R5RS : 6.3 Other data types : 6.3.6 Vectors
===========================================================================*/
ScmObj
scm_p_vectorp(ScmObj obj)
{
    DECLARE_FUNCTION("vector?", procedure_fixed_1);

    return MAKE_BOOL(VECTORP(obj));
}

ScmObj
scm_p_make_vector(ScmObj scm_len, ScmObj args)
{
    ScmObj *vec, filler;
    scm_int_t len, i;
    DECLARE_FUNCTION("make-vector", procedure_variadic_1);

    ENSURE_INT(scm_len);

    len = SCM_INT_VALUE(scm_len);
    if (len < 0)
        ERR_OBJ("length must be a positive integer", scm_len);

    vec = scm_malloc(sizeof(ScmObj) * len);
    if (NULLP(args)) {
        filler = SCM_UNDEF;
    } else {
        filler = POP(args);
        ASSERT_NO_MORE_ARG(args);
    }
    for (i = 0; i < len; i++)
        vec[i] = filler;

    return MAKE_VECTOR(vec, len);
}

ScmObj
scm_p_vector(ScmObj args)
{
    DECLARE_FUNCTION("vector", procedure_variadic_0);

    return scm_p_list2vector(args);
}

ScmObj
scm_p_vector_length(ScmObj vec)
{
    DECLARE_FUNCTION("vector-length", procedure_fixed_1);

    ENSURE_VECTOR(vec);

    return MAKE_INT(SCM_VECTOR_LEN(vec));
}

ScmObj
scm_p_vector_ref(ScmObj vec, ScmObj scm_k)
{
    scm_int_t k;
    DECLARE_FUNCTION("vector-ref", procedure_fixed_2);

    ENSURE_VECTOR(vec);
    ENSURE_INT(scm_k);

    k = SCM_INT_VALUE(scm_k);

    if (!SCM_VECTOR_VALID_INDEXP(vec, k))
        ERR_OBJ("index out of range", scm_k);

    return SCM_VECTOR_VEC(vec)[k];
}

ScmObj
scm_p_vector_setd(ScmObj vec, ScmObj scm_k, ScmObj obj)
{
    scm_int_t k;
    DECLARE_FUNCTION("vector-set!", procedure_fixed_3);

    ENSURE_VECTOR(vec);
#if SCM_CONST_VECTOR_LITERAL
    ENSURE_MUTABLE_VECTOR(vec);
#endif
    ENSURE_INT(scm_k);

    k = SCM_INT_VALUE(scm_k);

    if (!SCM_VECTOR_VALID_INDEXP(vec, k))
        ERR_OBJ("index out of range", scm_k);

    SCM_VECTOR_VEC(vec)[k] = obj;

    return SCM_UNDEF;
}

ScmObj
scm_p_vector2list(ScmObj vec)
{
    ScmQueue q;
    ScmObj res, *v;
    scm_int_t len, i;
    DECLARE_FUNCTION("vector->list", procedure_fixed_1);

    ENSURE_VECTOR(vec);

    v   = SCM_VECTOR_VEC(vec);
    len = SCM_VECTOR_LEN(vec);

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    for (i = 0; i < len; i++)
        SCM_QUEUE_ADD(q, v[i]);

    return res;
}

ScmObj
scm_p_list2vector(ScmObj lst)
{
    ScmObj *vec;
    scm_int_t len, i;
    DECLARE_FUNCTION("list->vector", procedure_fixed_1);

    len = scm_length(lst);
    if (!SCM_LISTLEN_PROPERP(len))
        ERR_OBJ("proper list required but got", lst);

    vec = scm_malloc(sizeof(ScmObj) * len);
    for (i = 0; i < len; i++)
        vec[i] = POP(lst);

    return MAKE_VECTOR(vec, len);
}

ScmObj
scm_p_vector_filld(ScmObj vec, ScmObj fill)
{
    ScmObj *v;
    scm_int_t len, i;
    DECLARE_FUNCTION("vector-fill!", procedure_fixed_2);

    ENSURE_VECTOR(vec);
#if SCM_CONST_VECTOR_LITERAL
    ENSURE_MUTABLE_VECTOR(vec);
#endif

    v   = SCM_VECTOR_VEC(vec);
    len = SCM_VECTOR_LEN(vec);
    for (i = 0; i < len; i++)
        v[i] = fill;

    return vec;
}

/*=======================================
  R5RS : 6.4 Control Features
=======================================*/
ScmObj
scm_p_procedurep(ScmObj obj)
{
    DECLARE_FUNCTION("procedure?", procedure_fixed_1);

    return MAKE_BOOL(PROCEDUREP(obj));
}

ScmObj
scm_p_map(ScmObj proc, ScmObj args)
{
    DECLARE_FUNCTION("map", procedure_variadic_1);

    if (NULLP(args))
        ERR("map: wrong number of arguments");

    /* fast path for single arg case */
    if (NULLP(CDR(args)))
        return map_single_arg(proc, CAR(args));

    /* multiple args case */
    return map_multiple_args(proc, args);
}

static ScmObj
map_single_arg(ScmObj proc, ScmObj lst)
{
    ScmQueue q;
    ScmObj elm, res;
    DECLARE_INTERNAL_FUNCTION("map");

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    FOR_EACH (elm, lst) {
        elm = scm_call(proc, LIST_1(elm));
        SCM_QUEUE_ADD(q, elm);
    }

    return res;
}

static ScmObj
map_multiple_args(ScmObj proc, ScmObj args)
{
    ScmQueue resq, argq;
    ScmObj res, elm, map_args, rest_args, arg;
    DECLARE_INTERNAL_FUNCTION("map");

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(resq, res);
    for (;;) {
        /* slice args */
        map_args = SCM_NULL;
        SCM_QUEUE_POINT_TO(argq, map_args);
        for (rest_args = args; CONSP(rest_args); rest_args = CDR(rest_args)) {
            arg = CAR(rest_args);
            if (CONSP(arg))
                SCM_QUEUE_ADD(argq, CAR(arg));
            else if (NULLP(arg))
                return res;
            else
                ERR_OBJ("invalid argument", arg);
            /* pop destructively */
            SET_CAR(rest_args, CDR(arg));
        }

        elm = scm_call(proc, map_args);
        SCM_QUEUE_ADD(resq, elm);
    }
}

ScmObj
scm_p_for_each(ScmObj proc, ScmObj args)
{
    DECLARE_FUNCTION("for-each", procedure_variadic_1);

    scm_p_map(proc, args);

    return SCM_UNDEF;
}

ScmObj
scm_p_force(ScmObj closure)
{
    DECLARE_FUNCTION("force", procedure_fixed_1);

    ENSURE_CLOSURE(closure);

    return scm_call(closure, SCM_NULL);
}

ScmObj
scm_p_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state)
{
    DECLARE_FUNCTION("call-with-current-continuation",
                     procedure_fixed_tailrec_1);

    ENSURE_PROCEDURE(proc);

    return scm_call_with_current_continuation(proc, eval_state);
}

ScmObj
scm_p_values(ScmObj args)
{
    DECLARE_FUNCTION("values", procedure_variadic_0);

    /* Values with one arg must return something that fits an ordinary
     * continuation. */
    if (LIST_1_P(args))
        return CAR(args);

    /* Otherwise, we'll return the values in a packet. */
    return SCM_MAKE_VALUEPACKET(args);
}

ScmObj
scm_p_call_with_values(ScmObj producer, ScmObj consumer,
                       ScmEvalState *eval_state)
{
    ScmObj vals;
    DECLARE_FUNCTION("call-with-values", procedure_fixed_tailrec_2);

    ENSURE_PROCEDURE(producer);
    ENSURE_PROCEDURE(consumer);

    vals = scm_call(producer, SCM_NULL);

    if (!VALUEPACKETP(vals)) {
        /* got back a single value */
        vals = LIST_1(vals);
    } else {
        /* extract */
        vals = SCM_VALUEPACKET_VALUES(vals);
    }

    return scm_tailcall(consumer, vals, eval_state);
}

ScmObj
scm_p_dynamic_wind(ScmObj before, ScmObj thunk, ScmObj after)
{
    DECLARE_FUNCTION("dynamic-wind", procedure_fixed_3);

    ENSURE_PROCEDURE(before);
    ENSURE_PROCEDURE(thunk);
    ENSURE_PROCEDURE(after);

    return scm_dynamic_wind(before, thunk, after);
}

#if SCM_USE_DEEP_CADRS
#include "operations-r5rs-deepcadrs.c"
#endif
#if SCM_USE_NONSTD_FEATURES
#include "operations-nonstd.c"
#endif
#if SCM_USE_SRFI1
#include "operations-srfi1.c"
#endif
#if SCM_USE_SRFI2
#include "operations-srfi2.c"
#endif
#if SCM_USE_SRFI6
#include "operations-srfi6.c"
#endif
#if SCM_USE_SRFI8
#include "operations-srfi8.c"
#endif
#if SCM_USE_SRFI23
#include "operations-srfi23.c"
#endif
#if SCM_USE_SRFI34
#include "operations-srfi34.c"
#endif
#if SCM_USE_SRFI38
#include "operations-srfi38.c"
#endif
#if SCM_USE_SRFI60
#include "operations-srfi60.c"
#endif
#if SCM_COMPAT_SIOD
#include "operations-siod.c"
#endif
