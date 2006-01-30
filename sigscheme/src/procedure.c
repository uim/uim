/*===========================================================================
 *  FileName : procedure.c
 *  About    : Miscellaneous R5RS procedures
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
/* FIXME: remove this for direct inclusion of module-srfi6.c and
 * strport.c */
#include "config-asprintf.h"

/*=======================================
  System Include
=======================================*/

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

/*=======================================
  Variable Declarations
=======================================*/
/* canonical internal encoding for identifiers */
ScmCharCodec *scm_identifier_codec;

/*=======================================
  File Local Function Declarations
=======================================*/
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
        ERR("wrong number of arguments");

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
    ScmObj elm, ret;
    DECLARE_INTERNAL_FUNCTION("map");

    ret = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, ret);
    FOR_EACH (elm, lst) {
        elm = scm_call(proc, LIST_1(elm));
        SCM_QUEUE_ADD(q, elm);
    }

    return ret;
}

static ScmObj
map_multiple_args(ScmObj proc, ScmObj args)
{
    ScmQueue retq, argq;
    ScmObj ret, elm, map_args, rest_args, arg;
    DECLARE_INTERNAL_FUNCTION("map");

    ret = SCM_NULL;
    SCM_QUEUE_POINT_TO(retq, ret);
    for (;;) {
        /* slice args */
        map_args = SCM_NULL;
        SCM_QUEUE_POINT_TO(argq, map_args);
        for (rest_args = args; CONSP(rest_args); rest_args = CDR(rest_args)) {
            arg = CAR(rest_args);
            if (CONSP(arg))
                SCM_QUEUE_ADD(argq, CAR(arg));
            else if (NULLP(arg))
                return ret;
            else
                ERR_OBJ("invalid argument", arg);
            /* pop destructively */
            SET_CAR(rest_args, CDR(arg));
        }

        elm = scm_call(proc, map_args);
        SCM_QUEUE_ADD(retq, elm);
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
#include "module-r5rs-deepcadrs.c"
#endif
#if SCM_USE_NONSTD_FEATURES
#include "module-nonstd.c"
#endif
#if SCM_USE_SRFI1
#include "module-srfi1.c"
#endif
#if SCM_USE_SRFI2
#include "module-srfi2.c"
#endif
#if SCM_USE_SRFI6
#include "module-srfi6.c"
#endif
#if SCM_USE_SRFI8
#include "module-srfi8.c"
#endif
#if SCM_USE_SRFI23
#include "module-srfi23.c"
#endif
#if SCM_USE_SRFI34
#include "module-srfi34.c"
#endif
#if SCM_USE_SRFI38
#include "module-srfi38.c"
#endif
#if SCM_USE_SRFI60
#include "module-srfi60.c"
#endif
#if SCM_COMPAT_SIOD
#include "module-siod.c"
#endif
