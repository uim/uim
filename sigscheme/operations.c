/*===========================================================================
 *  FileName : operations.c
 *  About    : basic scheme procedure
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
/*=======================================
  System Include
=======================================*/
#include <string.h>
#include <stdlib.h>
#include <limits.h>

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

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj ScmOp_listtail_internal(ScmObj obj, int k);

static ScmObj map_single_arg(ScmObj proc, ScmObj args);
static ScmObj map_multiple_args(ScmObj proc, ScmObj args);

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  R5RS : 6.1 Equivalence predicates
==============================================================================*/
ScmObj ScmOp_eqvp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type;
    DECLARE_FUNCTION("eqv?", ProcedureFixed2);

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    type = SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
    case ScmInt:
        if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2)))
            return SCM_TRUE;
        break;

    case ScmChar:
        return ScmOp_char_equal(obj1, obj2);

#if SCM_DEBUG
    case ScmFreeCell:
        SigScm_Error("eqv? : cannnot compare freecell, gc broken?");
        break;
#endif

    default:
        break;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_eqp(ScmObj obj1, ScmObj obj2)
{
    DECLARE_FUNCTION("eq?", ProcedureFixed2);
    return (EQ(obj1, obj2)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_equalp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type;
    int i = 0;
    ScmObj elm1 = SCM_FALSE;
    ScmObj elm2 = SCM_FALSE;
    DECLARE_FUNCTION("equal?", ProcedureFixed2);

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    type = SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
    case ScmInt:
        if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2)))
            return SCM_TRUE;
        break;

    case ScmChar:
        return ScmOp_char_equal(obj1, obj2);

    case ScmString:
        if (strcmp(SCM_STRING_STR(obj1), SCM_STRING_STR(obj2)) == 0)
            return SCM_TRUE;
        break;

    case ScmCons:
        for (; CONSP(obj1) && CONSP(obj2); obj1 = CDR(obj1), obj2 = CDR(obj2))
        {
            elm1 = CAR(obj1);
            elm2 = CAR(obj2);
            if (!EQ(elm1, elm2)
                && (SCM_TYPE(elm1) != SCM_TYPE(elm2)
                    || FALSEP(ScmOp_equalp(elm1, elm2))))
                return SCM_FALSE;
        }
        /* compare last cdr */
        return (EQ(obj1, obj2)) ? SCM_TRUE : ScmOp_equalp(obj1, obj2);

    case ScmVector:
        if (SCM_VECTOR_LEN(obj1) != SCM_VECTOR_LEN(obj2))
            return SCM_FALSE;

        for (i = 0; i < SCM_VECTOR_LEN(obj1); i++) {
            elm1 = SCM_VECTOR_CREF(obj1, i);
            elm2 = SCM_VECTOR_CREF(obj2, i);
            if (!EQ(elm1, elm2)
                && (SCM_TYPE(elm1) != SCM_TYPE(elm2)
                    || FALSEP(ScmOp_equalp(elm1, elm2))))
                return SCM_FALSE;
        }
        return SCM_TRUE;

#if SCM_USE_NONSTD_FEATURES
    case ScmCPointer:
        if (SCM_C_POINTER_VALUE(obj1) == SCM_C_POINTER_VALUE(obj2))
            return SCM_TRUE;
        break;

    case ScmCFuncPointer:
        if (SCM_C_FUNCPOINTER_VALUE(obj1) == SCM_C_FUNCPOINTER_VALUE(obj2))
            return SCM_TRUE;
        break;
#endif

#if SCM_DEBUG
    case ScmFreeCell:
        ERR("cannnot compare freecell, gc broken?");
        break;
#endif

    default:
        break;
    }

    return SCM_FALSE;
}

/*=======================================
  R5RS : 6.2 Numbers
=======================================*/
/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.5 Numerical Operations
==============================================================================*/
/* Note: SigScheme supports only the integer part of the numerical tower. */

ScmObj ScmOp_add(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 0;
    DECLARE_FUNCTION("+", ReductionOperator);
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ASSERT_INTP(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ASSERT_INTP(right);
        result += SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        ERR("(internal error) unrecognized state specifier: %d", *state);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_multiply(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    DECLARE_FUNCTION("*", ReductionOperator);
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ASSERT_INTP(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ASSERT_INTP(right);
        result *= SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        ERR("(internal error) unrecognized state specifier: %d", *state);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 0;
    DECLARE_FUNCTION("-", ReductionOperator);
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ASSERT_INTP(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ASSERT_INTP(right);
        result -= SCM_INT_VALUE(right);
        break;

    case SCM_REDUCE_0:
        ERR("at least 1 argument required");
    default:
        ERR("(internal error) unrecognized state specifier: %d", *state);
    }
    return Scm_NewInt(result);
}

ScmObj ScmOp_divide(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    DECLARE_FUNCTION("/", ReductionOperator);
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ASSERT_INTP(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ASSERT_INTP(right);
        if (SCM_INT_VALUE(right) == 0)
            ERR("division by zero");
        result /= SCM_INT_VALUE(right);
        break;
    case SCM_REDUCE_0:
        ERR("at least 1 argument required");
    default:
        ERR("(internal error) unrecognized state specifier: %d", *state);
    }
    return Scm_NewInt(result);
}

ScmObj ScmOp_numberp(ScmObj obj)
{
    DECLARE_FUNCTION("number?", ProcedureFixed1);
    return (INTP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("=", ReductionOperator);
#define COMPARATOR_BODY(op) \
    switch (*state) { \
    case SCM_REDUCE_0: \
    case SCM_REDUCE_1: \
        ERR("at least 2 arguments required"); \
    case SCM_REDUCE_PARTWAY: \
    case SCM_REDUCE_LAST: \
        ASSERT_INTP(left); \
        ASSERT_INTP(right); \
        if (SCM_INT_VALUE(left) op SCM_INT_VALUE(right)) \
            return *state == SCM_REDUCE_LAST ? SCM_TRUE : right; \
        *state = SCM_REDUCE_STOP; \
        return SCM_FALSE; \
    default: \
        ERR("(internal error) unrecognized state specifier: %d", *state); \
    } \
    return SCM_INVALID

    COMPARATOR_BODY(==);
}

ScmObj ScmOp_less(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<", ReductionOperator);
    COMPARATOR_BODY(<);
}

ScmObj ScmOp_less_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<=", ReductionOperator);
    COMPARATOR_BODY(<=);
}

ScmObj ScmOp_greater(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">", ReductionOperator);
    COMPARATOR_BODY(>);
}

ScmObj ScmOp_greater_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">=", ReductionOperator);
    COMPARATOR_BODY(>=);
#undef COMPARATOR_BODY
}

ScmObj ScmOp_zerop(ScmObj scm_num)
{
    DECLARE_FUNCTION("zero?", ProcedureFixed1);
    ASSERT_INTP(scm_num);
    return (SCM_INT_VALUE(scm_num) == 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_positivep(ScmObj scm_num)
{
    DECLARE_FUNCTION("positive?", ProcedureFixed1);
    ASSERT_INTP(scm_num);
    return (SCM_INT_VALUE(scm_num) > 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_negativep(ScmObj scm_num)
{
    DECLARE_FUNCTION("negative?", ProcedureFixed1);
    ASSERT_INTP(scm_num);
    return (SCM_INT_VALUE(scm_num) < 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_oddp(ScmObj scm_num)
{
    DECLARE_FUNCTION("odd?", ProcedureFixed1);
    ASSERT_INTP(scm_num);
    return (SCM_INT_VALUE(scm_num) & 0x1) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_evenp(ScmObj scm_num)
{
    DECLARE_FUNCTION("even?", ProcedureFixed1);
    ASSERT_INTP(scm_num);
    return (SCM_INT_VALUE(scm_num) & 0x1) ? SCM_FALSE : SCM_TRUE;
}

ScmObj ScmOp_max(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("max", ReductionOperator);
    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ASSERT_INTP(left);
    ASSERT_INTP(right);

    return SCM_INT_VALUE(left) > SCM_INT_VALUE(right) ? left : right;
}

ScmObj ScmOp_min(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("min", ReductionOperator);
    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ASSERT_INTP(left);
    ASSERT_INTP(right);

    return SCM_INT_VALUE(left) < SCM_INT_VALUE(right) ? left : right;
}


ScmObj ScmOp_abs(ScmObj scm_num)
{
    int num = 0;
    DECLARE_FUNCTION("abs", ProcedureFixed1);

    ASSERT_INTP(scm_num);

    num = SCM_INT_VALUE(scm_num);

    return (num < 0) ? Scm_NewInt(-num) : scm_num;
}

ScmObj ScmOp_quotient(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1 = 0;
    int n2 = 0;
    DECLARE_FUNCTION("quotient", ProcedureFixed2);

    ASSERT_INTP(scm_n1);
    ASSERT_INTP(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return Scm_NewInt((int)(n1 / n2));
}

ScmObj ScmOp_modulo(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1  = 0;
    int n2  = 0;
    int rem = 0;
    DECLARE_FUNCTION("modulo", ProcedureFixed2);

    ASSERT_INTP(scm_n1);
    ASSERT_INTP(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    rem  = n1 % n2;
    if (n1 < 0 && n2 > 0) {
        rem += n2;
    } else if (n1 > 0 && n2 < 0) {
        rem += n2;
    }

    return Scm_NewInt(rem);
}

ScmObj ScmOp_remainder(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1  = 0;
    int n2  = 0;
    DECLARE_FUNCTION("remainder", ProcedureFixed2);

    ASSERT_INTP(scm_n1);
    ASSERT_INTP(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return Scm_NewInt(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/
ScmObj ScmOp_number2string(ScmObj num, ScmObj args)
{
  char buf[sizeof(int)*CHAR_BIT + 1];
  char *p;
  unsigned int n, r;
  ScmObj radix;
  DECLARE_FUNCTION("number->string", ProcedureVariadic1);

  ASSERT_INTP(num);
  n = SCM_INT_VALUE(num);

  /* r = radix */
  if (NO_MORE_ARG(args))
      r = 10;
  else {
      radix = POP_ARG(args);
      ASSERT_NO_MORE_ARG(args);

      ASSERT_INTP(radix);
      r = SCM_INT_VALUE(radix);
#if SCM_STRICT_R5RS
      if (!(r == 2 || r == 8 || r == 10 || r == 16))
#else
      if (!(2 <= r && r <= 16))
#endif
          ERR_OBJ("invalid or unsupported radix", radix);
  }

  /* no signs for nondecimals */
  if (r != 10)
      n = abs(n);

  /* initialize buffer */
  p = &buf[sizeof(buf)-1];
  *p = 0;

  do {
      if (n % r > 9)
        *--p = 'A' + n % r - 10;
      else
        *--p = '0' + n % r;
  } while (n /= r);
  if (r == 10 && SCM_INT_VALUE (num) < 0)
    *--p = '-';

  return Scm_NewStringCopying(p);
}

/* TODO : support radix */
ScmObj ScmOp_string2number(ScmObj string)
{
    char  *str = NULL;
    char  *p   = NULL;
    size_t len = 0;
    DECLARE_FUNCTION("string->number", ProcedureFixed1);

    ASSERT_STRINGP(string);

    str = SCM_STRING_STR(string);
    len = strlen(str);
    for (p = str; p < str + len; p++) {
        if (isdigit(*p) == 0)
            return SCM_FALSE;
    }

    return Scm_NewInt((int)atoi(SCM_STRING_STR(string)));
}

/*===================================
  R5RS : 6.3 Other data types
===================================*/
/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.1 Booleans
==============================================================================*/
ScmObj ScmOp_not(ScmObj obj)
{
    DECLARE_FUNCTION("not", ProcedureFixed1);
    return (FALSEP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_booleanp(ScmObj obj)
{
    DECLARE_FUNCTION("boolean?", ProcedureFixed1);
    return (EQ(obj, SCM_FALSE) || EQ(obj, SCM_TRUE)) ? SCM_TRUE : SCM_FALSE;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
==============================================================================*/
ScmObj ScmOp_car(ScmObj obj)
{
    DECLARE_FUNCTION("car", PocedureFixed1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ASSERT_CONSP(obj);

    return CAR(obj);
}

ScmObj ScmOp_cdr(ScmObj obj)
{
    DECLARE_FUNCTION("cdr", ProcedureFixed1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ASSERT_CONSP(obj);

    return CDR(obj);
}

ScmObj ScmOp_pairp(ScmObj obj)
{
    DECLARE_FUNCTION("pair?", ProcedureFixed1);
    return (CONSP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_cons(ScmObj car, ScmObj cdr)
{
    DECLARE_FUNCTION("cons", ProcedureFixed1);
    return CONS(car, cdr);
}

ScmObj ScmOp_setcar(ScmObj pair, ScmObj car)
{
    DECLARE_FUNCTION("set-car!", SyntaxFixed2);
    ASSERT_CONSP(pair);

    SET_CAR(pair, car);

#if SCM_COMPAT_SIOD
    return car;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_setcdr(ScmObj pair, ScmObj cdr)
{
    DECLARE_FUNCTION("set-cdr!", SyntaxFixed2);
    ASSERT_CONSP(pair);

    SET_CDR(pair, cdr);

#if SCM_COMPAT_SIOD
    return cdr;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_caar(ScmObj lst)
{
    DECLARE_FUNCTION("caar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car(lst) );
}
ScmObj ScmOp_cadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr(lst) );
}
ScmObj ScmOp_cdar(ScmObj lst)
{
    DECLARE_FUNCTION("cdar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car(lst) );
}
ScmObj ScmOp_cddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr(lst) );
}
ScmObj ScmOp_caddr(ScmObj lst)
{
    DECLARE_FUNCTION("caddr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cdddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) ));
}

#if SCM_USE_DEEP_CADRS
ScmObj ScmOp_caaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_caadr(ScmObj lst)
{
    DECLARE_FUNCTION("caadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cadar(ScmObj lst)
{
    DECLARE_FUNCTION("cadar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cddar(ScmObj lst)
{
    DECLARE_FUNCTION("cddar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) ));
}
ScmObj ScmOp_caaaar(ScmObj lst)
{
    DECLARE_FUNCTION("caaaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaadr(ScmObj lst)
{
    DECLARE_FUNCTION("caaadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caadar(ScmObj lst)
{
    DECLARE_FUNCTION("caadar", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaddr(ScmObj lst)
{
    DECLARE_FUNCTION("caaddr", ProcedureFixed1);
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cadaar(ScmObj lst)
{
    DECLARE_FUNCTION("cadaar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadadr(ScmObj lst)
{
    DECLARE_FUNCTION("cadadr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caddar(ScmObj lst)
{
    DECLARE_FUNCTION("caddar", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadddr(ScmObj lst)
{
    DECLARE_FUNCTION("cadddr", ProcedureFixed1);
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdaaar(ScmObj lst)
{
    DECLARE_FUNCTION("cdaaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaadr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdadar(ScmObj lst)
{
    DECLARE_FUNCTION("cdadar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaddr(ScmObj lst)
{
    DECLARE_FUNCTION("cdaddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cddaar(ScmObj lst)
{
    DECLARE_FUNCTION("cddaar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddadr(ScmObj lst)
{
    DECLARE_FUNCTION("cddadr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdddar(ScmObj lst)
{
    DECLARE_FUNCTION("cdddar", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddddr(ScmObj lst)
{
    DECLARE_FUNCTION("cddddr", ProcedureFixed1);
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}
#endif /* SCM_USE_DEEP_CADRS */

ScmObj ScmOp_list(ScmObj args)
{
    DECLARE_FUNCTION("list", ProcedureVariadic0);
    return args;
}

ScmObj ScmOp_nullp(ScmObj obj)
{
    DECLARE_FUNCTION("null?", ProcedureFixed1);
    return (NULLP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_listp(ScmObj obj)
{
    int len = 0;
    DECLARE_FUNCTION("list?", ProcedureFixed1);

    if (NULLP(obj))
        return SCM_TRUE;
    if (!CONSP(obj))
        return SCM_FALSE;

    len = ScmOp_c_length(obj);

    return (len != -1) ? SCM_TRUE : SCM_FALSE;
}

/*
 * Notice
 *
 * This function is ported from Gauche, by Shiro Kawai(shiro@acm.org)
 */
int ScmOp_c_length(ScmObj lst)
{
    ScmObj slow = lst;
    int len = 0;

    for (;;) {
        if (NULLP(lst)) break;
        if (!CONSP(lst)) return -1;
        if (len != 0 && lst == slow) return -1; /* circular */

        lst = CDR(lst);
        len++;
        if (NULLP(lst)) break;
        if (!CONSP(lst)) return -1;
        if (lst == slow) return -1; /* circular */

        lst = CDR(lst);
        slow = CDR(slow);
        len++;
    }

    return len;
}

ScmObj ScmOp_length(ScmObj obj)
{
    int len = ScmOp_c_length(obj);
    DECLARE_FUNCTION("length", ProcedureFixed1);

    if (len < 0)
        ERR_OBJ("list required but got", obj);

    return Scm_NewInt(len);
}

/*
 * FIXME: Invalid direct cdr part referencing as lvalue. Don't assume such
 * specific storage model. It breaks the abstract storage API. For example,
 * base pointer + offset representation will not work under the lvalue
 * assumption. Use SET_CDR properly.  -- YamaKen 2005-09-23
 */
ScmObj ScmOp_append(ScmObj args)
{
    ScmObj ret_lst = SCM_NULL;
    ScmObj *ret_tail = &ret_lst;

    ScmObj ls;
    ScmObj obj = SCM_NULL;

    if (NULLP(args))
        return SCM_NULL;

    /* duplicate and merge all but the last argument */
    for (; !NULLP(CDR(args)); args = CDR(args)) {
        for (ls = CAR(args); CONSP(ls); ls = CDR(ls)) {
            obj = CAR(ls);
            *ret_tail = CONS(obj, SCM_NULL);
            ret_tail = &CDR(*ret_tail);
        }
        if (!NULLP(ls))
            SigScm_ErrorObj("append: proper list required but got: ",
                            CAR(args));
    }

    /* append the last argument */
    *ret_tail = CAR(args);

    return ret_lst;
}

ScmObj ScmOp_reverse(ScmObj lst)
{
    ScmObj ret_lst  = SCM_NULL;

    for (; CONSP(lst); lst = CDR(lst))
        ret_lst = CONS(CAR(lst), ret_lst);

    if (!NULLP(lst))
        SigScm_ErrorObj("reverse: got improper list: ", lst);

    return ret_lst;
}

static ScmObj ScmOp_listtail_internal(ScmObj lst, int k)
{
    while (k--) {
        if (!CONSP(lst))
            return SCM_INVALID;
        lst = CDR(lst);
    }

    return lst;
}

ScmObj ScmOp_list_tail(ScmObj lst, ScmObj scm_k)
{
    ScmObj ret;

    if (FALSEP(ScmOp_numberp(scm_k)))
        SigScm_ErrorObj("list-tail: number required but got ", scm_k);

    ret = ScmOp_listtail_internal(lst, SCM_INT_VALUE(scm_k));

    if (EQ(ret, SCM_INVALID))
        SigScm_ErrorObj("list-tail: out of range or bad list, arglist is: ",
                        CONS(lst, scm_k));
    return ret;
}

ScmObj ScmOp_list_ref(ScmObj lst, ScmObj scm_k)
{
    ScmObj tail = SCM_NULL;

    if (FALSEP(ScmOp_numberp(scm_k)))
        SigScm_ErrorObj("list-ref : int required but got ", scm_k);

    tail = ScmOp_listtail_internal(lst, SCM_INT_VALUE(scm_k));
    if (EQ(tail, SCM_INVALID) || NULLP(tail))
        SigScm_ErrorObj("list-ref : out of range or bad list, arglist is: ",
                        CONS(lst, scm_k));

    return CAR(tail);
}

ScmObj ScmOp_memq(ScmObj obj, ScmObj lst)
{
    for (; CONSP(lst); lst = CDR(lst))
        if (EQ(obj, CAR(lst)))
            return lst;

    return SCM_FALSE;
}

ScmObj ScmOp_memv(ScmObj obj, ScmObj lst)
{
    for (; CONSP(lst); lst = CDR(lst))
        if (NFALSEP(ScmOp_eqvp(obj, CAR(lst))))
            return lst;

    return SCM_FALSE;
}

ScmObj ScmOp_member(ScmObj obj, ScmObj lst)
{
    for (; CONSP(lst); lst = CDR(lst))
        if (NFALSEP(ScmOp_equalp(obj, CAR(lst))))
            return lst;

    return SCM_FALSE;
}

ScmObj ScmOp_assq(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        if (!CONSP(tmpobj))
            SigScm_ErrorObj("assq: invalid alist: ", alist);
        if (EQ(CAR(tmpobj), obj))
            return tmpobj;
#else
        if (CONSP(tmpobj) && EQ(CAR(tmpobj), obj))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assv(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        if (!CONSP(tmpobj))
            SigScm_ErrorObj("assv: invalid alist: ", alist);
        if (NFALSEP(ScmOp_eqvp(car, obj)))
            return tmpobj;
#else
        if (CONSP(tmpobj) && NFALSEP(ScmOp_eqvp(car, obj)))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assoc(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        if (!CONSP(tmpobj))
            SigScm_ErrorObj("assoc: invalid alist: ", alist);
        if (NFALSEP(ScmOp_equalp(car, obj)))
            return tmpobj;
#else
        if (CONSP(tmpobj) && NFALSEP(ScmOp_equalp(car, obj)))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}


/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.3 Symbols
==============================================================================*/
ScmObj ScmOp_symbolp(ScmObj obj)
{
    DECLARE_FUNCTION("symbol?", ProcedureFixed1);
    return (SYMBOLP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_symbol2string(ScmObj obj)
{
    DECLARE_FUNCTION("symbol->string", ProcedureFixed1);
    ASSERT_SYMBOLP(obj);
    return Scm_NewStringCopying(SCM_SYMBOL_NAME(obj));
}

ScmObj ScmOp_string2symbol(ScmObj str)
{
    DECLARE_FUNCTION("string->symbol", ProcedureFixed1);
    ASSERT_STRINGP(str);
    return Scm_Intern(SCM_STRING_STR(str));
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.4 Characters
==============================================================================*/
ScmObj ScmOp_charp(ScmObj obj)
{
    DECLARE_FUNCTION("char?", ProcedureFixed1);
    return (CHARP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_char_equal(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char=?", ProcedureFixed2);
    ASSERT_CHARP(ch1);
    ASSERT_CHARP(ch2);

    if (strcmp(SCM_CHAR_VALUE(ch1), SCM_CHAR_VALUE(ch2)) == 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_alphabeticp(ScmObj obj)
{
    DECLARE_FUNCTION("char-alphabetic?", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return SCM_FALSE;

    /* check alphabet */
    if (isalpha(SCM_CHAR_VALUE(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_numericp(ScmObj obj)
{
    DECLARE_FUNCTION("char-numeric?", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return SCM_FALSE;

    /* check digit */
    if (isdigit(SCM_CHAR_VALUE(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_whitespacep(ScmObj obj)
{
    DECLARE_FUNCTION("char-whitespace?", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return SCM_FALSE;

    /* check space */
    if (isspace(SCM_CHAR_VALUE(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_upper_casep(ScmObj obj)
{
    DECLARE_FUNCTION("char-upper-case?", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return SCM_FALSE;

    /* check uppercase */
    if (isupper(SCM_CHAR_VALUE(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_lower_casep(ScmObj obj)
{
    DECLARE_FUNCTION("char-lower-case?", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return SCM_FALSE;

    /* check lowercase */
    if (islower(SCM_CHAR_VALUE(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_upcase(ScmObj obj)
{
    DECLARE_FUNCTION("char-upcase", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return obj;

    /* to upcase */
    SCM_CHAR_VALUE(obj)[0] = toupper(SCM_CHAR_VALUE(obj)[0]);

    return obj;
}

ScmObj ScmOp_char_downcase(ScmObj obj)
{
    DECLARE_FUNCTION("char-downcase", ProcedureFixed1);
    ASSERT_CHARP(obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return obj;

    /* to upcase */
    SCM_CHAR_VALUE(obj)[0] = tolower(SCM_CHAR_VALUE(obj)[0]);

    return obj;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.5 Strings
==============================================================================*/
ScmObj ScmOp_stringp(ScmObj obj)
{
    DECLARE_FUNCTION("string?", ProcedureFixed1);
    return (STRINGP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_make_string(ScmObj length, ScmObj args)
{
    int len = 0;
    ScmObj str    = SCM_FALSE;
    ScmObj filler = SCM_FALSE;
    DECLARE_FUNCTION("make-string", ProcedureVariadic1);

    ASSERT_INTP(length);
    len = SCM_INT_VALUE(length);
    if (len == 0)
        return Scm_NewStringCopying("");

    /* specify filler */
    if (NO_MORE_ARG(args)) {
        filler = Scm_NewChar(strdup(" "));
    } else {
        filler = POP_ARG(args);
        ASSERT_CHARP(filler);
    }

    /* make string */
    str = Scm_NewStringWithLen(NULL, len);

    /* and fill! */
    ScmOp_string_fill(str, filler);

    return str;
}

ScmObj ScmOp_string(ScmObj args)
{
    DECLARE_FUNCTION("string", ProcedureVariadic0);
    return ScmOp_list2string(args);
}

ScmObj ScmOp_string_length(ScmObj str)
{
    DECLARE_FUNCTION("string-length", ProcedureFixed1);
    ASSERT_STRINGP(str);
    return Scm_NewInt(SigScm_default_encoding_strlen(SCM_STRING_STR(str)));
}

ScmObj ScmOp_string_ref(ScmObj str, ScmObj k)
{
    int   c_index = 0;
    char *new_ch  = NULL;
    const char *string_str   = NULL;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;
    DECLARE_FUNCTION("string-ref", ProcedureFixed2);

    ASSERT_STRINGP(str);
    ASSERT_INTP(k);

    /* get start_ptr and end_ptr */
    c_index = SCM_INT_VALUE(k);
    string_str   = SCM_STRING_STR(str);
    ch_start_ptr = SigScm_default_encoding_str_startpos(string_str, c_index);
    ch_end_ptr   = SigScm_default_encoding_str_endpos(string_str, c_index);

    /* copy from start_ptr to end_ptr */
    new_ch = (char*)malloc(sizeof(char) * (ch_end_ptr - ch_start_ptr) + 1);
    memset(new_ch, 0, sizeof(char) * (ch_end_ptr - ch_start_ptr) + 1);
    strncpy(new_ch, ch_start_ptr, (ch_end_ptr - ch_start_ptr));

    return Scm_NewChar(new_ch);
}

ScmObj ScmOp_string_set(ScmObj str, ScmObj k, ScmObj ch)
{
    int   c_start_index = 0;
    int   front_size = 0;
    int   newch_size = 0;
    int   back_size  = 0;
    int   total_size = 0;
    char *new_str  = NULL;
    const char *string_str   = NULL;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;
    DECLARE_FUNCTION("string-set!", ProcedureFixed3);

    ASSERT_STRINGP(str);
    ASSERT_INTP(k);
    ASSERT_CHARP(ch);

    /* get indexes */
    c_start_index = SCM_INT_VALUE(k);
    string_str    = SCM_STRING_STR(str);
    ch_start_ptr  = SigScm_default_encoding_str_startpos(string_str, c_start_index);
    ch_end_ptr    = SigScm_default_encoding_str_endpos(string_str, c_start_index);

    /* calculate total size */
    front_size = strlen(string_str) - strlen(ch_start_ptr);
    newch_size = strlen(SCM_CHAR_VALUE(ch));
    back_size  = strlen(ch_end_ptr);
    total_size = front_size + newch_size + back_size;

    /* copy each parts */
    new_str = (char*)malloc(total_size + 1);
    memset(new_str, 0, total_size + 1);
    strncpy(new_str                           , string_str      , front_size);
    strncpy(new_str + front_size              , SCM_CHAR_VALUE(ch) , newch_size);
    strncpy(new_str + front_size + newch_size , ch_end_ptr      , back_size);

    /* set */
    if (SCM_STRING_STR(str))
        free(SCM_STRING_STR(str));

    SCM_STRING_SET_STR(str, new_str);

    return str;
}

ScmObj ScmOp_string_equal(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string=", ProcedureFixed2);

    ASSERT_STRINGP(str1);
    ASSERT_STRINGP(str2);

    if (strcmp(SCM_STRING_STR(str1), SCM_STRING_STR(str2)) == 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_string_substring(ScmObj str, ScmObj start, ScmObj end)
{
    int   c_start_index = 0;
    int   c_end_index   = 0;
    char *new_str  = NULL;
    const char *string_str   = NULL;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;
    DECLARE_FUNCTION("substring", ProcedureFixed3);

    ASSERT_STRINGP(str);
    ASSERT_INTP(start);
    ASSERT_INTP(end);

    /* get start_ptr and end_ptr */
    c_start_index = SCM_INT_VALUE(start);
    c_end_index   = SCM_INT_VALUE(end);

    /* sanity check */
    if (c_start_index == c_end_index)
        return Scm_NewStringCopying("");

    /* get str */
    string_str    = SCM_STRING_STR(str);
    ch_start_ptr  = SigScm_default_encoding_str_startpos(string_str, c_start_index);
    ch_end_ptr    = SigScm_default_encoding_str_startpos(string_str, c_end_index);

    /* copy from start_ptr to end_ptr */
    new_str = (char*)malloc(sizeof(char) * (ch_end_ptr - ch_start_ptr) + 1);
    memset(new_str, 0, sizeof(char) * (ch_end_ptr - ch_start_ptr) + 1);
    strncpy(new_str, ch_start_ptr, sizeof(char) * (ch_end_ptr - ch_start_ptr));

    return Scm_NewString(new_str);
}

ScmObj ScmOp_string_append(ScmObj args)
{
    /* FIXME: transition to new arg extraction mechanism incomplete. */
    int total_size = 0;
    int total_len  = 0;
    ScmObj strings = SCM_NULL;
    ScmObj obj     = SCM_NULL;
    char  *new_str = NULL;
    char  *p       = NULL;
    DECLARE_FUNCTION("string-append", ProcedureFixed1);

    if (NO_MORE_ARG(args))
        return Scm_NewStringCopying("");

    /* count total size of the new string */
    for (strings = args; !NULLP(strings); strings = CDR(strings)) {
        obj = CAR(strings);
        if (!STRINGP(obj))
            SigScm_ErrorObj("string-append : string required but got ", obj);

        total_size += strlen(SCM_STRING_STR(obj));
        total_len  += SCM_STRING_LEN(obj);
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy string by string */
    p = new_str;
    for (strings = args; !NULLP(strings); strings = CDR(strings)) {
        obj = CAR(strings);

        strcpy(p, SCM_STRING_STR(obj));
        p += strlen(SCM_STRING_STR(obj));
    }

    return Scm_NewStringWithLen(new_str, total_len);
}

ScmObj ScmOp_string2list(ScmObj string)
{
    char *string_str = NULL;
    int   str_len    = 0;
    ScmObj head = SCM_NULL;
    ScmObj prev = NULL;
    ScmObj next = NULL;
    int i = 0;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;
    char *new_ch = NULL;
    DECLARE_FUNCTION("string->list", string);

    ASSERT_STRINGP(string);

    string_str = SCM_STRING_STR(string);
    str_len    = SCM_STRING_LEN(string);
    if (str_len == 0)
        return SCM_NULL;

    for (i = 0; i < str_len; i++) {
        ch_start_ptr = SigScm_default_encoding_str_startpos(string_str, i);
        ch_end_ptr   = SigScm_default_encoding_str_endpos(string_str, i);

        new_ch = (char*)malloc(sizeof(char) * (ch_end_ptr - ch_start_ptr + 1));
        memset(new_ch, 0, sizeof(char) * (ch_end_ptr - ch_start_ptr + 1));
        strncpy(new_ch, ch_start_ptr, (sizeof(char) * (ch_end_ptr - ch_start_ptr)));

        next = CONS(Scm_NewChar(new_ch), SCM_NULL);
        if (prev)
            SET_CDR(prev, next);
        else
            head = next;

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list2string(ScmObj lst)
{
    int len = 0;
    int total_size = 0;
    ScmObj chars   = SCM_NULL;
    ScmObj obj     = SCM_NULL;
    char  *new_str = NULL;
    char  *ch      = NULL;
    char  *p       = NULL;

    if (FALSEP(ScmOp_listp(lst)))
        SigScm_ErrorObj("list->string : list required but got ", lst);

    if (NULLP(lst))
        return Scm_NewStringCopying("");

    /* count total size of the string */
    for (chars = lst; !NULLP(chars); chars = CDR(chars)) {
        obj = CAR(chars);
        if (!CHARP(obj))
            SigScm_ErrorObj("list->string : char required but got ", obj);

        total_size += strlen(SCM_CHAR_VALUE(obj));
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy char by char */
    p = new_str;
    for (chars = lst; !NULLP(chars); chars = CDR(chars)) {
        obj = CAR(chars);
        ch  = SCM_CHAR_VALUE(obj);
        len = strlen(SCM_CHAR_VALUE(obj));

        strcpy(p, ch);
        p += len;
    }

    return Scm_NewString(new_str);
}

ScmObj ScmOp_string_copy(ScmObj string)
{
    DECLARE_FUNCTION("string-copy", ProcedureFixed1);
    ASSERT_STRINGP(string);
    return Scm_NewStringCopying(SCM_STRING_STR(string));
}

ScmObj ScmOp_string_fill(ScmObj string, ScmObj ch)
{
    int  char_size = 0;
    int  str_len   = 0;
    char *new_str  = NULL;
    char *p        = NULL;
    int   i        = 0;
    DECLARE_FUNCTION("string-fill!", ProcedureFixed2);

    ASSERT_STRINGP(string);
    ASSERT_CHARP(ch);

    /* create new str */
    char_size = strlen(SCM_CHAR_VALUE(ch));
    str_len   = SCM_STRING_LEN(string);
    new_str   = (char*)realloc(SCM_STRING_STR(string),
                               sizeof(char) * str_len * char_size + 1);
    for (i = 0, p = new_str; i < char_size * str_len;) {
        strcpy(p, SCM_CHAR_VALUE(ch));

        p += char_size;
        i += char_size;
    }

    SCM_STRING_SET_STR(string, new_str);

    return string;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.6 Vectors
==============================================================================*/
ScmObj ScmOp_vectorp(ScmObj obj)
{
    return (VECTORP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_make_vector(ScmObj vector_len, ScmObj args)
{
    ScmObj *vec    = NULL;
    ScmObj  filler = SCM_FALSE;
    int len = 0;
    int i   = 0;

    if (!INTP(vector_len))
        SigScm_ErrorObj("make-vector : integer required but got ", vector_len);

    /* allocate vector */
    len = SCM_INT_VALUE(vector_len);
    vec = (ScmObj*)malloc(sizeof(ScmObj) * len);

    /* fill vector */
    filler = SCM_UNDEF;
    if (!NULLP(args))
        filler = CAR(args);

    for (i = 0; i < len; i++)
        vec[i] = filler;

    return Scm_NewVector(vec, len);
}

ScmObj ScmOp_vector(ScmObj args)
{
    int len = SCM_INT_VALUE(ScmOp_length(args));
    int i   = 0;
    ScmObj *vec = (ScmObj*)malloc(sizeof(ScmObj) * len); /* allocate vector */

    /* set item */
    for (i = 0; i < len; i++)
        SCM_SHIFT_RAW_1(vec[i], args);

    return Scm_NewVector(vec, len);
}

ScmObj ScmOp_vector_length(ScmObj vec)
{
    DECLARE_FUNCTION("vector-length", ProcedureFixed1);

    ASSERT_VECTORP(vec);
    return Scm_NewInt(SCM_VECTOR_LEN(vec));
}

ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj scm_k)
{
    DECLARE_FUNCTION("vector-ref", ProcedureFixed2);

    ASSERT_VECTORP(vec);
    ASSERT_INTP(scm_k);

    return SCM_VECTOR_REF(vec, scm_k);
}

ScmObj ScmOp_vector_set(ScmObj vec, ScmObj scm_k, ScmObj obj)
{
    DECLARE_FUNCTION("vector-set!", ProcedureFixed3);
    ASSERT_VECTORP(vec);
    ASSERT_INTP(scm_k);

    SCM_VECTOR_SET_REF(vec, scm_k, obj);

    return SCM_UNDEF;
}

ScmObj ScmOp_vector2list(ScmObj vec)
{
    ScmObj *v    = NULL;
    ScmObj  prev = NULL;
    ScmObj  next = NULL;
    ScmObj  head = NULL;
    int c_len = 0;
    int i = 0;
    DECLARE_FUNCTION("vector->list", ProcedureFixed1);

    ASSERT_VECTORP(vec);

    v = SCM_VECTOR_VEC(vec);
    c_len = SCM_VECTOR_LEN(vec);
    if (c_len == 0)
        return SCM_NULL;

    for (i = 0; i < c_len; i++) {
        next = CONS(v[i], SCM_NULL);

        if (prev) {
            SET_CDR(prev, next);
        } else {
            head = next;
        }

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list2vector(ScmObj lst)
{
    ScmObj  scm_len = SCM_NULL;
    ScmObj *v       = NULL;
    int c_len = 0;
    int i = 0;

    /* TOOD : canbe optimized. scanning list many times */
    if (FALSEP(ScmOp_listp(lst)))
        SigScm_ErrorObj("list->vector : list required but got ", lst);

    scm_len = ScmOp_length(lst);
    c_len   = SCM_INT_VALUE(scm_len);
    v       = (ScmObj*)malloc(sizeof(ScmObj) * c_len);
    for (i = 0; i < c_len; i++) {
        v[i] = CAR(lst);
        lst  = CDR(lst);
    }

    return Scm_NewVector(v, c_len);
}

ScmObj ScmOp_vector_fill(ScmObj vec, ScmObj fill)
{
    int c_len = 0;
    int i = 0;
    DECLARE_FUNCTION("vector-fill!", ProcedureFixed2);

    ASSERT_VECTORP(vec);

    c_len = SCM_VECTOR_LEN(vec);
    for (i = 0; i < c_len; i++) {
        SCM_VECTOR_VEC(vec)[i] = fill;
    }

    return vec;
}

/*=======================================
  R5RS : 6.4 Control Features
=======================================*/
ScmObj ScmOp_procedurep(ScmObj obj)
{
    DECLARE_FUNCTION("procedure?", ProcedureFixed1);
    return (PROCEDUREP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_map(ScmObj proc, ScmObj args)
{
    DECLARE_FUNCTION("map", ProcedureVariadic1);
     /* sanity check */
    if (NULLP(args))
        SigScm_Error("map : wrong number of arguments");

    /* fast path for single arg case */
    if (NULLP(CDR(args)))
        return map_single_arg(proc, CAR(args));

    /* multiple args case */
    return map_multiple_args(proc, args);
}

static ScmObj map_single_arg(ScmObj proc, ScmObj lst)
{
    ScmObj ret        = SCM_FALSE;
    ScmObj ret_last   = SCM_FALSE;
    ScmObj mapped_elm = SCM_FALSE;

    if (NULLP(lst))
        return SCM_NULL;

    for (; !NULLP(lst); lst = CDR(lst)) {
        if (NFALSEP(ret)) {
            /* subsequent */
            mapped_elm = CONS(Scm_call(proc, LIST_1(CAR(lst))), SCM_NULL);
            SET_CDR(ret_last, mapped_elm);
            ret_last = mapped_elm;
        } else {
            /* first */
            ret = CONS(Scm_call(proc, LIST_1(CAR(lst))), SCM_NULL);
            ret_last = ret;
        }
    }

    return ret;
}

/*
 * FIXME:
 * - Simplify and make names appropriate as like as map_singular_arg()
 */
static ScmObj map_multiple_args(ScmObj proc, ScmObj args)
{
    ScmObj map_arg      = SCM_FALSE;
    ScmObj map_arg_last = SCM_FALSE;
    ScmObj tmp_lsts     = SCM_FALSE;
    ScmObj lst          = SCM_FALSE;
    ScmObj ret          = SCM_FALSE;
    ScmObj ret_last     = SCM_FALSE;

    while (1) {
        /* construct "map_arg" */
        map_arg  = SCM_FALSE;
        tmp_lsts = args;
        for (; !NULLP(tmp_lsts); tmp_lsts = CDR(tmp_lsts)) {
            lst = CAR(tmp_lsts);
            if (NULLP(lst))
                return ret;

            if (NFALSEP(map_arg)) {
                /* subsequent */
                SET_CDR(map_arg_last, CONS(CAR(lst), SCM_NULL));
                map_arg_last = CDR(map_arg_last);
            } else {
                /* first */
                map_arg = CONS(CAR(lst), SCM_NULL);
                map_arg_last = map_arg;
            }

            /* update tmp_lsts */
            SET_CAR(tmp_lsts, CDR(lst));
        }

        /* construct "ret" by applying proc to each map_arg */
        if (NFALSEP(ret)) {
            /* subsequent */
            SET_CDR(ret_last, CONS(Scm_call(proc, map_arg), SCM_NULL));
            ret_last = CDR(ret_last);
        } else {
            /* first */
            ret = CONS(Scm_call(proc, map_arg), SCM_NULL);
            ret_last = ret;
        }
    }

    SigScm_Error("map : invalid argument ", args);
    return SCM_NULL;
}

ScmObj ScmOp_for_each(ScmObj proc, ScmObj args)
{
    DECLARE_FUNCTION("for-each", ProcedureVariadic1);
    ScmOp_map(proc, args);

    return SCM_UNDEF;
}

ScmObj ScmOp_force(ScmObj closure)
{
    DECLARE_FUNCTION("force", ProcedureFixed1);
    if (!CLOSUREP(closure))
        SigScm_ErrorObj("force : not proper delayed object ", closure);

    return Scm_call(closure, SCM_NULL);
}

ScmObj ScmOp_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state)
{
    ScmObj cont = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;
    DECLARE_FUNCTION("call-with-current-continuation", ProcedureFixedTailRec1);

    ASSERT_PROCEDUREP(proc);

    cont = Scm_NewContinuation();

    if (setjmp(SCM_CONTINUATION_JMPENV(cont))) {
        /* returned from longjmp */
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        ret = scm_continuation_thrown_obj;
        scm_continuation_thrown_obj = SCM_FALSE;  /* make ret sweepable */
        return ret;
    } else {
#if 1
        /* call proc with current continutation as (proc cont): This call must
         * not be Scm_tailcall(), to preserve current stack until longjmp()
         * called.
         */
        return Scm_call(proc, LIST_1(cont));
#else
        /* ONLY FOR TESTING: This call is properly recursible, but all
         * continuations are broken and cannot be called.
         */
        return Scm_tailcall(proc, LIST_1(cont), eval_state);
#endif
    }
}

ScmObj ScmOp_values(ScmObj args)
{
    DECLARE_FUNCTION("values", ProcedureVariadic0);
    /* Values with one arg must return something that fits an ordinary
     * continuation. */
    if (CONSP(args) && NULLP(CDR(args)))
        return CAR(args);

#if SCM_USE_VALUECONS
    if (NULLP(args)) {
        return SigScm_null_values;
    } else {
        SCM_ENTYPE_VALUEPACKET(args);
        return args;
    }
#else
    /* Otherwise, we'll return the values in a packet. */
    return Scm_NewValuePacket(args);
#endif
}

ScmObj ScmOp_call_with_values(ScmObj producer, ScmObj consumer,
                              ScmEvalState *eval_state)
{
    ScmObj vals;
    DECLARE_FUNCTION("call-with-values", ProcedureFixedTailRec2);

    vals = Scm_call(producer, SCM_NULL);

#if SCM_USE_VALUECONS
    if (SCM_NULLVALUESP(vals)) {
        vals = SCM_NULL;
    } else if (VALUEPACKETP(vals)) {
        SCM_ENTYPE_CONS(vals);
    } else {
        /* got back a single value */
        vals = CONS(vals, SCM_NULL);
    }
#else
    if (!VALUEPACKETP(vals)) {
        /* got back a single value */
        vals = CONS(vals, SCM_NULL);
    } else {
        /* extract */
        vals = SCM_VALUEPACKET_VALUES(vals);
    }
#endif
    
    return Scm_tailcall(consumer, vals, eval_state);
}

#if SCM_USE_SRFI1
#include "operations-srfi1.c"
#endif
#if SCM_USE_SRFI2
#include "operations-srfi2.c"
#endif
#if SCM_USE_SRFI8
#include "operations-srfi8.c"
#endif
#if SCM_USE_SRFI23
#include "operations-srfi23.c"
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
