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
#include <ctype.h>

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
static ScmObj scm_p_listtail_internal(ScmObj obj, int k);

static ScmObj map_single_arg(ScmObj proc, ScmObj args);
static ScmObj map_multiple_args(ScmObj proc, ScmObj args);

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  R5RS : 6.1 Equivalence predicates
==============================================================================*/
ScmObj
scm_p_eqvp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type;
    DECLARE_FUNCTION("eqv?", procedure_fixed_2);

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
        return scm_p_charequalp(obj1, obj2);

#if SCM_DEBUG
    case ScmFreeCell:
        ERR("eqv?: cannnot compare freecell, gc broken?");
        break;
#endif

    default:
        break;
    }

    return SCM_FALSE;
}

ScmObj
scm_p_eqp(ScmObj obj1, ScmObj obj2)
{
    DECLARE_FUNCTION("eq?", procedure_fixed_2);
    return MAKE_BOOL(EQ(obj1, obj2));
}

ScmObj
scm_p_equalp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type;
    int i = 0;
    ScmObj elm1 = SCM_FALSE;
    ScmObj elm2 = SCM_FALSE;
    DECLARE_FUNCTION("equal?", procedure_fixed_2);

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
        return scm_p_charequalp(obj1, obj2);

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
                    || FALSEP(scm_p_equalp(elm1, elm2))))
                return SCM_FALSE;
        }
        /* compare last cdr */
        return (EQ(obj1, obj2)) ? SCM_TRUE : scm_p_equalp(obj1, obj2);

    case ScmVector:
        if (SCM_VECTOR_LEN(obj1) != SCM_VECTOR_LEN(obj2))
            return SCM_FALSE;

        for (i = 0; i < SCM_VECTOR_LEN(obj1); i++) {
            elm1 = SCM_VECTOR_VEC(obj1)[i];
            elm2 = SCM_VECTOR_VEC(obj2)[i];
            if (!EQ(elm1, elm2)
                && (SCM_TYPE(elm1) != SCM_TYPE(elm2)
                    || FALSEP(scm_p_equalp(elm1, elm2))))
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

ScmObj
scm_p_add(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 0;
    DECLARE_FUNCTION("+", reduction_operator);
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

    return MAKE_INT(result);
}

ScmObj
scm_p_multiply(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    DECLARE_FUNCTION("*", reduction_operator);
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

    return MAKE_INT(result);
}

ScmObj
scm_p_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 0;
    DECLARE_FUNCTION("-", reduction_operator);
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
    return MAKE_INT(result);
}

ScmObj
scm_p_divide(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    DECLARE_FUNCTION("/", reduction_operator);
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
    return MAKE_INT(result);
}

ScmObj
scm_p_numberp(ScmObj obj)
{
    DECLARE_FUNCTION("number?", procedure_fixed_1);
    return MAKE_BOOL(INTP(obj));
}

ScmObj
scm_p_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("=", reduction_operator);
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

ScmObj
scm_p_less(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<", reduction_operator);
    COMPARATOR_BODY(<);
}

ScmObj
scm_p_less_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<=", reduction_operator);
    COMPARATOR_BODY(<=);
}

ScmObj
scm_p_greater(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">", reduction_operator);
    COMPARATOR_BODY(>);
}

ScmObj
scm_p_greater_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">=", reduction_operator);
    COMPARATOR_BODY(>=);
#undef COMPARATOR_BODY
}

ScmObj
scm_p_zerop(ScmObj scm_num)
{
    DECLARE_FUNCTION("zero?", procedure_fixed_1);
    ASSERT_INTP(scm_num);
    return MAKE_BOOL(SCM_INT_VALUE(scm_num) == 0);
}

ScmObj
scm_p_positivep(ScmObj scm_num)
{
    DECLARE_FUNCTION("positive?", procedure_fixed_1);
    ASSERT_INTP(scm_num);
    return MAKE_BOOL(SCM_INT_VALUE(scm_num) > 0);
}

ScmObj
scm_p_negativep(ScmObj scm_num)
{
    DECLARE_FUNCTION("negative?", procedure_fixed_1);
    ASSERT_INTP(scm_num);
    return MAKE_BOOL(SCM_INT_VALUE(scm_num) < 0);
}

ScmObj
scm_p_oddp(ScmObj scm_num)
{
    DECLARE_FUNCTION("odd?", procedure_fixed_1);
    ASSERT_INTP(scm_num);
    return MAKE_BOOL(SCM_INT_VALUE(scm_num) & 0x1);
}

ScmObj
scm_p_evenp(ScmObj scm_num)
{
    DECLARE_FUNCTION("even?", procedure_fixed_1);
    ASSERT_INTP(scm_num);
    return MAKE_BOOL(!(SCM_INT_VALUE(scm_num) & 0x1));
}

ScmObj
scm_p_max(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("max", reduction_operator);
    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ASSERT_INTP(left);
    ASSERT_INTP(right);

    return SCM_INT_VALUE(left) > SCM_INT_VALUE(right) ? left : right;
}

ScmObj
scm_p_min(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("min", reduction_operator);
    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ASSERT_INTP(left);
    ASSERT_INTP(right);

    return SCM_INT_VALUE(left) < SCM_INT_VALUE(right) ? left : right;
}


ScmObj
scm_p_abs(ScmObj scm_num)
{
    int num = 0;
    DECLARE_FUNCTION("abs", procedure_fixed_1);

    ASSERT_INTP(scm_num);

    num = SCM_INT_VALUE(scm_num);

    return (num < 0) ? MAKE_INT(-num) : scm_num;
}

ScmObj
scm_p_quotient(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1 = 0;
    int n2 = 0;
    DECLARE_FUNCTION("quotient", procedure_fixed_2);

    ASSERT_INTP(scm_n1);
    ASSERT_INTP(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT((int)(n1 / n2));
}

ScmObj
scm_p_modulo(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1  = 0;
    int n2  = 0;
    int rem = 0;
    DECLARE_FUNCTION("modulo", procedure_fixed_2);

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

    return MAKE_INT(rem);
}

ScmObj
scm_p_remainder(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1  = 0;
    int n2  = 0;
    DECLARE_FUNCTION("remainder", procedure_fixed_2);

    ASSERT_INTP(scm_n1);
    ASSERT_INTP(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/
ScmObj
scm_p_number2string(ScmObj num, ScmObj args)
{
  char buf[sizeof(int)*CHAR_BIT + 1];
  char *p;
  unsigned int n, r;
  ScmObj radix;
  DECLARE_FUNCTION("number->string", procedure_variadic_1);

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

  return MAKE_STRING_COPYING(p);
}

ScmObj
scm_p_string2number(ScmObj str, ScmObj args)
{
    ScmObj radix = SCM_FALSE;
    int r = 10;
    int num = 0;
    char *first_nondigit = NULL;
    DECLARE_FUNCTION("string->number", procedure_variadic_1);

    ASSERT_STRINGP(str);

    /* r = radix */
    if (!NO_MORE_ARG(args)) {
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

    num = (int)strtol(SCM_STRING_STR(str), &first_nondigit, r);
    if (*first_nondigit)
        ERR("ill-formatted number: %s", SCM_STRING_STR(str));

    return MAKE_INT(num);
}

/*===================================
  R5RS : 6.3 Other data types
===================================*/
/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.1 Booleans
==============================================================================*/
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

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
==============================================================================*/
ScmObj
scm_p_car(ScmObj obj)
{
    DECLARE_FUNCTION("car", procedure_fixed_1);
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    ASSERT_CONSP(obj);

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

    ASSERT_CONSP(obj);

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
    ASSERT_CONSP(pair);

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
    ASSERT_CONSP(pair);

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
    int len = 0;
    DECLARE_FUNCTION("list?", procedure_fixed_1);

    if (NULLP(obj))
        return SCM_TRUE;
    if (!CONSP(obj))
        return SCM_FALSE;

    len = scm_p_c_length(obj);

    return MAKE_BOOL(len != -1);
}

/*
 * Notice
 *
 * This function is ported from Gauche, by Shiro Kawai(shiro@acm.org)
 */
/* FIXME:
 * - Rename to scm_c_length() since it isn't a Scheme procedure
 * - Insert its copyright and license into this file properly
 */
int
scm_p_c_length(ScmObj lst)
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

ScmObj
scm_p_length(ScmObj obj)
{
    int len = scm_p_c_length(obj);
    DECLARE_FUNCTION("length", procedure_fixed_1);

    if (len < 0)
        ERR_OBJ("list required but got", obj);

    return MAKE_INT(len);
}

ScmObj
scm_p_append(ScmObj args)
{
    ScmQueue q;
    ScmObj elm_lst, res;
    DECLARE_FUNCTION("append", procedure_variadic_0);

    if (NULLP(args))
        return SCM_NULL;

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    /* duplicate and merge all but the last argument */
    while (elm_lst = POP_ARG(args), !NO_MORE_ARG(args)) {
        for (; CONSP(elm_lst); elm_lst = CDR(elm_lst))
            SCM_QUEUE_ADD(q, CAR(elm_lst));
        if (!NULLP(elm_lst))
            ERR_OBJ("proper list required but got", elm_lst);
    }
    /* append the last argument */
    SCM_QUEUE_SLOPPY_APPEND(q, elm_lst);

    return res;
}

ScmObj
scm_p_reverse(ScmObj lst)
{
    ScmObj ret_lst  = SCM_NULL;
    DECLARE_FUNCTION("reverse", procedure_fixed_1);

    for (; CONSP(lst); lst = CDR(lst))
        ret_lst = CONS(CAR(lst), ret_lst);

    if (!NULLP(lst))
        ERR_OBJ("got improper list", lst);

    return ret_lst;
}

static ScmObj
scm_p_listtail_internal(ScmObj lst, int k)
{
    while (k--) {
        if (!CONSP(lst))
            return SCM_INVALID;
        lst = CDR(lst);
    }

    return lst;
}

ScmObj
scm_p_list_tail(ScmObj lst, ScmObj scm_k)
{
    ScmObj ret;
    DECLARE_FUNCTION("list-tail", procedure_fixed_2);

    ASSERT_INTP(scm_k);

    ret = scm_p_listtail_internal(lst, SCM_INT_VALUE(scm_k));
    if (EQ(ret, SCM_INVALID))
        ERR_OBJ("out of range or bad list, arglist is", CONS(lst, scm_k));

    return ret;
}

ScmObj
scm_p_list_ref(ScmObj lst, ScmObj scm_k)
{
    ScmObj tail = SCM_NULL;
    DECLARE_FUNCTION("list-ref", procedure_fixed_2);

    ASSERT_INTP(scm_k);

    tail = scm_p_listtail_internal(lst, SCM_INT_VALUE(scm_k));
    if (EQ(tail, SCM_INVALID) || NULLP(tail))
        ERR_OBJ("out of range or bad list, arglist is", CONS(lst, scm_k));

    return CAR(tail);
}

#define MEM_OPERATION_BODY(obj, lst, cmpop)     \
    do {                                        \
        for (; CONSP(lst); lst = CDR(lst))      \
            if (cmpop(obj, CAR(lst)))           \
                return lst;                     \
        return SCM_FALSE;                       \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_memq(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memq", procedure_fixed_2);

    for (; CONSP(lst); lst = CDR(lst))
        if (EQ(obj, CAR(lst)))
            return lst;

#if SCM_STRICT_ARGCHECK
    if (!NULLP(lst))
        ERR_OBJ("invalid list", lst);
#endif

    return SCM_FALSE;
}

ScmObj
scm_p_memv(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("memv", procedure_fixed_2);

    for (; CONSP(lst); lst = CDR(lst))
        if (NFALSEP(scm_p_eqvp(obj, CAR(lst))))
            return lst;

#if SCM_STRICT_ARGCHECK
    if (!NULLP(lst))
        ERR_OBJ("invalid list", lst);
#endif

    return SCM_FALSE;
}

ScmObj
scm_p_member(ScmObj obj, ScmObj lst)
{
    DECLARE_FUNCTION("member", procedure_fixed_2);

    for (; CONSP(lst); lst = CDR(lst))
        if (NFALSEP(scm_p_equalp(obj, CAR(lst))))
            return lst;

#if SCM_STRICT_ARGCHECK
    if (!NULLP(lst))
        ERR_OBJ("invalid list", lst);
#endif

    return SCM_FALSE;
}

ScmObj
scm_p_assq(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;
    DECLARE_FUNCTION("assq", procedure_fixed_2);

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        ASSERT_CONSP(tmpobj);
        if (EQ(CAR(tmpobj), obj))
            return tmpobj;
#else
        if (CONSP(tmpobj) && EQ(CAR(tmpobj), obj))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}

ScmObj
scm_p_assv(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;
    DECLARE_FUNCTION("assv", procedure_fixed_2);

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        ASSERT_CONSP(tmpobj);
        if (NFALSEP(scm_p_eqvp(car, obj)))
            return tmpobj;
#else
        if (CONSP(tmpobj) && NFALSEP(scm_p_eqvp(car, obj)))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}

ScmObj
scm_p_assoc(ScmObj obj, ScmObj alist)
{
    ScmObj tmp_lst = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;
    DECLARE_FUNCTION("assoc", procedure_fixed_2);

    for (tmp_lst = alist; CONSP(tmp_lst); tmp_lst = CDR(tmp_lst)) {
        tmpobj = CAR(tmp_lst);
        car = CAR(tmpobj);
#if SCM_STRICT_R5RS
        ASSERT_CONSP(tmpobj);
        if (NFALSEP(scm_p_equalp(car, obj)))
            return tmpobj;
#else
        if (CONSP(tmpobj) && NFALSEP(scm_p_equalp(car, obj)))
            return tmpobj;
#endif
    }

    return SCM_FALSE;
}


/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.3 Symbols
==============================================================================*/
ScmObj
scm_p_symbolp(ScmObj obj)
{
    DECLARE_FUNCTION("symbol?", procedure_fixed_1);
    return MAKE_BOOL(SYMBOLP(obj));
}

ScmObj
scm_p_symbol2string(ScmObj obj)
{
    DECLARE_FUNCTION("symbol->string", procedure_fixed_1);
    ASSERT_SYMBOLP(obj);
    return MAKE_IMMUTABLE_STRING_COPYING(SCM_SYMBOL_NAME(obj));
}

ScmObj
scm_p_string2symbol(ScmObj str)
{
    DECLARE_FUNCTION("string->symbol", procedure_fixed_1);
    ASSERT_STRINGP(str);
    return scm_intern(SCM_STRING_STR(str));
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.4 Characters
==============================================================================*/
ScmObj
scm_p_charp(ScmObj obj)
{
    DECLARE_FUNCTION("char?", procedure_fixed_1);
    return MAKE_BOOL(CHARP(obj));
}

ScmObj
scm_p_charequalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char=?", procedure_fixed_2);

    ASSERT_CHARP(ch1);
    ASSERT_CHARP(ch2);

    return MAKE_BOOL(SCM_CHAR_VALUE(ch1) == SCM_CHAR_VALUE(ch2));
}

ScmObj
scm_p_char_alphabeticp(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-alphabetic?", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);

    return MAKE_BOOL(isascii(ch) && isalpha(ch));
}

ScmObj
scm_p_char_numericp(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-numeric?", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);

    return MAKE_BOOL(isascii(ch) && isdigit(ch));
}

ScmObj
scm_p_char_whitespacep(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-whitespace?", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);

    return MAKE_BOOL(isascii(ch) && isspace(ch));
}

ScmObj
scm_p_char_upper_casep(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-upper-case?", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);

    return MAKE_BOOL(isascii(ch) && isupper(ch));
}

ScmObj
scm_p_char_lower_casep(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-lower-case?", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);

    return MAKE_BOOL(isascii(ch) && islower(ch));
}

ScmObj
scm_p_char2integer(ScmObj obj)
{
    DECLARE_FUNCTION("char->integer", procedure_fixed_1);

    ASSERT_CHARP(obj);

    return MAKE_INT(SCM_CHAR_VALUE(obj));
}

ScmObj
scm_p_integer2char(ScmObj obj)
{
    int val;
    char buf[SCM_MB_MAX_LEN + sizeof("")];
    DECLARE_FUNCTION("integer->char", procedure_fixed_1);

    ASSERT_INTP(obj);

    val = SCM_INT_VALUE(obj);
    if (!SCM_CHARCODEC_INT2STR(scm_current_char_codec, buf, val, SCM_MB_STATELESS))
        ERR_OBJ("invalid char value", obj);
    return MAKE_CHAR(val);
}

ScmObj
scm_p_char_upcase(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-upcase", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);
    if (isascii(ch))
        SCM_CHAR_SET_VALUE(obj, toupper(ch));

    return obj;
}

ScmObj
scm_p_char_downcase(ScmObj obj)
{
    int ch;
    DECLARE_FUNCTION("char-downcase", procedure_fixed_1);

    ASSERT_CHARP(obj);

    ch = SCM_CHAR_VALUE(obj);
    if (isascii(ch))
        SCM_CHAR_SET_VALUE(obj, tolower(ch));

    return obj;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.5 Strings
==============================================================================*/
ScmObj
scm_p_stringp(ScmObj obj)
{
    DECLARE_FUNCTION("string?", procedure_fixed_1);
    return MAKE_BOOL(STRINGP(obj));
}

ScmObj
scm_p_make_string(ScmObj length, ScmObj args)
{
    int filler_val, len, i;
    ScmObj filler = SCM_FALSE;
    ScmObj sport  = SCM_FALSE;
    DECLARE_FUNCTION("make-string", procedure_variadic_1);

    ASSERT_INTP(length);
    len = SCM_INT_VALUE(length);
    if (len == 0)
        return MAKE_STRING_COPYING("");
    if (len < 0)
        ERR_OBJ("length must be a positive integer", length);

    /* extract filler */
    if (NO_MORE_ARG(args)) {
        filler_val = ' ';
    } else {
        filler = POP_ARG(args);
        ASSERT_NO_MORE_ARG(args);
        ASSERT_CHARP(filler);
        filler_val = SCM_CHAR_VALUE(filler);
    }

    /* TODO: make efficient */
    /* fill string (multibyte-ready) */
    sport = scm_p_srfi6_open_output_string();
    for (i = 0; i < len; i++) {
        scm_port_put_char(sport, filler_val);
    }

    return scm_p_srfi6_get_output_string(sport);
}

ScmObj
scm_p_string(ScmObj args)
{
    DECLARE_FUNCTION("string", procedure_variadic_0);
    return scm_p_list2string(args);
}

ScmObj
scm_p_string_length(ScmObj str)
{
    DECLARE_FUNCTION("string-length", procedure_fixed_1);
    ASSERT_STRINGP(str);
    return MAKE_INT(scm_mb_bare_c_strlen(SCM_STRING_STR(str)));
}

ScmObj
scm_p_string_ref(ScmObj str, ScmObj k)
{
    int   c_index = 0;
    int   ch;
    ScmMultibyteString mbs;
    DECLARE_FUNCTION("string-ref", procedure_fixed_2);

    ASSERT_STRINGP(str);
    ASSERT_INTP(k);

    c_index = SCM_INT_VALUE(k);
    if (c_index < 0 || SCM_STRING_LEN(str) <= c_index)
        ERR_OBJ("index out of range", k);

    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, SCM_STRING_STR(str));
    SCM_MBS_SET_SIZE(mbs, strlen(SCM_STRING_STR(str)));
    mbs = scm_mb_strref(mbs, c_index);

    /* FIXME: support stateful encoding */
    ch = SCM_CHARCODEC_STR2INT(scm_current_char_codec, SCM_MBS_GET_STR(mbs),
                               SCM_MBS_GET_SIZE(mbs), SCM_MB_STATELESS);
    if (ch == EOF)
        ERR("string-ref: invalid char sequence");

    return MAKE_CHAR(ch);
}

ScmObj
scm_p_string_setd(ScmObj str, ScmObj k, ScmObj ch)
{
    int   c_start_index = 0;
    int   prefix_size = 0;
    int   newch_size = 0;
    int   postfix_size  = 0;
    int   total_size = 0;
    char *new_str  = NULL;
    ScmMultibyteString mbs;
    const char *string_str   = NULL;
    char new_ch_str[SCM_MB_MAX_LEN + sizeof("")];
    const char *next;
    DECLARE_FUNCTION("string-set!", procedure_fixed_3);

    ASSERT_STRINGP(str);
    ASSERT_MUTABLEP(str);
    ASSERT_INTP(k);
    ASSERT_CHARP(ch);

    /* get indexes */
    c_start_index = SCM_INT_VALUE(k);
    string_str    = SCM_STRING_STR(str);
    if (c_start_index < 0 || SCM_STRING_LEN(str) <= c_start_index)
        ERR_OBJ("index out of range", k);
    /* FIXME: can string_str be NULL at this point or not? */
    if (!string_str) string_str = "";

    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, string_str);
    SCM_MBS_SET_SIZE(mbs, strlen(string_str));
    mbs = scm_mb_strref(mbs, c_start_index);

    /* FIXME: support stateful encoding */
    next = SCM_CHARCODEC_INT2STR(scm_current_char_codec, new_ch_str,
                                 SCM_CHAR_VALUE(ch), SCM_MB_STATELESS);
    if (!next)
        ERR_OBJ("invalid char in", str);

    /* calculate total size */
    prefix_size = SCM_MBS_GET_STR(mbs) - string_str;
    newch_size  = next - new_ch_str;
    postfix_size  = strlen(SCM_MBS_GET_STR(mbs) + SCM_MBS_GET_SIZE(mbs));
    total_size = prefix_size + newch_size + postfix_size;

    /* copy each part */
    new_str = scm_malloc(total_size + 1);
    memcpy(new_str, string_str, prefix_size);
    memcpy(new_str+prefix_size, new_ch_str, newch_size);
    memcpy(new_str+prefix_size+newch_size,
           SCM_MBS_GET_STR(mbs)+SCM_MBS_GET_SIZE(mbs), postfix_size);

    if (SCM_STRING_STR(str))
        free(SCM_STRING_STR(str));

    SCM_STRING_SET_STR(str, new_str);

    return str;
}

ScmObj
scm_p_stringequalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string=?", procedure_fixed_2);

    ASSERT_STRINGP(str1);
    ASSERT_STRINGP(str2);

    if (strcmp(SCM_STRING_STR(str1), SCM_STRING_STR(str2)) == 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj
scm_p_substring(ScmObj str, ScmObj start, ScmObj end)
{
    int   c_start_index = 0;
    int   c_end_index   = 0;
    char *new_str = NULL;
    ScmMultibyteString mbs;
    const char *string_str   = NULL;
    DECLARE_FUNCTION("substring", procedure_fixed_3);

    ASSERT_STRINGP(str);
    ASSERT_MUTABLEP(str);
    ASSERT_INTP(start);
    ASSERT_INTP(end);

    /* get start_ptr and end_ptr */
    c_start_index = SCM_INT_VALUE(start);
    c_end_index   = SCM_INT_VALUE(end);

    /* sanity check */
    if (c_start_index < 0 || SCM_STRING_LEN(str) <= c_start_index)
        ERR_OBJ("start index out of range", start);
    if (c_end_index < 0 || SCM_STRING_LEN(str) <= c_end_index)
        ERR_OBJ("end index out of range", end);
    if (c_start_index > c_end_index)
        ERR("substring: start index is greater than end index.");

    string_str = SCM_STRING_STR(str);
    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, string_str);
    SCM_MBS_SET_SIZE(mbs, strlen(string_str));
    mbs = scm_mb_substring(mbs, c_start_index, c_end_index - c_start_index);

    /* copy from start_ptr to end_ptr */
    new_str = scm_malloc(SCM_MBS_GET_SIZE(mbs) + 1);
    memcpy(new_str, SCM_MBS_GET_STR(mbs), SCM_MBS_GET_SIZE(mbs));
    new_str[SCM_MBS_GET_SIZE(mbs)] = 0;

    return MAKE_STRING(new_str);
}

ScmObj
scm_p_string_append(ScmObj args)
{
    /* FIXME: transition to new arg extraction mechanism incomplete. */
    int total_size = 0;
    int total_len  = 0;
    ScmObj strlst  = SCM_FALSE;
    ScmObj obj     = SCM_FALSE;
    char  *new_str = NULL;
    char  *p       = NULL;
    DECLARE_FUNCTION("string-append", procedure_variadic_0);

    if (NO_MORE_ARG(args))
        return MAKE_STRING_COPYING("");

    /* count total size of the new string */
    for (strlst = args; !NULLP(strlst); strlst = CDR(strlst)) {
        obj = CAR(strlst);
        ASSERT_STRINGP(obj);

        total_size += strlen(SCM_STRING_STR(obj));
        total_len  += SCM_STRING_LEN(obj);
    }

    new_str = scm_malloc(total_size + 1);

    /* copy string by string */
    p = new_str;
    for (strlst = args; !NULLP(strlst); strlst = CDR(strlst)) {
        obj = CAR(strlst);

        strcpy(p, SCM_STRING_STR(obj));
        p += strlen(SCM_STRING_STR(obj));
    }

    return MAKE_STRING(new_str);
}

ScmObj
scm_p_string2list(ScmObj str)
{
    ScmQueue q;
    ScmObj res;
    int ch;
    ScmMultibyteString mbs;
    ScmMultibyteCharInfo mbc;
    ScmMultibyteState state;
    DECLARE_FUNCTION("string->list", procedure_fixed_1);

    ASSERT_STRINGP(str);

    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, SCM_STRING_STR(str));
    SCM_MBS_SET_SIZE(mbs, strlen(SCM_STRING_STR(str)));

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    while (SCM_MBS_GET_SIZE(mbs)) {
        state = SCM_MBS_GET_STATE(mbs);
        mbc = SCM_CHARCODEC_SCAN_CHAR(scm_current_char_codec, mbs);
        if (SCM_MBCINFO_ERRORP(mbc) || SCM_MBCINFO_INCOMPLETEP(mbc))
            ERR("string->list: invalid char sequence");
        ch = SCM_CHARCODEC_STR2INT(scm_current_char_codec,
                                   SCM_MBS_GET_STR(mbs),
                                   SCM_MBCINFO_GET_SIZE(mbc),
                                   state);
        if (ch == EOF)
            ERR("string->list: invalid char sequence");

        SCM_QUEUE_ADD(q, MAKE_CHAR(ch));
        SCM_MBS_SKIP_CHAR(mbs, mbc);
    }

    return res;
}

ScmObj
scm_p_list2string(ScmObj lst)
{
    ScmObj rest, ch, sport;
    DECLARE_FUNCTION("list->string", procedure_fixed_1);

    ASSERT_LISTP(lst);

    if (NULLP(lst))
        return MAKE_STRING_COPYING("");

    /* TODO: make efficient */
    sport = scm_p_srfi6_open_output_string();
    for (rest = lst; CONSP(rest); rest = CDR(rest)) {
        ch = CAR(rest);
        ASSERT_CHARP(ch);
        scm_port_put_char(sport, SCM_CHAR_VALUE(ch));
    }
    if (!NULLP(rest))
        ERR_OBJ("invalid char list", lst);

    return scm_p_srfi6_get_output_string(sport);
}

ScmObj
scm_p_string_copy(ScmObj str)
{
    DECLARE_FUNCTION("string-copy", procedure_fixed_1);
    ASSERT_STRINGP(str);
    return MAKE_STRING_COPYING(SCM_STRING_STR(str));
}

ScmObj
scm_p_string_filld(ScmObj str, ScmObj ch)
{
    int  char_size = 0;
    int  str_len   = 0;
    char *new_str  = NULL;
    char *p        = NULL;
    char ch_str[SCM_MB_MAX_LEN + sizeof("")];
    const char *next;
    DECLARE_FUNCTION("string-fill!", procedure_fixed_2);

    ASSERT_STRINGP(str);
    ASSERT_MUTABLEP(str);
    ASSERT_CHARP(ch);

    str_len = SCM_STRING_LEN(str);
    if (str_len == 0)
        return MAKE_STRING_COPYING("");

    /* FIXME: support stateful encoding */
    next = SCM_CHARCODEC_INT2STR(scm_current_char_codec, ch_str,
                                 SCM_CHAR_VALUE(ch), SCM_MB_STATELESS);
    if (!next)
        ERR_OBJ("invalid char in", str);

    /* create new str */
    char_size = next - ch_str;
    new_str = scm_realloc(SCM_STRING_STR(str), str_len * char_size + 1);
    for (p = new_str; p < &new_str[char_size * str_len]; p += char_size) {
        strcpy(p, ch_str);
    }

    SCM_STRING_SET_STR(str, new_str);

    return str;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.6 Vectors
==============================================================================*/
ScmObj
scm_p_vectorp(ScmObj obj)
{
    DECLARE_FUNCTION("vector?", procedure_fixed_1);
    return MAKE_BOOL(VECTORP(obj));
}

ScmObj
scm_p_make_vector(ScmObj vector_len, ScmObj args)
{
    ScmObj *vec    = NULL;
    ScmObj  filler = SCM_FALSE;
    int len = 0;
    int i   = 0;
    DECLARE_FUNCTION("make-vector", procedure_variadic_1);

    ASSERT_INTP(vector_len);

    /* sanity check */
    if (SCM_INT_VALUE(vector_len) < 0)
        ERR_OBJ("length must be a positive integer", vector_len);

    /* allocate vector */
    len = SCM_INT_VALUE(vector_len);
    vec = scm_malloc(sizeof(ScmObj) * len);

    /* fill vector */
    filler = SCM_UNDEF;
    if (!NULLP(args))
        filler = CAR(args);

    for (i = 0; i < len; i++)
        vec[i] = filler;

    return MAKE_VECTOR(vec, len);
}

ScmObj
scm_p_vector(ScmObj args)
{
    int len, i;
    ScmObj *vec;
    DECLARE_FUNCTION("vector", procedure_variadic_0);

    len = SCM_INT_VALUE(scm_p_length(args));
    vec = scm_malloc(sizeof(ScmObj) * len);

    for (i = 0; i < len; i++)
        vec[i] = POP_ARG(args);

    return MAKE_VECTOR(vec, len);
}

ScmObj
scm_p_vector_length(ScmObj vec)
{
    DECLARE_FUNCTION("vector-length", procedure_fixed_1);

    ASSERT_VECTORP(vec);
    return MAKE_INT(SCM_VECTOR_LEN(vec));
}

ScmObj
scm_p_vector_ref(ScmObj vec, ScmObj scm_k)
{
    int k;
    DECLARE_FUNCTION("vector-ref", procedure_fixed_2);

    ASSERT_VECTORP(vec);
    ASSERT_INTP(scm_k);

    k = SCM_INT_VALUE(scm_k);

    if (!SCM_VECTOR_VALID_INDEXP(vec, k))
        ERR_OBJ("index out of range", scm_k);

    return SCM_VECTOR_VEC(vec)[k];
}

ScmObj
scm_p_vector_setd(ScmObj vec, ScmObj scm_k, ScmObj obj)
{
    int k;
    DECLARE_FUNCTION("vector-set!", procedure_fixed_3);

    ASSERT_VECTORP(vec);
    ASSERT_INTP(scm_k);

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
    ScmObj res;
    ScmObj *v;
    int len, i;
    DECLARE_FUNCTION("vector->list", procedure_fixed_1);

    ASSERT_VECTORP(vec);

    v = SCM_VECTOR_VEC(vec);
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
    ScmObj  scm_len = SCM_NULL;
    ScmObj *v       = NULL;
    int c_len = 0;
    int i = 0;
    DECLARE_FUNCTION("list->vector", procedure_fixed_1);

    /* TOOD : canbe optimized. scanning list many times */
    if (FALSEP(scm_p_listp(lst)))
        ERR_OBJ("list required but got", lst);

    scm_len = scm_p_length(lst);
    c_len   = SCM_INT_VALUE(scm_len);
    v       = scm_malloc(sizeof(ScmObj) * c_len);
    for (i = 0; i < c_len; i++) {
        v[i] = CAR(lst);
        lst  = CDR(lst);
    }

    return MAKE_VECTOR(v, c_len);
}

ScmObj
scm_p_vector_filld(ScmObj vec, ScmObj fill)
{
    int c_len = 0;
    int i = 0;
    DECLARE_FUNCTION("vector-fill!", procedure_fixed_2);

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
    while (!NO_MORE_ARG(lst)) {
        elm = POP_ARG(lst);
        SCM_QUEUE_ADD(q, scm_call(proc, LIST_1(elm)));
    }

    return res;
}

static ScmObj
map_multiple_args(ScmObj proc, ScmObj args)
{
    ScmQueue resq, argq;
    ScmObj res, map_args, rest_args, arg;
    DECLARE_INTERNAL_FUNCTION("map");

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(resq, res);
    for (;;) {
        /* slice args */
        map_args = SCM_NULL;
        SCM_QUEUE_POINT_TO(argq, map_args);
        for (rest_args = args; CONSP(rest_args); rest_args = CDR(rest_args)) {
            arg = CAR(rest_args);
            if (NULLP(arg))
                return res;
            if (CONSP(arg))
                SCM_QUEUE_ADD(argq, CAR(arg));
            else
                ERR_OBJ("invalid argument", arg);
            /* pop destructively */
            SET_CAR(rest_args, CDR(arg));
        }
        if (!NULLP(rest_args))
            ERR_OBJ("proper list required but got", args);

        SCM_QUEUE_ADD(resq, scm_call(proc, map_args));
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

    ASSERT_CLOSUREP(closure);

    return scm_call(closure, SCM_NULL);
}

ScmObj
scm_p_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state)
{
    DECLARE_FUNCTION("call-with-current-continuation", procedure_fixed_tailrec_1);

    ASSERT_PROCEDUREP(proc);

    return scm_call_with_current_continuation(proc, eval_state);
}

ScmObj
scm_p_values(ScmObj args)
{
    DECLARE_FUNCTION("values", procedure_variadic_0);
    /* Values with one arg must return something that fits an ordinary
     * continuation. */
    if (CONSP(args) && NULLP(CDR(args)))
        return CAR(args);

    /* Otherwise, we'll return the values in a packet. */
    return SCM_MAKE_VALUEPACKET(args);
}

ScmObj scm_p_call_with_values(ScmObj producer, ScmObj consumer,
                              ScmEvalState *eval_state)
{
    ScmObj vals;
    DECLARE_FUNCTION("call-with-values", procedure_fixed_tailrec_2);

    ASSERT_PROCEDUREP(producer);
    ASSERT_PROCEDUREP(consumer);

    vals = scm_call(producer, SCM_NULL);

    if (!VALUEPACKETP(vals)) {
        /* got back a single value */
        vals = CONS(vals, SCM_NULL);
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

    ASSERT_PROCEDUREP(before);
    ASSERT_PROCEDUREP(thunk);
    ASSERT_PROCEDUREP(after);

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
