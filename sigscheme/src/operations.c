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

#include "config.h"
#include "config-nonstd-string.h"
/* FIXME: remove this for direct inclusion of operations-srfi6.c and
 * strport.c */
#include "config-asprintf.h"

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
#define EQVP(a, b)   (NFALSEP(scm_p_eqvp((a), (b))))
#define EQUALP(a, b) (NFALSEP(scm_p_equalp((a), (b))))
#define STRING_EQUALP(str1, str2)                                            \
    (EQ((str1), (str2))                                                      \
     || (SCM_STRING_LEN(str1) == SCM_STRING_LEN(str2)  /* rough rejection */ \
         && strcmp(SCM_STRING_STR(str1), SCM_STRING_STR(str2)) == 0))
#define STRING_CMP(str1, str2)                                               \
    (string_cmp(SCM_MANGLE(name), (str1), (str2), scm_false))
#define STRING_CI_CMP(str1, str2)                                            \
    (string_cmp(SCM_MANGLE(name), (str1), (str2), scm_true))

/*
 * SigScheme's case-insensitive comparison conforms to the foldcase'ed
 * comparison described in SRFI-75 and SRFI-13, although R5RS does not specify
 * comparison between alphabetic and non-alphabetic char.
 *
 * This specification is needed to produce natural result on sort functions
 * with these case-insensitive predicates as comparator.
 *
 *   (a-sort '(#\a #\c #\B #\D #\1 #\[ #\$ #\_) char-ci<?)
 *     => (#\$ #\1 #\a #\B #\c #\D #\[ #\_)  ;; the "natural result"
 *
 *     => (#\$ #\1 #\B #\D #\[ #\_ #\a #\c)  ;; "unnatural result"
 *
 * See also:
 *
 *   - Description around 'char-foldcase' in SRFI-75
 *   - "Case mapping and case-folding" and "Comparison" section of SRFI-13
 */
/* FIXME: support SRFI-75 */
#define ICHAR_DOWNCASE(c) ((isascii((int)(c))) ? tolower((int)(c)) : (c))
#define ICHAR_UPCASE(c)   ((isascii((int)(c))) ? toupper((int)(c)) : (c))
/* foldcase for case-insensitive character comparison is done by downcase as
 * described in SRFI-75. Although SRFI-13 expects (char-downcase (char-upcase
 * c)), this implementation is sufficient for ASCII range. */
#define ICHAR_FOLDCASE(c) (ICHAR_DOWNCASE(c))

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static int prepare_radix(const char *funcname, ScmObj args);
static ScmObj list_tail(ScmObj lst, scm_int_t k);
#if (!HAVE_STRCASECMP && !SCM_USE_MULTIBYTE_CHAR)
static int strcasecmp(const char *s1, const char *s2);
#endif
static int string_cmp(const char *funcname,
                      ScmObj str1, ScmObj str2, scm_bool case_insensitive);
static ScmObj map_single_arg(ScmObj proc, ScmObj args);
static ScmObj map_multiple_args(ScmObj proc, ScmObj args);

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  R5RS : 6.1 Equivalence predicates
==============================================================================*/
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
    scm_int_t result;
    DECLARE_FUNCTION("+", reduction_operator);

    result = 0;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        result += SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        SCM_ASSERT(scm_false);
    }

    return MAKE_INT(result);
}

ScmObj
scm_p_multiply(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result;
    DECLARE_FUNCTION("*", reduction_operator);

    result = 1;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        result *= SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        SCM_ASSERT(scm_false);
    }

    return MAKE_INT(result);
}

ScmObj
scm_p_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result;
    DECLARE_FUNCTION("-", reduction_operator);

    result = 0;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        result -= SCM_INT_VALUE(right);
        break;

    case SCM_REDUCE_0:
        ERR("at least 1 argument required");
    default:
        SCM_ASSERT(scm_false);
    }
    return MAKE_INT(result);
}

ScmObj
scm_p_divide(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result;
    DECLARE_FUNCTION("/", reduction_operator);

    result = 1;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        if (SCM_INT_VALUE(right) == 0)
            ERR("division by zero");
        result /= SCM_INT_VALUE(right);
        break;
    case SCM_REDUCE_0:
        ERR("at least 1 argument required");
    default:
        SCM_ASSERT(scm_false);
    }
    return MAKE_INT(result);
}

ScmObj
scm_p_numberp(ScmObj obj)
{
    DECLARE_FUNCTION("number?", procedure_fixed_1);

    return MAKE_BOOL(NUMBERP(obj));
}

ScmObj
scm_p_integerp(ScmObj obj)
{
    DECLARE_FUNCTION("integer?", procedure_fixed_1);

    return MAKE_BOOL(INTP(obj));
}

#define COMPARATOR_BODY(op)                                                  \
    switch (*state) {                                                        \
    case SCM_REDUCE_0:                                                       \
    case SCM_REDUCE_1:                                                       \
        ERR("at least 2 arguments required");                                \
    case SCM_REDUCE_PARTWAY:                                                 \
    case SCM_REDUCE_LAST:                                                    \
        ENSURE_INT(left);                                                    \
        ENSURE_INT(right);                                                   \
        if (SCM_INT_VALUE(left) op SCM_INT_VALUE(right))                     \
            return *state == SCM_REDUCE_LAST ? SCM_TRUE : right;             \
        *state = SCM_REDUCE_STOP;                                            \
        return SCM_FALSE;                                                    \
    default:                                                                 \
        SCM_ASSERT(scm_false);                                               \
    }                                                                        \
    return SCM_INVALID

ScmObj
scm_p_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("=", reduction_operator);

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
}

#undef COMPARATOR_BODY

ScmObj
scm_p_zerop(ScmObj n)
{
    DECLARE_FUNCTION("zero?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) == 0);
}

ScmObj
scm_p_positivep(ScmObj n)
{
    DECLARE_FUNCTION("positive?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) > 0);
}

ScmObj
scm_p_negativep(ScmObj n)
{
    DECLARE_FUNCTION("negative?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) < 0);
}

ScmObj
scm_p_oddp(ScmObj n)
{
    DECLARE_FUNCTION("odd?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) & 0x1);
}

ScmObj
scm_p_evenp(ScmObj n)
{
    DECLARE_FUNCTION("even?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(!(SCM_INT_VALUE(n) & 0x1));
}

ScmObj
scm_p_max(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("max", reduction_operator);

    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ENSURE_INT(left);
    ENSURE_INT(right);

    return (SCM_INT_VALUE(left) > SCM_INT_VALUE(right)) ? left : right;
}

ScmObj
scm_p_min(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("min", reduction_operator);

    if (*state == SCM_REDUCE_0)
        ERR("at least 1 argument required");
    ENSURE_INT(left);
    ENSURE_INT(right);

    return (SCM_INT_VALUE(left) < SCM_INT_VALUE(right)) ? left : right;
}


ScmObj
scm_p_abs(ScmObj scm_n)
{
    scm_int_t n;
    DECLARE_FUNCTION("abs", procedure_fixed_1);

    ENSURE_INT(scm_n);

    n = SCM_INT_VALUE(scm_n);

    return (n < 0) ? MAKE_INT(-n) : scm_n;
}

ScmObj
scm_p_quotient(ScmObj scm_n1, ScmObj scm_n2)
{
    scm_int_t n1, n2;
    DECLARE_FUNCTION("quotient", procedure_fixed_2);

    ENSURE_INT(scm_n1);
    ENSURE_INT(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT((int)(n1 / n2));
}

ScmObj
scm_p_modulo(ScmObj scm_n1, ScmObj scm_n2)
{
    scm_int_t n1, n2, rem;
    DECLARE_FUNCTION("modulo", procedure_fixed_2);

    ENSURE_INT(scm_n1);
    ENSURE_INT(scm_n2);

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
    scm_int_t n1, n2;
    DECLARE_FUNCTION("remainder", procedure_fixed_2);

    ENSURE_INT(scm_n1);
    ENSURE_INT(scm_n2);

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/

static int
prepare_radix(const char *funcname, ScmObj args)
{
    ScmObj radix;
    int r;
    DECLARE_INTERNAL_FUNCTION("(internal)");

    ASSERT_PROPER_ARG_LIST(args);

    /* dirty hack to replace internal function name */
    SCM_MANGLE(name) = funcname;

    if (NULLP(args)) {
        r = 10;
    } else {
        radix = POP(args);
        ASSERT_NO_MORE_ARG(args);
        ENSURE_INT(radix);
        r = SCM_INT_VALUE(radix);
        if (!(r == 2 || r == 8 || r == 10 || r == 16))
            ERR_OBJ("invalid radix", radix);
    }

    return r;
}

ScmObj
scm_p_number2string(ScmObj num, ScmObj args)
{
  char buf[sizeof(scm_int_t) * CHAR_BIT + sizeof("")];
  char *p;
  const char *end;
  scm_int_t n, digit;
  int r;
  scm_bool neg;
  DECLARE_FUNCTION("number->string", procedure_variadic_1);

  ENSURE_INT(num);

  n = SCM_INT_VALUE(num);
  neg = (n < 0);
  n = (neg) ? -n : n;
  r = prepare_radix(SCM_MANGLE(name), args);

  end = p = &buf[sizeof(buf) - 1];
  *p = '\0';

  do {
      digit = n % r;
      *--p = (digit <= 9) ? '0' + digit : 'A' + digit - 10;
  } while (n /= r);
  if (neg)
    *--p = '-';

  return MAKE_STRING_COPYING(p, end - p);
}

ScmObj
scm_p_string2number(ScmObj str, ScmObj args)
{
    scm_int_t n;
    int r;
    char *end;
    const char *c_str;
    scm_bool empty_strp;
    DECLARE_FUNCTION("string->number", procedure_variadic_1);

    ENSURE_STRING(str);

    /* R5RS:
     *
     * - If string is not a syntactically valid notation for a number, then
     *   `string->number' returns #f.
     *
     * - `String->number' is permitted to return #f whenever string contains an
     *   explicit radix prefix.
     *
     * - If all numbers supported by an implementation are real, then
     *   `string->number' is permitted to return #f whenever string uses the
     *   polar or rectangular notations for complex numbers.
     *
     * - If all numbers are integers, then `string->number' may return #f
     *   whenever the fractional notation is used.
     *
     * - If all numbers are exact, then `string->number' may return #f whenever
     *   an exponent marker or explicit exactness prefix is used, or if a #
     *   appears in place of a digit.
     *
     * - If all inexact numbers are integers, then `string->number' may return
     *   #f whenever a decimal point is used.
     */

    c_str = SCM_STRING_STR(str);
    r = prepare_radix(SCM_MANGLE(name), args);
    n = (scm_int_t)strtol(c_str, &end, r);

    empty_strp = (end == c_str);  /* apply the first rule above */
    return (empty_strp || *end) ? SCM_FALSE : MAKE_INT(n);
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
scm_p_char_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char=?", procedure_fixed_2);

    ENSURE_CHAR(ch1);
    ENSURE_CHAR(ch2);

#if SCM_HAS_IMMEDIATE_CHAR_ONLY
    return MAKE_BOOL(EQ(ch1, ch2));
#else
    return MAKE_BOOL(SCM_CHAR_VALUE(ch1) == SCM_CHAR_VALUE(ch2));
#endif
}

#define CHAR_CMP_BODY(op, ch1, ch2)                                          \
    do {                                                                     \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        return MAKE_BOOL(SCM_CHAR_VALUE(ch1) op SCM_CHAR_VALUE(ch2));        \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_char_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<?", procedure_fixed_2);

    CHAR_CMP_BODY(<, ch1, ch2);
}

ScmObj
scm_p_char_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>?", procedure_fixed_2);

    CHAR_CMP_BODY(>, ch1, ch2);
}

ScmObj
scm_p_char_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char<=?", procedure_fixed_2);

    CHAR_CMP_BODY(<=, ch1, ch2);
}

ScmObj
scm_p_char_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char>=?", procedure_fixed_2);

    CHAR_CMP_BODY(>=, ch1, ch2);
}

#define CHAR_CI_CMP_BODY(op, ch1, ch2)                                       \
    do {                                                                     \
        scm_ichar_t val1, val2;                                              \
                                                                             \
        ENSURE_CHAR(ch1);                                                    \
        ENSURE_CHAR(ch2);                                                    \
                                                                             \
        val1 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch1));                          \
        val2 = ICHAR_FOLDCASE(SCM_CHAR_VALUE(ch2));                          \
                                                                             \
        return MAKE_BOOL(val1 op val2);                                      \
    } while (/* CONSTCOND */ 0)

ScmObj
scm_p_char_ci_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(==, ch1, ch2);
}

ScmObj
scm_p_char_ci_lessp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<, ch1, ch2);
}

ScmObj
scm_p_char_ci_greaterp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>, ch1, ch2);
}

ScmObj
scm_p_char_ci_less_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci<=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(<=, ch1, ch2);
}

ScmObj
scm_p_char_ci_greater_equalp(ScmObj ch1, ScmObj ch2)
{
    DECLARE_FUNCTION("char-ci>=?", procedure_fixed_2);

    CHAR_CI_CMP_BODY(>=, ch1, ch2);
}

#undef CHAR_CMP_BODY
#undef CHAR_CI_CMP_BODY

ScmObj
scm_p_char_alphabeticp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-alphabetic?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(isascii(val) && isalpha(val));
}

ScmObj
scm_p_char_numericp(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-numeric?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(isascii(val) && isdigit(val));
}

ScmObj
scm_p_char_whitespacep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-whitespace?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(isascii(val) && isspace(val));
}

ScmObj
scm_p_char_upper_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upper-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(isascii(val) && isupper(val));
}

ScmObj
scm_p_char_lower_casep(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-lower-case?", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);

    return MAKE_BOOL(isascii(val) && islower(val));
}

ScmObj
scm_p_char2integer(ScmObj ch)
{
    DECLARE_FUNCTION("char->integer", procedure_fixed_1);

    ENSURE_CHAR(ch);

    return MAKE_INT(SCM_CHAR_VALUE(ch));
}

ScmObj
scm_p_integer2char(ScmObj n)
{
    scm_int_t val;
    DECLARE_FUNCTION("integer->char", procedure_fixed_1);

    ENSURE_INT(n);

    val = SCM_INT_VALUE(n);
#if SCM_USE_MULTIBYTE_CHAR
    if (!SCM_CHARCODEC_CHAR_LEN(scm_current_char_codec, val))
#else
    if (!isascii(val))
#endif
        ERR_OBJ("invalid char value", n);

    return MAKE_CHAR(val);
}

ScmObj
scm_p_char_upcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-upcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_UPCASE(val));

    return ch;
}

ScmObj
scm_p_char_downcase(ScmObj ch)
{
    scm_ichar_t val;
    DECLARE_FUNCTION("char-downcase", procedure_fixed_1);

    ENSURE_CHAR(ch);

    val = SCM_CHAR_VALUE(ch);
    SCM_CHAR_SET_VALUE(ch, ICHAR_DOWNCASE(val));

    return ch;
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
    ScmObj filler;
    scm_ichar_t filler_val;
    size_t len;
    int ch_len;
    char *str, *dst;
#if SCM_USE_MULTIBYTE_CHAR
    const char *next;
    char ch_str[SCM_MB_MAX_LEN + sizeof("")];
#endif
    DECLARE_FUNCTION("make-string", procedure_variadic_1);

    ENSURE_STATELESS_CODEC(scm_current_char_codec);
    ENSURE_INT(length);
    len = SCM_INT_VALUE(length);
    if (len == 0)
        return MAKE_STRING_COPYING("", 0);
    if (len < 0)
        ERR_OBJ("length must be a positive integer", length);

    /* extract filler */
    if (NULLP(args)) {
        filler_val = ' ';
        ch_len = sizeof((char)' ');
    } else {
        filler = POP(args);
        ASSERT_NO_MORE_ARG(args);
        ENSURE_CHAR(filler);
        filler_val = SCM_CHAR_VALUE(filler);
#if SCM_USE_MULTIBYTE_CHAR
        ch_len = SCM_CHARCODEC_CHAR_LEN(scm_current_char_codec, filler_val);
#endif
    }
#if !SCM_USE_NULL_CAPABLE_STRING
    if (filler_val == '\0')
        ERR("make-string: " SCM_ERRMSG_NULL_IN_STRING);
#endif

#if SCM_USE_MULTIBYTE_CHAR
    next = SCM_CHARCODEC_INT2STR(scm_current_char_codec, ch_str, filler_val,
                                 SCM_MB_STATELESS);
    if (!next)
        ERR("make-string: invalid char 0x%x for encoding %s",
            (int)filler_val, SCM_CHARCODEC_ENCODING(scm_current_char_codec));

    str = scm_malloc(ch_len * len + sizeof(""));
    for (dst = str; dst < &str[ch_len * len]; dst += ch_len)
        memcpy(dst, ch_str, ch_len);
#else
    SCM_ASSERT(isascii(filler_val));
    str = scm_malloc(len + sizeof(""));
    for (dst = str; dst < &str[len];)
        *dst++ = filler_val;
#endif
    *dst = '\0';

    return MAKE_STRING(str, len);
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
    scm_int_t len;
    DECLARE_FUNCTION("string-length", procedure_fixed_1);

    ENSURE_STRING(str);

#if SCM_USE_MULTIBYTE_CHAR
    len = scm_mb_bare_c_strlen(scm_current_char_codec, SCM_STRING_STR(str));
#else
    len = SCM_STRING_LEN(str);
#endif

    return MAKE_INT(len);
}

ScmObj
scm_p_string_ref(ScmObj str, ScmObj k)
{
    scm_int_t idx;
    scm_ichar_t ch;
#if SCM_USE_MULTIBYTE_CHAR
    ScmMultibyteString mbs;
#endif
    DECLARE_FUNCTION("string-ref", procedure_fixed_2);

    ENSURE_STRING(str);
    ENSURE_INT(k);

    idx = SCM_INT_VALUE(k);
    if (idx < 0 || SCM_STRING_LEN(str) <= idx)
        ERR_OBJ("index out of range", k);

#if SCM_USE_MULTIBYTE_CHAR
    SCM_MBS_INIT2(mbs, SCM_STRING_STR(str), strlen(SCM_STRING_STR(str)));
    mbs = scm_mb_strref(scm_current_char_codec, mbs, idx);

    ch = SCM_CHARCODEC_STR2INT(scm_current_char_codec, SCM_MBS_GET_STR(mbs),
                               SCM_MBS_GET_SIZE(mbs), SCM_MBS_GET_STATE(mbs));
    if (ch == EOF)
        ERR("string-ref: invalid char sequence");
#else
    ch = ((unsigned char *)SCM_STRING_STR(str))[idx];
#endif

    return MAKE_CHAR(ch);
}

ScmObj
scm_p_string_setd(ScmObj str, ScmObj k, ScmObj ch)
{
    scm_int_t idx;
    scm_ichar_t ch_val;
    char *c_str;
#if SCM_USE_MULTIBYTE_CHAR
    int ch_len, orig_ch_len;
    size_t prefix_len, suffix_len, new_str_len;
    const char *suffix_src, *ch_end;
    char *new_str, *suffix_dst;
    char ch_buf[SCM_MB_MAX_LEN + sizeof("")];
    ScmMultibyteString mbs_ch;
#endif
    DECLARE_FUNCTION("string-set!", procedure_fixed_3);

    ENSURE_STATELESS_CODEC(scm_current_char_codec);
    ENSURE_STRING(str);
    ENSURE_MUTABLE(str);
    ENSURE_INT(k);
    ENSURE_CHAR(ch);

    idx = SCM_INT_VALUE(k);
    c_str = SCM_STRING_STR(str);
    if (idx < 0 || SCM_STRING_LEN(str) <= idx)
        ERR_OBJ("index out of range", k);

#if SCM_USE_MULTIBYTE_CHAR
    /* point at the char that to be replaced */
    SCM_MBS_INIT2(mbs_ch, c_str, strlen(c_str));
    mbs_ch = scm_mb_strref(scm_current_char_codec, mbs_ch, idx);
    orig_ch_len = SCM_MBS_GET_SIZE(mbs_ch);
    prefix_len = SCM_MBS_GET_STR(mbs_ch) - c_str;

    /* prepare new char */
    ch_val = SCM_CHAR_VALUE(ch);
    ch_end = SCM_CHARCODEC_INT2STR(scm_current_char_codec, ch_buf, ch_val,
                                   SCM_MB_STATELESS);
    if (!ch_end)
        ERR("string-set!: invalid char 0x%x for encoding %s",
            (int)ch_val, SCM_CHARCODEC_ENCODING(scm_current_char_codec));
    ch_len = ch_end - ch_buf;

    /* prepare the space for new char */
    if (ch_len == orig_ch_len) {
        new_str = c_str;
    } else {
        suffix_src = &SCM_MBS_GET_STR(mbs_ch)[orig_ch_len];
        suffix_len = strlen(suffix_src);

        new_str_len = prefix_len + ch_len + suffix_len;
        new_str = scm_realloc(c_str, new_str_len + sizeof(""));

        suffix_src = &new_str[prefix_len + orig_ch_len];
        suffix_dst = &new_str[prefix_len + ch_len];
        memmove(suffix_dst, suffix_src, suffix_len);
        new_str[new_str_len] = '\0';
    }

    /* set new char */
    memcpy(&new_str[prefix_len], ch_buf, ch_len);

    SCM_STRING_SET_STR(str, new_str);
#else
    ch_val = SCM_CHAR_VALUE(ch);
    SCM_ASSERT(isascii(ch_val));
    c_str[idx] = ch_val;
#endif

    return str;
}

#if (!HAVE_STRCASECMP && !SCM_USE_MULTIBYTE_CHAR)
static int
strcasecmp(const char *s1, const char *s2)
{
    unsigned char c1, c2;

    for (;;) {
        c1 = *(const unsigned char *)s1;
        c2 = *(const unsigned char *)s2;

        if (c1 && !c2)
            return 1;
        if (!c1 && c2)
            return -1;
        if (!c1 && !c2)
            return 0;

        if (isascii(c1))
            c1 = tolower(c1);
        if (isascii(c2))
            c2 = tolower(c2);
        
        if (c1 > c2)
            return 1;
        if (c1 < c2)
            return -1;
    }
}
#endif

/* Upper case letters are less than lower. */
static int
string_cmp(const char *funcname,
           ScmObj str1, ScmObj str2, scm_bool case_insensitive)
{
    const char *c_str1, *c_str2;
#if SCM_USE_MULTIBYTE_CHAR
    scm_ichar_t c1, c2;
    ScmMultibyteString mbs1, mbs2;
#endif
    DECLARE_INTERNAL_FUNCTION("string_cmp");

    /* dirty hack to replace internal function name */
    SCM_MANGLE(name) = funcname;

    ENSURE_STRING(str1);
    ENSURE_STRING(str2);

    c_str1 = SCM_STRING_STR(str1);
    c_str2 = SCM_STRING_STR(str2);
#if SCM_USE_MULTIBYTE_CHAR
    SCM_MBS_INIT2(mbs1, c_str1, strlen(c_str1));
    SCM_MBS_INIT2(mbs2, c_str2, strlen(c_str2));
    for (;;) {
        if (SCM_MBS_GET_SIZE(mbs1) && !SCM_MBS_GET_SIZE(mbs2))
            return 1;
        if (!SCM_MBS_GET_SIZE(mbs1) && SCM_MBS_GET_SIZE(mbs2))
            return -1;
        if (!SCM_MBS_GET_SIZE(mbs1) && !SCM_MBS_GET_SIZE(mbs2))
            return 0;

        c1 = SCM_CHARCODEC_READ_CHAR(scm_current_char_codec, mbs1);
        c2 = SCM_CHARCODEC_READ_CHAR(scm_current_char_codec, mbs2);
        if (case_insensitive) {
            c1 = ICHAR_FOLDCASE(c1);
            c2 = ICHAR_FOLDCASE(c2);
        }
        
        if (c1 > c2)
            return 1;
        if (c1 < c2)
            return -1;
    }
#else /* SCM_USE_MULTIBYTE_CHAR */
    if (case_insensitive) {
        return strcasecmp(c_str1, c_str2);
    } else {
        return strcmp(c_str1, c_str2);
    }
#endif /* SCM_USE_MULTIBYTE_CHAR */
}

ScmObj
scm_p_stringequalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string=?", procedure_fixed_2);

    ENSURE_STRING(str1);
    ENSURE_STRING(str2);

    return MAKE_BOOL(STRING_EQUALP(str1, str2));
}

ScmObj
scm_p_string_ci_equalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string-ci=?", procedure_fixed_2);

    ENSURE_STRING(str1);
    ENSURE_STRING(str2);

    return MAKE_BOOL(EQ((str1), (str2))                                     
                     || (SCM_STRING_LEN(str1) == SCM_STRING_LEN(str2)
                         && STRING_CI_CMP(str1, str2) == 0));
}

ScmObj
scm_p_string_greaterp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string>?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CMP(str1, str2) > 0);
}

ScmObj
scm_p_string_lessp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string<?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CMP(str1, str2) < 0);
}

ScmObj
scm_p_string_greater_equalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string>=?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CMP(str1, str2) >= 0);
}

ScmObj
scm_p_string_less_equalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string<=?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CMP(str1, str2) <= 0);
}

ScmObj
scm_p_string_ci_greaterp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string-ci>?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CI_CMP(str1, str2) > 0);
}

ScmObj
scm_p_string_ci_lessp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string-ci<?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CI_CMP(str1, str2) < 0);
}

ScmObj
scm_p_string_ci_greater_equalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string-ci>=?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CI_CMP(str1, str2) >= 0);
}

ScmObj
scm_p_string_ci_less_equalp(ScmObj str1, ScmObj str2)
{
    DECLARE_FUNCTION("string-ci<=?", procedure_fixed_2);

    return MAKE_BOOL(STRING_CI_CMP(str1, str2) <= 0);
}

ScmObj
scm_p_substring(ScmObj str, ScmObj start, ScmObj end)
{
    scm_int_t c_start, c_end, len, sub_len;
    const char *c_str;
    char *new_str;
#if SCM_USE_MULTIBYTE_CHAR
    ScmMultibyteString mbs;
#endif
    DECLARE_FUNCTION("substring", procedure_fixed_3);

    ENSURE_STRING(str);
    ENSURE_INT(start);
    ENSURE_INT(end);

    c_start = SCM_INT_VALUE(start);
    c_end   = SCM_INT_VALUE(end);
    len     = SCM_STRING_LEN(str);

    if (c_start < 0 || len < c_start)
        ERR_OBJ("start index out of range", start);
    if (c_end < 0 || len < c_end)
        ERR_OBJ("end index out of range", end);
    if (c_start > c_end)
        ERR_OBJ("start index exceeded end index", LIST_2(start, end));

    c_str = SCM_STRING_STR(str);
    sub_len = c_end - c_start;

#if SCM_USE_MULTIBYTE_CHAR
    /* substring */
    SCM_MBS_INIT2(mbs, c_str, strlen(c_str));
    mbs = scm_mb_substring(scm_current_char_codec, mbs, c_start, sub_len);

    /* copy the substring */
    new_str = scm_malloc(SCM_MBS_GET_SIZE(mbs) + sizeof(""));
    memcpy(new_str, SCM_MBS_GET_STR(mbs), SCM_MBS_GET_SIZE(mbs));
    new_str[SCM_MBS_GET_SIZE(mbs)] = '\0';
#else
    new_str = scm_malloc(sub_len + sizeof(""));
    memcpy(new_str, &c_str[c_start], sub_len);
    new_str[sub_len] = '\0';
#endif

#if SCM_USE_NULL_CAPABLE_STRING
    /* FIXME: the result is truncated at null and incorrect */
    return MAKE_STRING(new_str, STRLEN_UNKNOWN);
#else
    return MAKE_STRING(new_str, sub_len);
#endif
}

/* FIXME: support stateful encoding */
ScmObj
scm_p_string_append(ScmObj args)
{
    ScmObj rest, str;
    size_t byte_len;
    scm_int_t mb_len;
    char  *new_str, *dst;
    const char *src;
    DECLARE_FUNCTION("string-append", procedure_variadic_0);

    if (NULLP(args))
        return MAKE_STRING_COPYING("", 0);

    /* count total size of the new string */
    byte_len = mb_len = 0;
    rest = args;
    FOR_EACH (str, rest) {
        ENSURE_STRING(str);
        mb_len   += SCM_STRING_LEN(str);
#if SCM_USE_MULTIBYTE_CHAR
        byte_len += strlen(SCM_STRING_STR(str));
#else
        byte_len = mb_len;
#endif
    }

    new_str = scm_malloc(byte_len + sizeof(""));

    /* copy all strings into new_str */
    dst = new_str;
    FOR_EACH (str, args) {
        for (src = SCM_STRING_STR(str); *src;)
            *dst++ = *src++;
    }
    *dst = '\0';

#if SCM_USE_NULL_CAPABLE_STRING
    /* each string is chopped at first null and the result is incorrect */
    return MAKE_STRING(new_str, STRLEN_UNKNOWN);
#else
    return MAKE_STRING(new_str, mb_len);
#endif
}

ScmObj
scm_p_string2list(ScmObj str)
{
#if SCM_USE_MULTIBYTE_CHAR
    ScmMultibyteString mbs;
    ScmQueue q;
#endif
    ScmObj res;
    scm_ichar_t ch;
    scm_int_t mb_len;
    const char *c_str;
    DECLARE_FUNCTION("string->list", procedure_fixed_1);

    ENSURE_STRING(str);

    c_str = SCM_STRING_STR(str);
    mb_len = SCM_STRING_LEN(str);

    res = SCM_NULL;
#if SCM_USE_MULTIBYTE_CHAR
    SCM_QUEUE_POINT_TO(q, res);
    SCM_MBS_INIT2(mbs, c_str, strlen(c_str));
    while (mb_len--) {
        if (SCM_MBS_GET_SIZE(mbs)) {
            ch = SCM_CHARCODEC_READ_CHAR(scm_current_char_codec, mbs);
        } else {
#if SCM_USE_NULL_CAPABLE_STRING
            /* CAUTION: this code may crash when (scm_current_char_codec !=
             * orig_codec) */
            ch = '\0';
            c_str = &SCM_MBS_GET_STR(mbs)[1];
            SCM_MBS_INIT2(mbs, c_str, strlen(c_str));
#else
            break;
#endif /* SCM_USE_NULL_CAPABLE_STRING */
        }
        SCM_QUEUE_ADD(q, MAKE_CHAR(ch));
    }
#else /* SCM_USE_MULTIBYTE_CHAR */
    while (mb_len) {
        ch = ((unsigned char *)c_str)[--mb_len];
        res = CONS(MAKE_CHAR(ch), res);
    }
#endif /* SCM_USE_MULTIBYTE_CHAR */

    return res;
}

ScmObj
scm_p_list2string(ScmObj lst)
{
    ScmObj rest, ch;
    size_t str_size;
    scm_int_t len;
    char *str, *dst;
#if SCM_USE_MULTIBYTE_CHAR
    scm_ichar_t ch_val;
#endif
    DECLARE_FUNCTION("list->string", procedure_fixed_1);

    ENSURE_STATELESS_CODEC(scm_current_char_codec);
    ENSURE_LIST(lst);

    if (NULLP(lst))
        return MAKE_STRING_COPYING("", 0);

    str_size = sizeof("");
    rest = lst;
    len = 0;
    FOR_EACH (ch, rest) {
        ENSURE_CHAR(ch);
#if SCM_USE_MULTIBYTE_CHAR
        ch_val = SCM_CHAR_VALUE(ch);
        str_size += SCM_CHARCODEC_CHAR_LEN(scm_current_char_codec, ch_val);
#else
        str_size++;
#endif
        len++;
    }
    ENSURE_PROPER_LIST_TERMINATION(rest, lst);

    dst = str = scm_malloc(str_size);
    FOR_EACH (ch, lst) {
#if !SCM_USE_NULL_CAPABLE_STRING
        if (ch == '\0')
            ERR("list->string: " SCM_ERRMSG_NULL_IN_STRING);
#endif
#if SCM_USE_MULTIBYTE_CHAR
        dst = SCM_CHARCODEC_INT2STR(scm_current_char_codec, dst,
                                    SCM_CHAR_VALUE(ch), SCM_MB_STATELESS);
#else
        *dst++ = SCM_CHAR_VALUE(ch);
#endif
    }
#if !SCM_USE_MULTIBYTE_CHAR
    *dst = '\0';
#endif

    return MAKE_STRING(str, len);
}

ScmObj
scm_p_string_copy(ScmObj str)
{
    DECLARE_FUNCTION("string-copy", procedure_fixed_1);

    ENSURE_STRING(str);

#if SCM_USE_NULL_CAPABLE_STRING
    /* result is truncated at first null and incorrect */
    return MAKE_STRING_COPYING(SCM_STRING_STR(str), STRLEN_UNKNOWN);
#else
    return MAKE_STRING_COPYING(SCM_STRING_STR(str), SCM_STRING_LEN(str));
#endif
}

ScmObj
scm_p_string_filld(ScmObj str, ScmObj ch)
{
    size_t str_len;
    char *dst;
#if SCM_USE_MULTIBYTE_CHAR
    int ch_len;
    char *new_str;
    char ch_str[SCM_MB_MAX_LEN + sizeof("")];
    const char *next;
#else
    scm_ichar_t ch_val;
    char *c_str;
#endif
    DECLARE_FUNCTION("string-fill!", procedure_fixed_2);

    ENSURE_STATELESS_CODEC(scm_current_char_codec);
    ENSURE_STRING(str);
    ENSURE_MUTABLE(str);
    ENSURE_CHAR(ch);

    str_len = SCM_STRING_LEN(str);
    if (str_len == 0)
        return MAKE_STRING_COPYING("", 0);

#if SCM_USE_MULTIBYTE_CHAR
    next = SCM_CHARCODEC_INT2STR(scm_current_char_codec, ch_str,
                                 SCM_CHAR_VALUE(ch), SCM_MB_STATELESS);
    if (!next)
        ERR("string-fill!: invalid char 0x%x for encoding %s",
            (int)SCM_CHAR_VALUE(ch),
            SCM_CHARCODEC_ENCODING(scm_current_char_codec));

    /* create new str */
    ch_len = next - ch_str;
    new_str = scm_realloc(SCM_STRING_STR(str), str_len * ch_len + sizeof(""));
    for (dst = new_str; dst < &new_str[ch_len * str_len]; dst += ch_len)
        memcpy(dst, ch_str, ch_len);
    *dst = '\0';

    SCM_STRING_SET_STR(str, new_str);
#else
    ch_val = SCM_CHAR_VALUE(ch);
    SCM_ASSERT(isascii(ch_val));
    c_str = SCM_STRING_STR(str);
    for (dst = c_str; dst < &c_str[str_len]; dst++)
        *dst = ch_val;
#endif

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
