/*===========================================================================
 *  FileName : number.c
 *  About    : R5RS numbers
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

/*=======================================
  System Include
=======================================*/
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
static int prepare_radix(const char *funcname, ScmObj args);

/*=======================================
  Function Implementations
=======================================*/
/*=======================================
  R5RS : 6.2 Numbers
=======================================*/
/*===========================================================================
  R5RS : 6.2 Numbers : 6.2.5 Numerical Operations
===========================================================================*/
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
scm_p_less_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
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
scm_p_greater_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
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
scm_p_abs(ScmObj _n)
{
    scm_int_t n;
    DECLARE_FUNCTION("abs", procedure_fixed_1);

    ENSURE_INT(_n);

    n = SCM_INT_VALUE(_n);

    return (n < 0) ? MAKE_INT(-n) : _n;
}

ScmObj
scm_p_quotient(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2;
    DECLARE_FUNCTION("quotient", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT((int)(n1 / n2));
}

ScmObj
scm_p_modulo(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2, rem;
    DECLARE_FUNCTION("modulo", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);

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
scm_p_remainder(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2;
    DECLARE_FUNCTION("remainder", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);

    if (n2 == 0)
        ERR("division by zero");

    return MAKE_INT(n1 % n2);
}

/*===========================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
===========================================================================*/

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
  char buf[sizeof("-") + SCM_INT_BITS];
  char *p;
  const char *end;
  scm_int_t n;
  /* 'un' must be unsinged to be capable of -INT_MIN */
  scm_uint_t un, digit, r;
  scm_bool neg;
  DECLARE_FUNCTION("number->string", procedure_variadic_1);

  ENSURE_INT(num);

  n = SCM_INT_VALUE(num);
  neg = (n < 0);
  un = (neg) ? -n : n;
  r = (scm_uint_t)prepare_radix(SCM_MANGLE(name), args);

  end = p = &buf[sizeof(buf) - 1];
  *p = '\0';

  do {
      digit = un % r;
      *--p = (digit <= 9) ? '0' + digit : 'a' + digit - 10;
  } while (un /= r);
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
