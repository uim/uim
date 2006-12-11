/*===========================================================================
 *  Filename : number.c
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

#include <config.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define ERRMSG_DIV_BY_ZERO     "division by zero"
#define ERRMSG_REQ_1_ARG       "at least 1 argument required"

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/

/*=======================================
  Function Definitions
=======================================*/
/*===========================================================================
  R5RS : 6.2 Numbers : 6.2.5 Numerical Operations
===========================================================================*/
/* Note: SigScheme supports only the integer part of the numerical tower. */

SCM_EXPORT ScmObj
scm_p_add(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result, l, r;
    DECLARE_FUNCTION("+", reduction_operator);

    result = l = 0;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        l = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        r = SCM_INT_VALUE(right);
        result = l + r;
        if (INT_OUT_OF_RANGEP(result)
            || (r > 0 && result < l)
            || (r < 0 && result > l))
            ERR(ERRMSG_FIXNUM_OVERFLOW);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        SCM_ASSERT(scm_false);
    }

    return MAKE_INT(result);
}

/* no overflow check */
SCM_EXPORT ScmObj
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

SCM_EXPORT ScmObj
scm_p_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result, l, r;
    DECLARE_FUNCTION("-", reduction_operator);

    result = l = 0;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        ENSURE_INT(left);
        l = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        ENSURE_INT(right);
        r = SCM_INT_VALUE(right);
        result = l - r;
        if (INT_OUT_OF_RANGEP(result)
            || (r > 0 && result > l)
            || (r < 0 && result < l))
            ERR(ERRMSG_FIXNUM_OVERFLOW);
        break;

    case SCM_REDUCE_0:
        ERR(ERRMSG_REQ_1_ARG);
    default:
        SCM_ASSERT(scm_false);
    }
    return MAKE_INT(result);
}

SCM_EXPORT ScmObj
scm_p_divide(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    scm_int_t result, val;
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
        val = SCM_INT_VALUE(right);
        if (val == 0)
            ERR(ERRMSG_DIV_BY_ZERO);
        result /= val;
        break;

    case SCM_REDUCE_0:
        ERR(ERRMSG_REQ_1_ARG);
    default:
        SCM_ASSERT(scm_false);
    }
    return MAKE_INT(result);
}

SCM_EXPORT ScmObj
scm_p_numberp(ScmObj obj)
{
    DECLARE_FUNCTION("number?", procedure_fixed_1);

    return MAKE_BOOL(NUMBERP(obj));
}

SCM_EXPORT ScmObj
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
            return (*state == SCM_REDUCE_LAST) ? SCM_TRUE : right;           \
        *state = SCM_REDUCE_STOP;                                            \
        break;                                                               \
                                                                             \
    default:                                                                 \
        SCM_ASSERT(scm_false);                                               \
    }                                                                        \
    return SCM_FALSE

SCM_EXPORT ScmObj
scm_p_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("=", reduction_operator);

    COMPARATOR_BODY(==);
}

SCM_EXPORT ScmObj
scm_p_less(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<", reduction_operator);

    COMPARATOR_BODY(<);
}

SCM_EXPORT ScmObj
scm_p_less_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("<=", reduction_operator);

    COMPARATOR_BODY(<=);
}

SCM_EXPORT ScmObj
scm_p_greater(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">", reduction_operator);

    COMPARATOR_BODY(>);
}

SCM_EXPORT ScmObj
scm_p_greater_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION(">=", reduction_operator);

    COMPARATOR_BODY(>=);
}

#undef COMPARATOR_BODY

SCM_EXPORT ScmObj
scm_p_zerop(ScmObj n)
{
    DECLARE_FUNCTION("zero?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) == 0);
}

SCM_EXPORT ScmObj
scm_p_positivep(ScmObj n)
{
    DECLARE_FUNCTION("positive?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) > 0);
}

SCM_EXPORT ScmObj
scm_p_negativep(ScmObj n)
{
    DECLARE_FUNCTION("negative?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) < 0);
}

SCM_EXPORT ScmObj
scm_p_oddp(ScmObj n)
{
    DECLARE_FUNCTION("odd?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(SCM_INT_VALUE(n) & 0x1);
}

SCM_EXPORT ScmObj
scm_p_evenp(ScmObj n)
{
    DECLARE_FUNCTION("even?", procedure_fixed_1);

    ENSURE_INT(n);

    return MAKE_BOOL(!(SCM_INT_VALUE(n) & 0x1));
}

SCM_EXPORT ScmObj
scm_p_max(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("max", reduction_operator);

    if (*state == SCM_REDUCE_0)
        ERR(ERRMSG_REQ_1_ARG);
    ENSURE_INT(left);
    ENSURE_INT(right);

    return (SCM_INT_VALUE(left) > SCM_INT_VALUE(right)) ? left : right;
}

SCM_EXPORT ScmObj
scm_p_min(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    DECLARE_FUNCTION("min", reduction_operator);

    if (*state == SCM_REDUCE_0)
        ERR(ERRMSG_REQ_1_ARG);
    ENSURE_INT(left);
    ENSURE_INT(right);

    return (SCM_INT_VALUE(left) < SCM_INT_VALUE(right)) ? left : right;
}


SCM_EXPORT ScmObj
scm_p_abs(ScmObj _n)
{
    scm_int_t n;
    DECLARE_FUNCTION("abs", procedure_fixed_1);

    ENSURE_INT(_n);

    n = SCM_INT_VALUE(_n);
    if (n == SCM_INT_MIN)
        ERR(ERRMSG_FIXNUM_OVERFLOW);

    return (n < 0) ? MAKE_INT(-n) : _n;
}

SCM_EXPORT ScmObj
scm_p_quotient(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2;
    DECLARE_FUNCTION("quotient", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);
    if (n2 == 0)
        ERR(ERRMSG_DIV_BY_ZERO);

    /*
     * ISO/IEC 9899:1999(E):
     *
     * 6.3.1.4 Real floating and integer
     * 
     * 1 When a finite value of real floating type is converted to an integer
     *   type other than _Bool, the fractional part is discarded (i.e., the
     *   value is truncated toward zero). If the value of the integral part
     *   cannot be represented by the integer type, the behavior is undefined.
     */    
    return MAKE_INT((scm_int_t)(n1 / n2));
}

SCM_EXPORT ScmObj
scm_p_modulo(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2, rem;
    DECLARE_FUNCTION("modulo", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);
    if (n2 == 0)
        ERR(ERRMSG_DIV_BY_ZERO);

    rem = n1 % n2;
    if (rem && ((n1 < 0 && 0 < n2) || (n2 < 0 && 0 < n1)))
        rem += n2;

    return MAKE_INT(rem);
}

SCM_EXPORT ScmObj
scm_p_remainder(ScmObj _n1, ScmObj _n2)
{
    scm_int_t n1, n2;
    DECLARE_FUNCTION("remainder", procedure_fixed_2);

    ENSURE_INT(_n1);
    ENSURE_INT(_n2);

    n1 = SCM_INT_VALUE(_n1);
    n2 = SCM_INT_VALUE(_n2);
    if (n2 == 0)
        ERR(ERRMSG_DIV_BY_ZERO);

    return MAKE_INT(n1 % n2);
}
