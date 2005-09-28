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
#define SCM_INVALID NULL

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static int ScmOp_c_length(ScmObj lst);
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

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    type = (enum ScmObjType)SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
    case ScmInt:
        /* both numbers, are numerically equal */
        if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2))) return SCM_TRUE;
        break;

    case ScmChar:
        /* chars and are the same character according to the char=? */
        return ScmOp_char_equal(obj1, obj2);

    case ScmSymbol:  /* equivalent symbols must already be true on eq? */
    case ScmCons:
    case ScmVector:
    case ScmString:
    case ScmFunc:
    case ScmClosure:
    case ScmPort:
    case ScmContinuation:
    case ScmValuePacket:
    case ScmEtc:
        break;

    case ScmFreeCell:
        SigScm_Error("eqv? : cannnot compare freecell, gc broken?");
        break;

    case ScmCPointer:
    case ScmCFuncPointer:
        break;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_eqp(ScmObj obj1, ScmObj obj2)
{
    return (EQ(obj1, obj2)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_equalp(ScmObj obj1, ScmObj obj2)
{
    int  i = 0;
    enum ScmObjType type;

    if (EQ(obj1, obj2))
        return SCM_TRUE;

    type = (enum ScmObjType)SCM_TYPE(obj1);

    /* different type */
    if (type != SCM_TYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
    case ScmInt:
        /* both numbers, are numerically equal */
        if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2))) return SCM_TRUE;
        break;

    case ScmChar:
        /* chars and are the same character according to the char=? */
        return ScmOp_char_equal(obj1, obj2);

    case ScmCons:
        for (; !NULLP(obj1); obj1 = CDR(obj1), obj2 = CDR(obj2)) {
            /* check contents */
            if (FALSEP(ScmOp_equalp(CAR(obj1), CAR(obj2))))
                return SCM_FALSE;

            /* check next cdr's type */
            if (SCM_TYPE(CDR(obj1)) != SCM_TYPE(CDR(obj2)))
                return SCM_FALSE;

            /* check dot pair */
            if (!CONSP(CDR(obj1)))
                return ScmOp_equalp(CDR(obj1), CDR(obj2));
        }
        return SCM_TRUE;

    case ScmVector:
        /* check len */
        if (SCM_VECTOR_LEN(obj1) != SCM_VECTOR_LEN(obj2))
            return SCM_FALSE;

        /* check contents */
        for (i = 0; i < SCM_VECTOR_LEN(obj1); i++) {
            if (FALSEP(ScmOp_equalp(SCM_VECTOR_CREF(obj1, i),
                                    SCM_VECTOR_CREF(obj2, i))))
                return SCM_FALSE;
        }
        return SCM_TRUE;

    case ScmString:
        /* check string data */
        if (strcmp(SCM_STRING_STR(obj1), SCM_STRING_STR(obj2)) == 0)
            return SCM_TRUE;
        break;

    case ScmSymbol:
        /* equivalent symbols must already be true on the prior EQ */
        break;

    case ScmFunc:
        if (EQ(SCM_FUNC_CFUNC(obj1), SCM_FUNC_CFUNC(obj2)))
            return SCM_TRUE;
        break;

    case ScmClosure:
        /*
         * eq? is the valid equality check for closures. Having same members
         * does not ensure equality.
         */
        break;

    case ScmPort:
#if 0
        /* does not make sense. eq? is sufficient */
        if (EQ(SCM_PORT_PORTDIRECTION(obj1), SCM_PORT_PORTDIRECTION(obj2))
            && EQ(SCM_PORT_PORTINFO(obj1), SCM_PORT_PORTINFO(obj2)))
            return SCM_TRUE;
#endif
        break;

    case ScmContinuation:
        /*
         * eq? is the valid equality check for continuations. Having same
         * members does not ensure equality.
         */
        break;

    case ScmValuePacket:
#if 0
        /* does not make sense. eq? is sufficient */
        if (EQ(SCM_VALUEPACKET_VALUES(obj1), SCM_VALUEPACKET_VALUES(obj2)))
            return SCM_TRUE;
#endif
        break;

    case ScmEtc:
        break;

    case ScmFreeCell:
        SigScm_Error("equal? : cannnot compare freecell, gc broken?");
        break;

    case ScmCPointer:
        if (SCM_C_POINTER_VALUE(obj1) == SCM_C_POINTER_VALUE(obj2))
            return SCM_TRUE;
        break;

    case ScmCFuncPointer:
        if (SCM_C_FUNCPOINTER_VALUE(obj1) == SCM_C_FUNCPOINTER_VALUE(obj2))
            return SCM_TRUE;
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
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        if (!INTP(left))
            SigScm_ErrorObj("+ : integer required but got ", left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        if (!INTP(right))
            SigScm_ErrorObj("+ : integer required but got ", right);
        result += SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        SigScm_Error("+ : (internal error) unrecognized state specifier: %d", *state);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_multiply(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        if (!INTP(left))
            SigScm_ErrorObj("* : integer required but got ", left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        if (!INTP(right))
            SigScm_ErrorObj("* : integer required but got ", right);
        result *= SCM_INT_VALUE(right);
        /* Fall through. */
    case SCM_REDUCE_0:
        break;
    default:
        SigScm_Error("* : (internal error) unrecognized state specifier: %d", *state);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_subtract(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 0;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        if (!INTP(left))
            SigScm_ErrorObj("- : integer required but got ", left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        if (!INTP(right))
            SigScm_ErrorObj("- : integer required but got ", right);
        result -= SCM_INT_VALUE(right);
        break;

    case SCM_REDUCE_0:
        SigScm_Error("- : at least 1 argument required");
    default:
        SigScm_Error("- : (internal error) unrecognized state specifier: %d", *state);
    }
    return Scm_NewInt(result);
}

ScmObj ScmOp_divide(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    int result = 1;
    switch (*state) {
    case SCM_REDUCE_PARTWAY:
    case SCM_REDUCE_LAST:
        if (!INTP(left))
            SigScm_ErrorObj("/ : integer required but got ", left);
        result = SCM_INT_VALUE(left);
        /* Fall through. */
    case SCM_REDUCE_1:
        if (!INTP(right))
            SigScm_ErrorObj("/ : integer required but got ", right);
        if (SCM_INT_VALUE(right) == 0)
            SigScm_Error("/ : division by zero");
        result /= SCM_INT_VALUE(right);
        break;
    case SCM_REDUCE_0:
        SigScm_Error("/ : at least 1 argument required");
    default:
        SigScm_Error("/ : (internal error) unrecognized state specifier: ", *state);
    }
    return Scm_NewInt(result);
}

ScmObj ScmOp_numberp(ScmObj obj)
{
    return (INTP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_equal(ScmObj left, ScmObj right, enum ScmReductionState *state)
{

#define COMPARATOR_BODY(op, opstr) \
    switch (*state) { \
    case SCM_REDUCE_0: \
    case SCM_REDUCE_1: \
        SigScm_Error(opstr " : at least 2 arguments required"); \
    case SCM_REDUCE_PARTWAY: \
    case SCM_REDUCE_LAST: \
        if (!INTP(left)) \
            SigScm_ErrorObj(opstr " : integer required but got ", left); \
        if (!INTP(right)) \
            SigScm_ErrorObj(opstr " : integer required but got ", right); \
        if (SCM_INT_VALUE(left) op SCM_INT_VALUE(right)) \
            return *state == SCM_REDUCE_LAST ? SCM_TRUE : right; \
        *state = SCM_REDUCE_STOP; \
        return SCM_FALSE; \
    default: \
        SigScm_Error(opstr " : (internal error) unrecognized state specifier: ", *state); \
    } \
    return SCM_INVALID

    COMPARATOR_BODY(==, "=");
}

ScmObj ScmOp_less(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    COMPARATOR_BODY(<, "<");
}

ScmObj ScmOp_less_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    COMPARATOR_BODY(<=, "<=");
}

ScmObj ScmOp_greater(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    COMPARATOR_BODY(>, ">");
}

ScmObj ScmOp_greater_eq(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    COMPARATOR_BODY(>=, ">=");
#undef COMPARATOR_BODY
}

ScmObj ScmOp_zerop(ScmObj scm_num)
{
    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("zero? : number required but got ", scm_num);

    return (SCM_INT_VALUE(scm_num) == 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_positivep(ScmObj scm_num)
{
    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("positive? : number required but got", scm_num);

    return (SCM_INT_VALUE(scm_num) > 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_negativep(ScmObj scm_num)
{
    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("negative? : number required but got ", scm_num);

    return (SCM_INT_VALUE(scm_num) < 0) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_oddp(ScmObj scm_num)
{
    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("odd? : number required but got ", scm_num);

    return (SCM_INT_VALUE(scm_num) & 0x1) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_evenp(ScmObj scm_num)
{
    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("even? : number required but got ", scm_num);

    return (SCM_INT_VALUE(scm_num) & 0x1) ? SCM_FALSE : SCM_TRUE;
}

ScmObj ScmOp_max(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    if (*state == SCM_REDUCE_0)
        SigScm_Error("max : at least 1 argument required");
    if (!INTP(left))
        SigScm_Error("max : integer required but got ", left);
    if (!INTP(right))
        SigScm_Error("max : integer required but got ", right);

    return SCM_INT_VALUE(left) > SCM_INT_VALUE(right) ? left : right;
}

ScmObj ScmOp_min(ScmObj left, ScmObj right, enum ScmReductionState *state)
{
    if (*state == SCM_REDUCE_0)
        SigScm_Error("min : at least 1 argument required");
    if (!INTP(left))
        SigScm_Error("min : integer required but got ", left);
    if (!INTP(right))
        SigScm_Error("min : integer required but got ", right);

    return SCM_INT_VALUE(left) < SCM_INT_VALUE(right) ? left : right;
}


ScmObj ScmOp_abs(ScmObj scm_num)
{
    int num = 0;

    if (FALSEP(ScmOp_numberp(scm_num)))
        SigScm_ErrorObj("abs : number required but got ", scm_num);

    num = SCM_INT_VALUE(scm_num);

    return (num < 0) ? Scm_NewInt(-num) : scm_num;
}

ScmObj ScmOp_quotient(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1 = 0;
    int n2 = 0;

    if (FALSEP(ScmOp_numberp(scm_n1)))
        SigScm_ErrorObj("quotient : number required but got ", scm_n1);
    if (FALSEP(ScmOp_numberp(scm_n2)))
        SigScm_ErrorObj("quotient : number required but got ", scm_n2);
    if (NFALSEP(ScmOp_zerop(scm_n2)))
        SigScm_Error("quotient : divide by zero");

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    return Scm_NewInt((int)(n1 / n2));
}

ScmObj ScmOp_modulo(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1  = 0;
    int n2  = 0;
    int rem = 0;

    if (FALSEP(ScmOp_numberp(scm_n1)))
        SigScm_ErrorObj("modulo : number required but got ", scm_n1);
    if (FALSEP(ScmOp_numberp(scm_n2)))
        SigScm_ErrorObj("modulo : number required but got ", scm_n2);
    if (NFALSEP(ScmOp_zerop(scm_n2)))
        SigScm_Error("modulo : divide by zero");

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

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

    if (FALSEP(ScmOp_numberp(scm_n1)))
        SigScm_ErrorObj("remainder : number required but got ", scm_n1);
    if (FALSEP(ScmOp_numberp(scm_n2)))
        SigScm_ErrorObj("remainder : number required but got ", scm_n2);
    if (NFALSEP(ScmOp_zerop(scm_n2)))
        SigScm_Error("remainder : divide by zero");

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    return Scm_NewInt(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/
ScmObj ScmOp_number2string (ScmObj num, ScmObj args)
{
  char buf[sizeof(int)*CHAR_BIT + 1];
  char *p;
  unsigned int n, r;
  ScmObj radix;

  if (!INTP(num))
      SigScm_ErrorObj("number->string: integer required but got ", num);

  n = SCM_INT_VALUE(num);

  /* r = radix */
  if (NULLP(args))
      r = 10;
  else {
#ifdef SCM_STRICT_ARGCHECK
      if (!NULLP(CDR(args)))
          SigScm_ErrorObj("number->string: too many arguments: ", args);
#endif
      radix = CAR(args);
      if (!INTP(radix))
          SigScm_ErrorObj("number->string: integer required but got ", radix);
      r = SCM_INT_VALUE(radix);

      if (!(2 <= r && r <= 16))
          SigScm_ErrorObj("number->string: invalid or unsupported radix: ",
                          radix);
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

    if (!STRINGP(string))
        SigScm_ErrorObj("string->number : string required but got ", string);

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
    return (FALSEP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_booleanp(ScmObj obj)
{
    return (EQ(obj, SCM_FALSE) || EQ(obj, SCM_TRUE)) ? SCM_TRUE : SCM_FALSE;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
==============================================================================*/
ScmObj ScmOp_car(ScmObj obj)
{
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    if (!CONSP(obj))
        SigScm_ErrorObj("car : pair required but got ", obj);

    return CAR(obj);
}

ScmObj ScmOp_cdr(ScmObj obj)
{
#if SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        return SCM_NULL;
#endif

    if (!CONSP(obj))
        SigScm_ErrorObj("cdr : pair required but got ", obj);

    return CDR(obj);
}

ScmObj ScmOp_pairp(ScmObj obj)
{
    return (CONSP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_cons(ScmObj car, ScmObj cdr)
{
    return CONS(car, cdr);
}

ScmObj ScmOp_setcar(ScmObj pair, ScmObj car)
{
    if (!CONSP(pair))
        SigScm_ErrorObj("set-car! : pair required but got ", pair);

    SET_CAR(pair, car);

#if SCM_COMPAT_SIOD
    return car;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_setcdr(ScmObj pair, ScmObj cdr)
{
    if (!CONSP(pair))
        SigScm_ErrorObj("set-cdr! : pair required but got ", pair);

    SET_CDR(pair, cdr);

#if SCM_COMPAT_SIOD
    return cdr;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_caar(ScmObj lst)
{
    return ScmOp_car( ScmOp_car(lst) );
}
ScmObj ScmOp_cadr(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr(lst) );
}
ScmObj ScmOp_cdar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car(lst) );
}
ScmObj ScmOp_cddr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr(lst) );
}
ScmObj ScmOp_caaar(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_caadr(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cadar(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car(lst) ));
}
ScmObj ScmOp_caddr(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cdaar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdadr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_cddar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) ));
}
ScmObj ScmOp_cdddr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) ));
}
ScmObj ScmOp_caaaar(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaadr(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caadar(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_caaddr(ScmObj lst)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cadaar(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadadr(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_caddar(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cadddr(ScmObj lst)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdaaar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaadr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdadar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cdaddr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cddaar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddadr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_cdr(lst) )));
}
ScmObj ScmOp_cdddar(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_car(lst) )));
}
ScmObj ScmOp_cddddr(ScmObj lst)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(lst) )));
}

ScmObj ScmOp_list(ScmObj args)
{
    return args;
}

ScmObj ScmOp_nullp(ScmObj obj)
{
    return (NULLP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_listp(ScmObj obj)
{
    int len = 0;

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
static int ScmOp_c_length(ScmObj lst)
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
    if (len < 0)
        SigScm_ErrorObj("length : list required but got ", obj);

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
    return (SYMBOLP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_symbol2string(ScmObj obj)
{
    if (!SYMBOLP(obj))
        SigScm_ErrorObj("symbol->string: symbol required, but got ", obj);

    return Scm_NewStringCopying(SCM_SYMBOL_NAME(obj));
}

ScmObj ScmOp_string2symbol(ScmObj str)
{
    if(!STRINGP(str))
        SigScm_ErrorObj("string->symbol: string required, but got ", str);

    return Scm_Intern(SCM_STRING_STR(str));
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.4 Characters
==============================================================================*/
ScmObj ScmOp_charp(ScmObj obj)
{
    return (CHARP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_char_equal(ScmObj ch1, ScmObj ch2)
{
    if (!CHARP(ch1))
        SigScm_ErrorObj("char=? : char required but got ", ch1);
    if (!CHARP(ch2))
        SigScm_ErrorObj("char=? : char required but got ", ch2);

    if (strcmp(SCM_CHAR_VALUE(ch1), SCM_CHAR_VALUE(ch2)) == 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_alphabeticp(ScmObj obj)
{
    if (!CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

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
    if (!CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

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
    if (!CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

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
    if (!CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

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
    if (!CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

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
    if (!CHARP(obj))
        SigScm_ErrorObj("char-upcase : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_VALUE(obj)) != 1)
        return obj;

    /* to upcase */
    SCM_CHAR_VALUE(obj)[0] = toupper(SCM_CHAR_VALUE(obj)[0]);

    return obj;
}

ScmObj ScmOp_char_downcase(ScmObj obj)
{
    if (!CHARP(obj))
        SigScm_ErrorObj("char-upcase : char required but got ", obj);

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
    return (STRINGP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_make_string(ScmObj length, ScmObj args)
{
    int len = 0;
    ScmObj str    = SCM_FALSE;
    ScmObj filler = SCM_FALSE;

    /* sanity check */
    if (!INTP(length))
        SigScm_ErrorObj("make-string : integer required but got ", length);

    /* get length */
    len = SCM_INT_VALUE(length);
    if (len == 0)
        return Scm_NewStringCopying("");

    /* specify filler */
    if (NULLP(args)) {
        filler = Scm_NewChar(strdup(" "));
    } else {
        filler = CAR(args);
        if (!CHARP(filler))
            SigScm_ErrorObj("make-string : character required but got ", filler);
    }

    /* make string */
    str = Scm_NewStringWithLen(NULL, len);

    /* and fill! */
    ScmOp_string_fill(str, filler);

    return str;
}

ScmObj ScmOp_string(ScmObj args)
{
    return ScmOp_list2string(args);
}

ScmObj ScmOp_string_length(ScmObj str)
{
    if (!STRINGP(str))
        SigScm_ErrorObj("string-length : string required but got ", str);

    return Scm_NewInt(SigScm_default_encoding_strlen(SCM_STRING_STR(str)));
}

ScmObj ScmOp_string_ref(ScmObj str, ScmObj k)
{
    int   c_index = 0;
    char *new_ch  = NULL;
    const char *string_str   = NULL;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;

    if (!STRINGP(str))
        SigScm_ErrorObj("string-ref : string required but got ", str);
    if (!INTP(k))
        SigScm_ErrorObj("string-ref : number required but got ", k);

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

    if (!STRINGP(str))
        SigScm_ErrorObj("string-set! : string required but got ", str);
    if (!INTP(k))
        SigScm_ErrorObj("string-set! : number required but got ", k);
    if (!CHARP(ch))
        SigScm_ErrorObj("string-set! : character required but got ", ch);

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

    if (!STRINGP(str))
        SigScm_ErrorObj("string-ref : string required but got ", str);
    if (!INTP(start))
        SigScm_ErrorObj("string-ref : number required but got ", start);
    if (!INTP(end))
        SigScm_ErrorObj("string-ref : number required but got ", end);

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
    int total_size = 0;
    int total_len  = 0;
    ScmObj strings = SCM_NULL;
    ScmObj obj     = SCM_NULL;
    char  *new_str = NULL;
    char  *p       = NULL;

    /* sanity check */
    if (NULLP(args))
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

    if (!STRINGP(string))
        SigScm_ErrorObj("string->list : string required but got ", string);

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
    if (!STRINGP(string))
        SigScm_ErrorObj("string-copy : string required but got ", string);

    return Scm_NewStringCopying(SCM_STRING_STR(string));
}

ScmObj ScmOp_string_fill(ScmObj string, ScmObj ch)
{
    int  char_size = 0;
    int  str_len   = 0;
    char *new_str  = NULL;
    char *p        = NULL;
    int   i        = 0;

    if (!STRINGP(string))
        SigScm_ErrorObj("string-fill! : string required but got ", string);
    if (!CHARP(ch))
        SigScm_ErrorObj("string-fill! : character required but got ", ch);

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
    if (!VECTORP(vec))
        SigScm_ErrorObj("vector-length : vector required but got ", vec);

    return Scm_NewInt(SCM_VECTOR_LEN(vec));
}

ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj scm_k)
{
    if (!VECTORP(vec))
        SigScm_ErrorObj("vector-ref : vector required but got ", vec);
    if (!INTP(scm_k))
        SigScm_ErrorObj("vector-ref : number required but got ", scm_k);

    return SCM_VECTOR_REF(vec, scm_k);
}

ScmObj ScmOp_vector_set(ScmObj vec, ScmObj scm_k, ScmObj obj)
{
    if (!VECTORP(vec))
        SigScm_ErrorObj("vector-set! : vector required but got ", vec);
    if (!INTP(scm_k))
        SigScm_ErrorObj("vector-set! : number required but got ", scm_k);

    SCM_VECTOR_SET_REF(vec, scm_k, obj);

    return vec;
}

ScmObj ScmOp_vector2list(ScmObj vec)
{
    ScmObj *v    = NULL;
    ScmObj  prev = NULL;
    ScmObj  next = NULL;
    ScmObj  head = NULL;
    int c_len = 0;
    int i = 0;

    if (!VECTORP(vec))
        SigScm_ErrorObj("vector->list : vector required but got ", vec);

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

    if (!VECTORP(vec))
        SigScm_ErrorObj("vector->list : vector required but got ", vec);

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
    return (FUNCP(obj) || CLOSUREP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_map(ScmObj proc, ScmObj args)
{
     /* sanity check */
    if (NULLP(args))
        SigScm_Error("map : wrong number of arguments");

    /* fast path for sinble arg case */
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
    ScmOp_map(proc, args);

    return SCM_UNDEF;
}

ScmObj ScmOp_force(ScmObj closure)
{
    if (!CLOSUREP(closure))
        SigScm_ErrorObj("force : not proper delayed object ", closure);

    return Scm_call(closure, SCM_NULL);
}

ScmObj ScmOp_call_with_current_continuation(ScmObj proc)
{
    int jmpret  = 0;
    ScmObj cont = SCM_FALSE;

    if (!CLOSUREP(proc))
        SigScm_ErrorObj("call-with-current-continuation : closure required but got ", proc);

    cont = Scm_NewContinuation();

    /* setjmp and check result */
    jmpret = setjmp(SCM_CONTINUATION_JMPENV(cont));
    if (jmpret) {
        /* return by calling longjmp */
        return scm_continuation_thrown_obj;
    }

    /* execute (proc cont) */
    return Scm_call(proc, LIST_1(cont));
}

ScmObj ScmOp_values(ScmObj args)
{
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

ScmObj ScmOp_call_with_values(ScmObj producer, ScmObj consumer)
{
    ScmObj vals;

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

    return Scm_call(consumer, vals);
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
