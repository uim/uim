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
static int ScmOp_c_length(ScmObj list);
static ScmObj ScmOp_listtail_internal(ScmObj obj, int k);

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
        SigScm_Error("eqv? : cannnot compare freecell, gc broken?\n");
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
            if (!CONSP(CDR(obj1))) {
                if (FALSEP(ScmOp_equalp(CDR(obj1), CDR(obj2))))
                    return SCM_FALSE;
                else
                    return SCM_TRUE;
            }
        }
        return SCM_TRUE;

    case ScmVector:
        /* check len */
        if (SCM_VECTOR_LEN(obj1) != SCM_VECTOR_LEN(obj2))
            return SCM_FALSE;

        /* check contents */
        for (i = 0; i < SCM_VECTOR_LEN(obj1); i++) {
            if (FALSEP(ScmOp_equalp(SCM_VECTOR_CREF(obj1, i), SCM_VECTOR_CREF(obj2, i))))
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
        SigScm_Error("equal? : cannnot compare freecell, gc broken?\n");
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

ScmObj ScmOp_plus(ScmObj args, ScmObj env)
{
    int result = 0;
    ScmObj operand = SCM_NULL;

    for (; !NULLP(args); args = CDR(args)) {
        operand = ScmOp_eval(CAR(args), env);
        if (!INTP(operand))
            SigScm_ErrorObj("+ : integer required but got ", operand);
        result += SCM_INT_VALUE(operand);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_times(ScmObj args, ScmObj env)
{
    int result = 1;
    ScmObj operand = SCM_NULL;

    for (; !NULLP(args); args = CDR(args)) {
        operand = ScmOp_eval(CAR(args), env);
        if (!INTP(operand))
            SigScm_ErrorObj("* : integer required but got ", operand);
        result *= SCM_INT_VALUE(operand);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_minus(ScmObj args, ScmObj env)
{
    int result = 0;
    ScmObj operand = SCM_NULL;

    if (NULLP(args))
        SigScm_Error("- : at least 1 argument required\n");

    result = SCM_INT_VALUE(ScmOp_eval(CAR(args), env));
    args = CDR(args);

    /* single arg */
    if (NULLP(args))
        return Scm_NewInt(-result);

    for (; !NULLP(args); args = CDR(args)) {
        operand = ScmOp_eval(CAR(args), env);
        if (!INTP(operand))
            SigScm_ErrorObj("- : integer required but got ", operand);
        result -= SCM_INT_VALUE(operand);
    }
    
    return Scm_NewInt(result);
}

ScmObj ScmOp_divide(ScmObj args, ScmObj env)
{
    int result = 0;
    ScmObj operand = SCM_NULL;

    if (NULLP(args))
        SigScm_Error("/ : at least 1 argument required\n");

    result = SCM_INT_VALUE(ScmOp_eval(CAR(args), env));
    args = CDR(args);

    /* single arg */
    if (NULLP(args))
        return Scm_NewInt(1 / result);

    for (; !NULLP(args); args = CDR(args)) {
        operand = ScmOp_eval(CAR(args), env);
        if (!INTP(operand))
            SigScm_ErrorObj("/ : integer required but got ", operand);

        if (SCM_INT_VALUE(operand) == 0)
            SigScm_ErrorObj("/ : division by zero ", args);
        result /= SCM_INT_VALUE(operand);
    }

    return Scm_NewInt(result);
}

ScmObj ScmOp_numberp(ScmObj obj)
{
    if (INTP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_equal(ScmObj args, ScmObj env)
{
    int    val = 0;
    ScmObj obj = SCM_NULL;

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("= : Wrong number of arguments\n");

    /* type check */
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj("= : number required but got ", CAR(args));

    /* Get first value */
    val = SCM_INT_VALUE(CAR(args));

    /* compare following value */
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        obj = CAR(args);
        if (FALSEP(ScmOp_numberp(obj)))
            SigScm_ErrorObj("= : number required but got ", obj);

        if (SCM_INT_VALUE(obj) != val)
        {
            return SCM_FALSE;
        }
    }

    return SCM_TRUE;
}

ScmObj ScmOp_less(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NULL;

    if (NULLP(args) || NULLP(CDR(args)))
        SigScm_Error("< : Wrong number of arguments\n");

    /* type check */
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj("< : number required but got ", CAR(args));

    /* Get first value */
    val = SCM_INT_VALUE(CAR(args));

    /* compare following value */
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        obj = CAR(args);
        if (FALSEP(ScmOp_numberp(obj)))
            SigScm_ErrorObj("< : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val < car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_greater(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NULL;

    /* type check */
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj("> : number required but got ", CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("> : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(CAR(args));

    /* compare following value */
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        obj = CAR(args);
        if (FALSEP(ScmOp_numberp(obj)))
            SigScm_ErrorObj("> : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val > car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_less_eq(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NULL;

    /* type check */
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj("<= : number required but got ", CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("<= : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(CAR(args));

    /* compare following value */
    obj = SCM_NULL;
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        obj = CAR(args);
        if (FALSEP(ScmOp_numberp(obj)))
            SigScm_ErrorObj("<= : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val <= car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_greater_eq(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NULL;

    /* type check */
    if (FALSEP(ScmOp_numberp(CAR(args))))
        SigScm_ErrorObj(">= : number required but got ", CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error(">= : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(CAR(args));

    /* compare following value */
    obj = SCM_NULL;
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        obj = CAR(args);
        if (FALSEP(ScmOp_numberp(obj)))
            SigScm_ErrorObj(">= : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val >= car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
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

ScmObj ScmOp_max(ScmObj args, ScmObj env )
{
    int max = 0;
    int val = 0;
    ScmObj scm_num = SCM_NULL;

    if (NULLP(args))
        SigScm_Error("max : at least 1 number required\n");

    for (; !NULLP(args); args = CDR(args)) {
        scm_num = ScmOp_eval(CAR(args), env);
        if (FALSEP(ScmOp_numberp(scm_num)))
            SigScm_ErrorObj("max : number required but got ", scm_num);

        val = SCM_INT_VALUE(scm_num);
        if (max < val)
            max = val;
    }

    return Scm_NewInt(max);
}

ScmObj ScmOp_min(ScmObj args, ScmObj env )
{
    int min = 0;
    int val = 0;
    ScmObj scm_num = SCM_NULL;

    if (NULLP(args))
        SigScm_Error("min : at least 1 number required\n");

    for (; !NULLP(args); args = CDR(args)) {
        scm_num = ScmOp_eval(CAR(args), env);
        if (FALSEP(ScmOp_numberp(scm_num)))
            SigScm_ErrorObj("min : number required but got ", scm_num);

        val = SCM_INT_VALUE(scm_num);
        if (val < min)
            min = val;
    }

    return Scm_NewInt(min);
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
        SigScm_Error("quotient : divide by zero\n");

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
        SigScm_Error("modulo : divide by zero\n");

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
        SigScm_Error("remainder : divide by zero\n");

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    return Scm_NewInt(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/
ScmObj ScmOp_number2string (ScmObj args, ScmObj env)
{
  char buf[sizeof(int)*CHAR_BIT + 1];
  char *p;
  unsigned int n, r;
  ScmObj number, radix;

  if (CHECK_1_ARG(args))
      SigScm_ErrorObj("number->string: requires 1 or 2 arguments: ", args);

  number = CAR(args);
  if (!INTP(number))
      SigScm_ErrorObj("number->string: integer required but got ", number);

  n = SCM_INT_VALUE(number);

  /* r = radix */
  if (NULLP(CDR(args)))
      r = 10;
  else {
#ifdef SCM_STRICT_ARGCHECK
      if (!NULLP(SCM_CDDR(args)))
          SigScm_ErrorObj("number->string: too many arguments: ", args);
#endif
      radix = SCM_CADR(args);
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

  do
    {
      if (n % r > 9)
        *--p = 'A' + n % r - 10;
      else
        *--p = '0' + n % r;
    }
  while (n /= r);
  if (r == 10 && SCM_INT_VALUE (number) < 0)
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
    /*
     * TODO: fixme! : Kazuki Ohta <mover@hct.zaq.ne.jp>
     *
     * In R5RS (car '()) becomes an error, but current uim assumes (car '()) == ()
     * in many places. So, I decided to change ScmOp_car to SIOD like behavior.
     * 
     */
#if !SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        SigScm_Error("car : empty list\n");
#endif
    if (NULLP(obj))
        return SCM_NULL;

    if (!CONSP(obj))
        SigScm_ErrorObj("car : list required but got ", obj);

    return CAR(obj);
}

ScmObj ScmOp_cdr(ScmObj obj)
{
    /*
     * TODO: fixme! : Kazuki Ohta <mover@hct.zaq.ne.jp>
     *
     * In R5RS (car '()) becomes an error, but current uim assumes (car '()) == ()
     * in many places. So, I decided to change ScmOp_car to SIOD like behavior.
     * 
     */
#if !SCM_COMPAT_SIOD_BUGS
    if (NULLP(obj))
        SigScm_Error("cdr : empty list\n");
#endif
    if (NULLP(obj))
        return SCM_NULL;

    if (!CONSP(obj))
        SigScm_ErrorObj("cdr : list required but got ", obj);

    return CDR(obj);
}

ScmObj ScmOp_pairp(ScmObj obj)
{
    return (CONSP(obj)) ? SCM_TRUE : SCM_FALSE;
}

ScmObj ScmOp_cons(ScmObj car, ScmObj cdr)
{
    return Scm_NewCons(car, cdr);
}

ScmObj ScmOp_setcar(ScmObj pair, ScmObj car)
{
    if (CONSP(pair))
        SET_CAR(pair, car);
    else
        SigScm_ErrorObj("set-car! : pair required but got ", pair);

#if SCM_COMPAT_SIOD
    return car;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_setcdr(ScmObj pair, ScmObj cdr)
{
    if (CONSP(pair))
        SET_CDR(pair, cdr);
    else
        SigScm_ErrorObj("set-cdr! : pair required but got ", pair);

#if SCM_COMPAT_SIOD
    return cdr;
#else
    return SCM_UNDEF;
#endif
}

ScmObj ScmOp_caar(ScmObj pair)
{
    return ScmOp_car( ScmOp_car(pair) );
}
ScmObj ScmOp_cadr(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr(pair) );
}
ScmObj ScmOp_cdar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car(pair) );
}
ScmObj ScmOp_cddr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr(pair) );
}
ScmObj ScmOp_caaar(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_car(pair) ));
}
ScmObj ScmOp_caadr(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr(pair) ));
}
ScmObj ScmOp_cadar(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car(pair) ));
}
ScmObj ScmOp_caddr(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr(pair) ));
}
ScmObj ScmOp_cdaar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car(pair) ));
}
ScmObj ScmOp_cdadr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr(pair) ));
}
ScmObj ScmOp_cddar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car(pair) ));
}
ScmObj ScmOp_cdddr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(pair) ));
}
ScmObj ScmOp_caaaar(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_car(pair) )));
}
ScmObj ScmOp_caaadr(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_car( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_caadar(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_car(pair) )));
}
ScmObj ScmOp_caaddr(ScmObj pair)
{
    return ScmOp_car( ScmOp_car( ScmOp_cdr( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_cadaar(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_car(pair) )));
}
ScmObj ScmOp_cadadr(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_car( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_caddar(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_car(pair) )));
}
ScmObj ScmOp_cadddr(ScmObj pair)
{
    return ScmOp_car( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_cdaaar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_car(pair) )));
}
ScmObj ScmOp_cdaadr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_car( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_cdadar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_car(pair) )));
}
ScmObj ScmOp_cdaddr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_car( ScmOp_cdr( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_cddaar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_car(pair) )));
}
ScmObj ScmOp_cddadr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_car( ScmOp_cdr(pair) )));
}
ScmObj ScmOp_cdddar(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_car(pair) )));
}
ScmObj ScmOp_cddddr(ScmObj pair)
{
    return ScmOp_cdr( ScmOp_cdr( ScmOp_cdr( ScmOp_cdr(pair) )));
}

ScmObj ScmOp_list(ScmObj obj, ScmObj env)
{
    return obj;
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
static int ScmOp_c_length(ScmObj obj)
{
    ScmObj slow = obj;
    int len = 0;

    for (;;) {
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return -1;
        if (len != 0 && obj == slow) return -1; /* circular */

        obj = CDR(obj);
        len++;
        if (NULLP(obj)) break;
        if (!CONSP(obj)) return -1;
        if (obj == slow) return -1; /* circular */

        obj = CDR(obj);
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

ScmObj ScmOp_append(ScmObj args, ScmObj env)
{
    ScmObj ret_list = SCM_NULL;
    ScmObj *ret_tail = &ret_list;

    ScmObj ls;
    ScmObj obj = SCM_NULL;

    if (NULLP(args))
        return SCM_NULL;

    /* duplicate and merge all but the last argument */
    for (; !NULLP(CDR(args)); args = CDR(args)) {
        for (ls = CAR(args); CONSP(ls); ls = CDR(ls)) {
            obj = CAR(ls);
            *ret_tail = Scm_NewCons(obj, SCM_NULL);
            ret_tail = &CDR(*ret_tail);
        }
        if (!NULLP(ls))
            SigScm_ErrorObj("append: proper list required but got: ",
                            CAR(args));
    }

    /* append the last argument */
    *ret_tail = CAR(args);

    return ret_list;
}

ScmObj ScmOp_reverse(ScmObj list)
{
    ScmObj ret_list  = SCM_NULL;

    for (; CONSP(list); list = CDR(list))
        ret_list = Scm_NewCons(CAR(list), ret_list);

    if (!NULLP(list))
        SigScm_ErrorObj("reverse: got improper list: ", list);

    return ret_list;
}

static ScmObj ScmOp_listtail_internal(ScmObj list, int k)
{
    while (k--) {
        if (!CONSP(list))
            return SCM_INVALID;
        list = CDR(list);
    }

    return list;
}

ScmObj ScmOp_list_tail(ScmObj list, ScmObj scm_k)
{
    ScmObj ret;

    if (FALSEP(ScmOp_numberp(scm_k)))
        SigScm_ErrorObj("list-tail: number required but got ", scm_k);

    ret = ScmOp_listtail_internal(list, SCM_INT_VALUE(scm_k));

    if (EQ(ret, SCM_INVALID))
        SigScm_ErrorObj("list-tail: out of range or bad list, arglist is: ",
                        Scm_NewCons(list, scm_k));
    return ret;
}

ScmObj ScmOp_list_ref(ScmObj list, ScmObj scm_k)
{
    ScmObj list_tail = SCM_NULL;

    if (FALSEP(ScmOp_numberp(scm_k)))
        SigScm_ErrorObj("list-ref : int required but got ", scm_k);

    list_tail = ScmOp_listtail_internal(list, SCM_INT_VALUE(scm_k));
    if (EQ(list_tail, SCM_INVALID))
        SigScm_ErrorObj("list-ref : out of range or bad list, arglist is: ",
                        Scm_NewCons(list, scm_k));

    return CAR(list_tail);
}

ScmObj ScmOp_memq(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NULL;
    for (tmplist = list; CONSP(tmplist); tmplist = CDR(tmplist)) {
        if (EQ(obj, CAR(tmplist))) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_memv(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    for (tmplist = list; CONSP(tmplist); tmplist = CDR(tmplist)) {
        tmpobj = CAR(tmplist);
        if (NFALSEP(ScmOp_eqvp(obj, tmpobj))) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_member(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    for (tmplist = list; CONSP(tmplist); tmplist = CDR(tmplist)) {
        tmpobj = CAR(tmplist);
        if (NFALSEP(ScmOp_equalp(obj, tmpobj))) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assq(ScmObj obj, ScmObj alist)
{
    ScmObj tmplist = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmplist = alist; CONSP(tmplist); tmplist = CDR(tmplist)) {
        tmpobj = CAR(tmplist);
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
    ScmObj tmplist = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmplist = alist; CONSP(tmplist); tmplist = CDR(tmplist)) {
        tmpobj = CAR(tmplist);
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
    ScmObj tmplist = SCM_NULL;
    ScmObj tmpobj  = SCM_NULL;
    ScmObj car;

    for (tmplist = alist; CONSP(tmplist); tmplist = CDR(tmplist)) {
        tmpobj = CAR(tmplist);
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

ScmObj ScmOp_make_string(ScmObj arg, ScmObj env)
{
    int argc = SCM_INT_VALUE(ScmOp_length(arg));
    int len  = 0;
    char  *tmp = NULL;
    ScmObj str = SCM_NULL;
    ScmObj ch  = SCM_NULL;

    /* sanity check */
    if (argc != 1 && argc != 2)
        SigScm_Error("make-string : invalid use\n");
    if (!INTP(CAR(arg)))
        SigScm_ErrorObj("make-string : integer required but got ", CAR(arg));
    if (argc == 2 && !CHARP(CAR(CDR(arg))))
        SigScm_ErrorObj("make-string : character required but got ", CAR(CDR(arg)));

    /* get length */
    len = SCM_INT_VALUE(CAR(arg));
    if (len == 0)
        return Scm_NewStringCopying("");

    /* specify filler */
    if (argc == 1) {
        /* specify length only, so fill string with space(' ') */
        tmp = (char*)malloc(sizeof(char) * (1 + 1));
        tmp[0] = ' ';
        tmp[1] = '\0';
        ch = Scm_NewChar(tmp);
    } else {
        /* also specify filler char */
        ch = CAR(CDR(arg));
    }

    /* make string */
    str = Scm_NewStringWithLen(NULL, len);

    /* and fill! */
    ScmOp_string_fill(str, ch);

    return str;
}

ScmObj ScmOp_string(ScmObj arg, ScmObj env)
{
    return ScmOp_list2string(arg);
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

ScmObj ScmOp_string_append(ScmObj arg, ScmObj env)
{
    int total_size = 0;
    int total_len  = 0;
    ScmObj strings = SCM_NULL;
    ScmObj obj     = SCM_NULL;
    char  *new_str = NULL;
    char  *p       = NULL;

    /* sanity check */
    if (NULLP(arg))
        return Scm_NewStringCopying("");

    /* count total size of the new string */
    for (strings = arg; !NULLP(strings); strings = CDR(strings)) {
        obj = CAR(strings);
        if (!STRINGP(obj))
            SigScm_ErrorObj("string-append : list required but got ", obj);

        total_size += strlen(SCM_STRING_STR(obj));
        total_len  += SCM_STRING_LEN(obj);
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy string by string */
    p = new_str;
    for (strings = arg; !NULLP(strings); strings = CDR(strings)) {
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

        next = Scm_NewCons(Scm_NewChar(new_ch), SCM_NULL);
        if (prev)
            SET_CDR(prev, next);
        else
            head = next;

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list2string(ScmObj list)
{
    int total_size = 0;
    ScmObj chars   = SCM_NULL;
    ScmObj obj     = SCM_NULL;
    char  *new_str = NULL;
    char  *p       = NULL;

    if (FALSEP(ScmOp_listp(list)))
        SigScm_ErrorObj("list->string : list required but got ", list);

    if (NULLP(list))
        return Scm_NewStringCopying("");

    /* count total size of the string */
    for (chars = list; !NULLP(chars); chars = CDR(chars)) {
        obj = CAR(chars);
        if (!CHARP(obj))
            SigScm_ErrorObj("list->string : char required but got ", obj);

        total_size += strlen(SCM_CHAR_VALUE(obj));
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy char by char */
    p = new_str;
    for (chars = list; !NULLP(chars); chars = CDR(chars)) {
        obj = CAR(chars);

        strcpy(p, SCM_CHAR_VALUE(obj));
        p += strlen(SCM_CHAR_VALUE(obj));
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
    new_str   = (char*)realloc(SCM_STRING_STR(string), sizeof(char) * str_len * char_size + 1);
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

ScmObj ScmOp_make_vector(ScmObj arg, ScmObj env )
{
    ScmObj *vec   = NULL;
    ScmObj  scm_k = CAR(arg);
    ScmObj  fill  = SCM_NULL;
    int c_k = 0;
    int i   = 0;

    if (!INTP(scm_k))
        SigScm_ErrorObj("make-vector : integer required but got ", scm_k);

    /* allocate vector */
    c_k = SCM_INT_VALUE(scm_k);
    vec = (ScmObj*)malloc(sizeof(ScmObj) * c_k);

    /* fill vector */
    fill = SCM_UNDEF;
    if (!NULLP(CDR(arg)))
        fill = CAR(CDR(arg));

    for (i = 0; i < c_k; i++) {
        vec[i] = fill;
    }

    return Scm_NewVector(vec, c_k);
}

ScmObj ScmOp_vector(ScmObj arg, ScmObj env )
{
    ScmObj scm_len = ScmOp_length(arg);
    int c_len      = SCM_INT_VALUE(scm_len);
    ScmObj *vec    = (ScmObj*)malloc(sizeof(ScmObj) * c_len); /* allocate vector */

    /* set item */
    int i = 0;
    for (i = 0; i < c_len; i++) {
        vec[i] = CAR(arg);
        arg = CDR(arg);
    }

    return Scm_NewVector(vec, c_len);
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
        next = Scm_NewCons(v[i], SCM_NULL);

        if (prev) {
            SET_CDR(prev, next);
        } else {
            head = next;
        }

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list2vector(ScmObj list)
{
    ScmObj  scm_len = SCM_NULL;
    ScmObj *v       = NULL;
    int c_len = 0;
    int i = 0;

    /* TOOD : canbe optimized. scanning list many times */
    if (FALSEP(ScmOp_listp(list)))
        SigScm_ErrorObj("list->vector : list required but got ", list);

    scm_len = ScmOp_length(list);
    c_len   = SCM_INT_VALUE(scm_len);
    v       = (ScmObj*)malloc(sizeof(ScmObj) * c_len);
    for (i = 0; i < c_len; i++) {
        v[i] = CAR(list);
        list = CDR(list);
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
    if (FUNCP(obj) || CLOSUREP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_map(ScmObj map_arg, ScmObj env)
{
    int arg_len = SCM_INT_VALUE(ScmOp_length(map_arg));
    ScmObj proc = CAR(map_arg);
    ScmObj args = SCM_NULL;
    ScmObj ret  = SCM_NULL;
    ScmObj tmp  = SCM_NULL;

    ScmObj arg_vector = SCM_NULL;
    ScmObj arg1       = SCM_NULL;
    int vector_len = 0;
    int i = 0;

    /* arglen check */
    if (arg_len < 2)
        SigScm_Error("map : Wrong number of arguments\n");

    /* 1proc and 1arg case */
    if (arg_len == 2) {
        /* apply func to each item */
        for (args = CAR(CDR(map_arg)); !NULLP(args); args = CDR(args)) {
            /* create proc's arg */
            tmp = CAR(args);

            /* create list for "apply" op */
            tmp = SCM_LIST_2(proc,
                             Scm_NewCons(tmp, SCM_NULL));

            /* apply proc */
            ret = Scm_NewCons(ScmOp_apply(tmp, env), ret);
        }
        return ScmOp_reverse(ret);
    }

    /* 1proc and many args case */
    arg_vector = ScmOp_list2vector(CDR(map_arg));
    vector_len = SCM_VECTOR_LEN(arg_vector);
    while (1) {
        /* create arg */
        arg1 = SCM_NULL;
        for (i = 0; i < vector_len; i++) {
            tmp  = SCM_VECTOR_CREF(arg_vector, i);
            /* check if we can continue next loop */
            if (NULLP(tmp)) {
                /* if next item is SCM_NULL, let's return! */
                return ScmOp_reverse(ret);
            }

            arg1 = Scm_NewCons(CAR(tmp), arg1);
            SCM_VECTOR_SET_CREF(arg_vector, i, CDR(tmp));
        }

        /* reverse arg */
        arg1 = ScmOp_reverse(arg1);

        /* apply proc to arg1 */
        ret = Scm_NewCons(ScmOp_apply(SCM_LIST_2(proc, arg1), env),
                          ret);
    }

    /* never reaches here */
    SigScm_Error("map bug?\n");
    return SCM_NULL;
}

ScmObj ScmOp_for_each(ScmObj arg, ScmObj env)
{
    ScmOp_map(arg, env);

    return SCM_UNDEF;
}

ScmObj ScmOp_force(ScmObj arg, ScmObj env)
{
    if (SCM_INT_VALUE(ScmOp_length(arg)) != 1)
        SigScm_Error("force : Wrong number of arguments\n");
    if (!CLOSUREP(CAR(arg)))
        SigScm_Error("force : not proper delayed object\n");

    /* the caller's already wrapped arg in a list for us */
    return ScmOp_eval(arg, env);
}

ScmObj ScmOp_call_with_current_continuation(ScmObj arg, ScmObj env)
{
    int jmpret  = 0;
    ScmObj proc = CAR(arg);
    ScmObj cont = SCM_NULL;

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
    SET_CDR(arg, Scm_NewCons(cont, SCM_NULL));

    return ScmOp_eval(arg, env);
}

ScmObj ScmOp_values(ScmObj argl, ScmObj env)
{
    /* Values with one arg must return something that fits an ordinary
     * continuation. */
    if (CONSP(argl) && NULLP(CDR(argl)))
        return CAR(argl);

    /* Otherwise, we'll return the values in a packet. */
    return Scm_NewValuePacket(argl);
}

ScmObj ScmOp_call_with_values(ScmObj argl, ScmObj *envp)
{
    ScmObj vals;
    ScmObj cons_wrapper;

    if (CHECK_2_ARGS(argl))
        SigScm_ErrorObj("call-with-values: too few arguments: ", argl);

    /* make the list (producer) and evaluate it */
    cons_wrapper = Scm_NewCons(CAR(argl), SCM_NULL);
    vals = ScmOp_eval(cons_wrapper, *envp);

    if (!VALUEPACKETP(vals)) {
        /* got back a single value */
        vals = Scm_NewCons(vals, SCM_NULL);
    } else {
        /* extract */
        vals = SCM_VALUEPACKET_VALUES(vals);
    }
    
    /* cons_wrapper would have no chance of being referenced from
     * anywhere else, so we'll reuse that object. */
    SET_CAR(cons_wrapper, SCM_CADR(argl));
    SET_CDR(cons_wrapper, vals);
    return cons_wrapper;
}

#if SCM_USE_SRFI1
#include "operations-srfi1.c"
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
#if SCM_COMPAT_SIOD
#include "operations-siod.c"
#endif
