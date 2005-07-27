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

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
extern ScmObj continuation_thrown_obj;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj list_gettail(ScmObj head);
static ScmObj ScmOp_listtail_internal(ScmObj obj, int k);
static ScmObj ScmOp_append_internal(ScmObj head, ScmObj tail);

/*=======================================
  Function Implementations
=======================================*/
/*==============================================================================
  R5RS : 6.1 Equivalence predicates
==============================================================================*/
ScmObj ScmOp_eqvp(ScmObj obj1, ScmObj obj2)
{
    enum ScmObjType type = (enum ScmObjType)SCM_GETTYPE(obj1);

    /* different type */
    if (type != SCM_GETTYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
        case ScmInt:
            /* both numbers, are numerically equal */
            if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2)))
            {
                return SCM_TRUE;
            }
            break;
        case ScmSymbol:
            /* symbols which have same name */
            if (strcmp(SCM_SYMBOL_NAME(obj1), SCM_SYMBOL_NAME(obj2)) == 0)
            {
                return SCM_TRUE;
            }
            break;
        case ScmChar:
            /* chars and are the same character according to the char=? */
            if (EQ(ScmOp_char_equal(obj1, obj2), SCM_TRUE))
            {
                return SCM_TRUE;
            }
            break;
        case ScmCons:
        case ScmVector:
        case ScmString:
        case ScmFunc:
        case ScmClosure:
	case ScmPort:
	case ScmContinuation:
            if (EQ(obj1, obj2))
            {
                return SCM_TRUE;
            }
            break;
        case ScmEtc:
            /* obj1 and obj2 are both #t or both #f */
            if (((EQ(obj1, SCM_TRUE) && EQ(obj2, SCM_TRUE)))
                || (EQ(obj1, SCM_FALSE) && EQ(obj2, SCM_FALSE)))
            {
                return SCM_TRUE;
            }
            /* both obj1 and obj2 are the empty list */
            if (SCM_NULLP(obj1) && SCM_NULLP(obj2))
            {
                return SCM_TRUE;
            }
            break;
        case ScmFreeCell:
            SigScm_Error("eqv? : cannnot compare freecell, gc broken?\n");
            break;
	case ScmCPointer:
	case ScmCFuncPointer:
	    if (EQ(obj1, obj2))
	    {
		return SCM_TRUE;
	    }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_eqp(ScmObj obj1, ScmObj obj2)
{
    return ScmOp_eqvp(obj1, obj2);
}

ScmObj ScmOp_equalp(ScmObj obj1, ScmObj obj2)
{
    int  i = 0;
    enum ScmObjType type = (enum ScmObjType)SCM_GETTYPE(obj1);

    /* different type */
    if (type != SCM_GETTYPE(obj2))
        return SCM_FALSE;

    /* same type */
    switch (type) {
        case ScmInt:
            /* both numbers, are numerically equal */
            if ((SCM_INT_VALUE(obj1) == SCM_INT_VALUE(obj2)))
            {
                return SCM_TRUE;
            }
            break;
        case ScmSymbol:
            /* symbols which have same name */
            if (strcmp(SCM_SYMBOL_NAME(obj1), SCM_SYMBOL_NAME(obj2)) == 0)
            {
                return SCM_TRUE;
            }
            break;
        case ScmChar:
            /* chars and are the same character according to the char=? */
            if (EQ(ScmOp_char_equal(obj1, obj2), SCM_TRUE))
	    {
                return SCM_TRUE;
            }
            break;
        case ScmCons:
	    for (; !SCM_NULLP(obj1); obj1 = SCM_CDR(obj1), obj2 = SCM_CDR(obj2))
	    {
		/* check contents */
		if (EQ(ScmOp_equalp(SCM_CAR(obj1), SCM_CAR(obj2)), SCM_FALSE))
		{
		    return SCM_FALSE;
		}

		/* check next cdr's type */
		if (SCM_GETTYPE(SCM_CDR(obj1)) != SCM_GETTYPE(SCM_CDR(obj2)))
		{
		    return SCM_FALSE;
		}
		
		/* check dot pair */
		if (!SCM_CONSP(SCM_CDR(obj1)))
		{
		    if(EQ(ScmOp_equalp(SCM_CDR(obj1), SCM_CDR(obj2)), SCM_FALSE))
			return SCM_FALSE;
		    else
			return SCM_TRUE;
		}
	    }
	    return SCM_TRUE;
        case ScmVector:
	    /* check len */
	    if (SCM_VECTOR_LEN(obj1) != SCM_VECTOR_LEN(obj2))
	    {
		return SCM_FALSE;
	    }
	    /* check contents */
	    for (i = 0; i < SCM_VECTOR_LEN(obj1); i++)
	    {
		if (EQ(ScmOp_equalp(SCM_VECTOR_CREF(obj1, i), SCM_VECTOR_CREF(obj2, i)), SCM_FALSE))
		    return SCM_FALSE;
	    }
	    return SCM_TRUE;
        case ScmString:
	    /* check string data */
	    if (strcmp(SCM_STRING_STR(obj1), SCM_STRING_STR(obj2)) == 0)
	    {
		return SCM_TRUE;
	    }
	    break;
        case ScmFunc:
        case ScmClosure:
	case ScmPort:
	case ScmContinuation:
            {
                return SCM_UNSPECIFIED;
            }
            break;
        case ScmEtc:
            /* obj1 and obj2 are both #t or both #f */
            if (((EQ(obj1, SCM_TRUE) && EQ(obj2, SCM_TRUE)))
                || (EQ(obj1, SCM_FALSE) && EQ(obj2, SCM_FALSE)))
            {
                return SCM_TRUE;
            }
            /* both obj1 and obj2 are the empty list */
            if (SCM_NULLP(obj1) && SCM_NULLP(obj2))
            {
                return SCM_TRUE;
            }
            break;
        case ScmFreeCell:
            SigScm_Error("equal? : cannnot compare freecell, gc broken?\n");
            break;
	case ScmCPointer:
	    if (SCM_C_POINTER_DATA(obj1) == SCM_C_POINTER_DATA(obj2))
	    {
		return SCM_TRUE;
	    }
	    break;
	case ScmCFuncPointer:
	    if (SCM_C_FUNCPOINTER_FUNC(obj1) == SCM_C_FUNCPOINTER_FUNC(obj2))
	    {
		return SCM_TRUE;
	    }
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
ScmObj ScmOp_plus2n(ScmObj obj1, ScmObj obj2)
{
    if (SCM_NULLP(obj1) && SCM_NULLP(obj2))
	return Scm_NewInt(0);

    if (!SCM_INTP(obj1))
        SigScm_ErrorObj("+ : integer required but got ", obj1);

    if (SCM_NULLP(obj2))
	return Scm_NewInt(SCM_INT_VALUE(obj1));

    if (!SCM_INTP(obj2))
	SigScm_ErrorObj("+ : integer required but got ", obj2);
    
    return Scm_NewInt(SCM_INT_VALUE(obj1) + SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_minus2n(ScmObj obj1, ScmObj obj2)
{
    if (!SCM_INTP(obj1))
        SigScm_ErrorObj("- : integer required but got ", obj1);

    if (SCM_NULLP(obj2))
	return Scm_NewInt(-(SCM_INT_VALUE(obj1)));

    if (!SCM_INTP(obj2))
        SigScm_ErrorObj("- : integer required but got ", obj2);
	
    return Scm_NewInt(SCM_INT_VALUE(obj1) - SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_multi2n(ScmObj obj1, ScmObj obj2)
{
    if (SCM_NULLP(obj1) && SCM_NULLP(obj2))
	return Scm_NewInt(1);

    if (!SCM_INTP(obj1))
        SigScm_ErrorObj("* : integer required but got ", obj1);

    if (SCM_NULLP(obj2))
	return Scm_NewInt(SCM_INT_VALUE(obj1));

    if (!SCM_INTP(obj2))
        SigScm_ErrorObj("* : integer required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) * SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_divide2n(ScmObj obj1, ScmObj obj2)
{
    if (!SCM_INTP(obj1))
        SigScm_ErrorObj("/ : integer required but got ", obj1);
    if (!SCM_INTP(obj2))
        SigScm_ErrorObj("/ : integer required but got ", obj2);
    if (EQ(ScmOp_zerop(obj2), SCM_TRUE))
        SigScm_Error("/ : divide by zero\n");

    return Scm_NewInt(SCM_INT_VALUE(obj1) / SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_numberp(ScmObj obj)
{
    if (SCM_INTP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_equal(ScmObj args, ScmObj env)
{
    int    val = 0;
    ScmObj obj = SCM_NIL;

    /* type check */
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
        SigScm_ErrorObj("= : number required but got ", SCM_CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("= : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(SCM_CAR(args));

    /* compare following value */
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
        obj = SCM_CAR(args);
        if (EQ(ScmOp_numberp(obj), SCM_FALSE))
            SigScm_ErrorObj("= : number required but got ", obj);

        if (SCM_INT_VALUE(obj) != val)
        {
            return SCM_FALSE;
        }
    }

    return SCM_TRUE;
}

ScmObj ScmOp_bigger(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NIL;

    if (SCM_NULLP(args) || SCM_NULLP(SCM_CDR(args)))
        SigScm_Error("< : Wrong number of arguments\n");

    /* type check */
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
        SigScm_ErrorObj("< : number required but got ", SCM_CAR(args));

    /* Get first value */
    val = SCM_INT_VALUE(SCM_CAR(args));

    /* compare following value */
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
        obj = SCM_CAR(args);
        if (EQ(ScmOp_numberp(obj), SCM_FALSE))
            SigScm_ErrorObj("< : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val < car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_smaller(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NIL;

    /* type check */
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
        SigScm_ErrorObj("> : number required but got ", SCM_CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("> : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(SCM_CAR(args));

    /* compare following value */
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
        obj = SCM_CAR(args);
        if (EQ(ScmOp_numberp(obj), SCM_FALSE))
            SigScm_ErrorObj("> : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val > car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_biggerEq(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NIL;

    /* type check */
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
        SigScm_ErrorObj("<= : number required but got ", SCM_CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error("<= : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(SCM_CAR(args));

    /* compare following value */
    obj = SCM_NIL;
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
        obj = SCM_CAR(args);
        if (EQ(ScmOp_numberp(obj), SCM_FALSE))
            SigScm_ErrorObj("<= : number required but got ", obj);

        car_val = SCM_INT_VALUE(obj);
        if (val <= car_val)
            val = car_val;
        else
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

ScmObj ScmOp_smallerEq(ScmObj args, ScmObj env )
{
    int    val     = 0;
    int    car_val = 0;
    ScmObj obj     = SCM_NIL;

    /* type check */
    if (EQ(ScmOp_numberp(SCM_CAR(args)), SCM_FALSE))
        SigScm_ErrorObj(">= : number required but got ", SCM_CAR(args));

    /* arglen check */
    if CHECK_2_ARGS(args)
        SigScm_Error(">= : Wrong number of arguments\n");

    /* Get first value */
    val = SCM_INT_VALUE(SCM_CAR(args));

    /* compare following value */
    obj = SCM_NIL;
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
        obj = SCM_CAR(args);
        if (EQ(ScmOp_numberp(obj), SCM_FALSE))
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
    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("zero? : number required but got ", scm_num);

    if (SCM_INT_VALUE(scm_num) == 0)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_positivep(ScmObj scm_num)
{
    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("positive? : number required but got", scm_num);

    if (SCM_INT_VALUE(scm_num) > 0)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_negativep(ScmObj scm_num)
{
    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("negative? : number required but got ", scm_num);

    if (SCM_INT_VALUE(scm_num) < 0)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_oddp(ScmObj scm_num)
{
    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("odd? : number required but got ", scm_num);

    if (SCM_INT_VALUE(scm_num) % 2 == 1)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_evenp(ScmObj scm_num)
{
    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("even? : number required but got ", scm_num);

    if (SCM_INT_VALUE(scm_num) % 2 == 0)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_max(ScmObj args, ScmObj env )
{
    int    max     = 0;
    int    car_val = 0;
    ScmObj car     = SCM_NIL;

    if (SCM_NULLP(args))
	SigScm_Error("max : at least 1 number required\n");

    for (; !SCM_NULLP(args); args = SCM_CDR(args)) {
        car = SCM_CAR(args);
        if (EQ(ScmOp_numberp(car), SCM_FALSE))
            SigScm_ErrorObj("max : number required but got ", car);

        car_val = SCM_INT_VALUE(SCM_CAR(args));
        if (max < car_val)
            max = car_val;
    }

    return Scm_NewInt(max);
}

ScmObj ScmOp_min(ScmObj args, ScmObj env )
{
    int    min     = 0;
    int    car_val = 0;
    ScmObj car     = SCM_NIL;

    if (SCM_NULLP(args))
	SigScm_Error("min : at least 1 number required\n");

    for (; !SCM_NULLP(args); args = SCM_CDR(args)) {
        car = SCM_CAR(args);
        if (EQ(ScmOp_numberp(car), SCM_FALSE))
            SigScm_ErrorObj("min : number required but got ", car);

        car_val = SCM_INT_VALUE(SCM_CAR(args));
        if (car_val < min)
            min = car_val;
    }

    return Scm_NewInt(min);
}


ScmObj ScmOp_abs(ScmObj scm_num)
{
    int num = 0;

    if (EQ(ScmOp_numberp(scm_num), SCM_FALSE))
        SigScm_ErrorObj("abs : number required but got ", scm_num);

    num = SCM_INT_VALUE(scm_num);
    if (0 < num)
        return scm_num;

    return Scm_NewInt(-num);
}

ScmObj ScmOp_quotient(ScmObj scm_n1, ScmObj scm_n2)
{
    int n1 = 0;
    int n2 = 0;

    if (EQ(ScmOp_numberp(scm_n1), SCM_FALSE))
        SigScm_ErrorObj("quotient : number required but got ", scm_n1);
    if (EQ(ScmOp_numberp(scm_n2), SCM_FALSE))
        SigScm_ErrorObj("quotient : number required but got ", scm_n2);
    if (EQ(ScmOp_zerop(scm_n2), SCM_TRUE))
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

    if (EQ(ScmOp_numberp(scm_n1), SCM_FALSE))
        SigScm_ErrorObj("modulo : number required but got ", scm_n1);
    if (EQ(ScmOp_numberp(scm_n2), SCM_FALSE))
        SigScm_ErrorObj("modulo : number required but got ", scm_n2);
    if (EQ(ScmOp_zerop(scm_n2), SCM_TRUE))
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

    if (EQ(ScmOp_numberp(scm_n1), SCM_FALSE))
        SigScm_ErrorObj("remainder : number required but got ", scm_n1);
    if (EQ(ScmOp_numberp(scm_n2), SCM_FALSE))
        SigScm_ErrorObj("remainder : number required but got ", scm_n2);
    if (EQ(ScmOp_zerop(scm_n2), SCM_TRUE))
        SigScm_Error("remainder : divide by zero\n");

    n1 = SCM_INT_VALUE(scm_n1);
    n2 = SCM_INT_VALUE(scm_n2);

    return Scm_NewInt(n1 % n2);
}

/*==============================================================================
  R5RS : 6.2 Numbers : 6.2.6 Numerical input and output
==============================================================================*/
/* TODO : support radix */
ScmObj ScmOp_number_to_string(ScmObj z)
{
    int n = 0;
    int i = 0;
    int size = 0;
    char *str = NULL;

    if (EQ(ScmOp_numberp(z), SCM_FALSE))
	SigScm_ErrorObj("number->string : number required but got ", z);

    /* get value */
    n = SCM_INT_VALUE(z);

    /* get size */
    for (size = 1; (int)(n / 10) != 0; size++)
	n /= 10;

    /* allocate str */
    str = (char *)malloc(sizeof(char) * size + 1);

    /* fill str */
    n = SCM_INT_VALUE(z);
    str[size] = '\0';
    for (i = size; 0 < i; i--) {
	str[i - 1] = '0' + (n % 10);
	n /= 10;
    }

    return Scm_NewString(str);
}

/* TODO : support radix */
ScmObj ScmOp_string_to_number(ScmObj string)
{
    if (!SCM_STRINGP(string))
	SigScm_ErrorObj("string->number : string required but got ", string);

    return Scm_NewInt((int)atof(SCM_STRING_STR(string)));
}

/*===================================
  R5RS : 6.3 Other data types
===================================*/
/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.1 Booleans
==============================================================================*/
ScmObj ScmOp_not(ScmObj obj)
{
    if (EQ(obj, SCM_FALSE))
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_booleanp(ScmObj obj)
{
    if (EQ(obj, SCM_FALSE) || EQ(obj, SCM_TRUE))
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.2 Pairs and lists
==============================================================================*/
ScmObj ScmOp_car(ScmObj obj)
{
    if (SCM_NULLP(obj))
        SigScm_Error("car : empty list\n");
    if (!SCM_CONSP(obj))
        SigScm_ErrorObj("car : list required but got ", obj);

    return SCM_CAR(obj);
}

ScmObj ScmOp_cdr(ScmObj obj)
{
    if (SCM_NULLP(obj))
        SigScm_Error("car : empty list");
    if (!SCM_CONSP(obj))
        SigScm_ErrorObj("car : list required but got ", obj);

    return SCM_CDR(obj);
}

ScmObj ScmOp_pairp(ScmObj obj)
{
    if (SCM_CONSP(obj))
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

ScmObj ScmOp_cons(ScmObj car, ScmObj cdr)
{
    return Scm_NewCons(car, cdr);
}

ScmObj ScmOp_setcar(ScmObj pair, ScmObj car)
{
    if (SCM_CONSP(pair))
        SCM_SETCAR(pair, car);
    else
        SigScm_ErrorObj("set-car! : pair required but got ", pair);

    return SCM_UNSPECIFIED;
}

ScmObj ScmOp_setcdr(ScmObj pair, ScmObj cdr)
{
    if (SCM_CONSP(pair))
        SCM_SETCDR(pair, cdr);
    else
        SigScm_ErrorObj("set-cdr! : pair required but got ", pair);

    return SCM_UNSPECIFIED;
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
    if (SCM_NULLP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_listp(ScmObj obj)
{
    for (; !SCM_NULLP(obj); obj = SCM_CDR(obj)) {
        /* check if valid list */
        if (!SCM_CONSP(obj))
            return SCM_FALSE;
    }

    return SCM_TRUE;
}

static ScmObj list_gettail(ScmObj head)
{
    ScmObj tail = head;

    if (SCM_NULLP(head)) return SCM_NIL;

    while (1) {
        if (!SCM_CONSP(tail) || SCM_NULLP(SCM_CDR(tail)))
            return tail;

        tail = SCM_CDR(tail);
    }

    return SCM_NIL;
}

ScmObj ScmOp_length(ScmObj obj)
{
    int length = 0;
    for (; !SCM_NULLP(obj); obj = SCM_CDR(obj)) {
        /* check if valid list */
        if (!SCM_NULLP(obj) && !SCM_CONSP(obj))
            SigScm_ErrorObj("length : bad list. given obj contains ", obj);

        length++;
    }

    return Scm_NewInt(length);
}

ScmObj ScmOp_append_internal(ScmObj head, ScmObj tail)
{
    ScmObj head_tail = SCM_NIL;

    /* TODO : need to rewrite using ScmOp_listp? */
    if (SCM_NULLP(head))
        return tail;

    if (!SCM_CONSP(head))
        SigScm_ErrorObj("append : list required but got ", head);

    head_tail = list_gettail(head);
    if (SCM_NULLP(head_tail)) {
        return tail;
    } else if (SCM_CONSP(head_tail)) {
        SCM_SETCDR(head_tail, tail);
    } else {
        SigScm_ErrorObj("append : list required but got ", head_tail);
    }

    return head;
}

ScmObj ScmOp_append(ScmObj args, ScmObj env)
{
    ScmObj ret = SCM_NIL;
    ScmObj obj = SCM_NIL;
    for (; !SCM_NULLP(args); args = SCM_CDR(args)) {
	obj = SCM_CAR(args);
	ret = ScmOp_append_internal(ret, obj);
    }

    return ret;
}

ScmObj ScmOp_reverse(ScmObj list)
{
    ScmObj ret_list  = SCM_NIL;

    /* TODO : canbe optimized not to use ScmOp_listp */
    if (EQ(ScmOp_listp(list), SCM_FALSE))
        SigScm_ErrorObj("reverse : list required but got ", list);

    for (; !SCM_NULLP(list); list = SCM_CDR(list)) {
        ret_list = Scm_NewCons(SCM_CAR(list), ret_list);
    }

    return ret_list;
}

/* TODO : not to use recursive call for avoiding stack overflow*/
ScmObj ScmOp_listtail_internal(ScmObj obj, int k)
{
    if (k == 0) {
        return obj;
    }

    if (SCM_NULLP(obj))
        SigScm_Error("already reached tail\n");

    return ScmOp_listtail_internal(SCM_CDR(obj), k - 1);
}

ScmObj ScmOp_listtail(ScmObj list, ScmObj scm_k)
{
    if (EQ(ScmOp_listp(list), SCM_FALSE))
        SigScm_ErrorObj("list-tail : list required but got ", list);
    if (EQ(ScmOp_numberp(scm_k), SCM_FALSE))
        SigScm_ErrorObj("list-tail : number required but got ", scm_k);

    return ScmOp_listtail_internal(list, SCM_INT_VALUE(scm_k));
}

ScmObj ScmOp_listref(ScmObj list, ScmObj scm_k)
{
    ScmObj list_tail = SCM_NIL;

    if (EQ(ScmOp_listp(list), SCM_FALSE))
        SigScm_ErrorObj("list-ref : list required but got ", list);
    if (EQ(ScmOp_numberp(scm_k), SCM_FALSE))
        SigScm_ErrorObj("list-ref : int required but got ", scm_k);

    list_tail = ScmOp_listtail_internal(list, SCM_INT_VALUE(scm_k));
    if (SCM_NULLP(list_tail)) {
        SigScm_Error("list-ref : out of range\n");
    }

    return SCM_CAR(list_tail);
}

ScmObj ScmOp_memq(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = list; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (EQ(ScmOp_eqp(obj, tmpobj), SCM_TRUE)) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_memv(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = list; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (EQ(ScmOp_eqvp(obj, tmpobj), SCM_TRUE)) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_member(ScmObj obj, ScmObj list)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = list; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (EQ(ScmOp_equalp(obj, tmpobj), SCM_TRUE)) {
            return tmplist;
        }
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assq(ScmObj obj, ScmObj alist)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = alist; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (SCM_CONSP(tmpobj) && EQ(ScmOp_eqp(SCM_CAR(tmpobj), obj), SCM_TRUE))
            return tmpobj;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assv(ScmObj obj, ScmObj alist)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = alist; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (SCM_CONSP(tmpobj) && EQ(ScmOp_eqvp(SCM_CAR(tmpobj), obj), SCM_TRUE))
            return tmpobj;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_assoc(ScmObj obj, ScmObj alist)
{
    ScmObj tmplist = SCM_NIL;
    ScmObj tmpobj  = SCM_NIL;
    for (tmplist = alist; SCM_CONSP(tmplist); tmplist = SCM_CDR(tmplist)) {
        tmpobj = SCM_CAR(tmplist);
        if (SCM_CONSP(tmpobj) && EQ(ScmOp_equalp(SCM_CAR(tmpobj), obj), SCM_TRUE))
            return tmpobj;
    }

    return SCM_FALSE;
}


/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.3 Symbols
==============================================================================*/
ScmObj ScmOp_symbolp(ScmObj obj)
{
    if (SCM_SYMBOLP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_boundp(ScmObj obj)
{
    if (SCM_SYMBOLP(obj)
        && !SCM_EQ(SCM_SYMBOL_VCELL(obj), SCM_UNBOUND))
    {
        return SCM_TRUE;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_symbol_to_string(ScmObj obj)
{
    if (!SCM_SYMBOLP(obj))
        return SCM_FALSE;

    return Scm_NewStringCopying(SCM_SYMBOL_NAME(obj));
}

ScmObj ScmOp_string_to_symbol(ScmObj str)
{
    char *name = NULL;

    if(!SCM_STRINGP(str))
        return SCM_FALSE;

    name = (char*)alloca(strlen(SCM_STRING_STR(str)) + 1);
    strcpy(name, SCM_STRING_STR(str));

    return Scm_Intern(name);
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.4 Characters
==============================================================================*/
ScmObj ScmOp_charp(ScmObj obj)
{
    if (SCM_CHARP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_equal(ScmObj ch1, ScmObj ch2)
{
    if (!SCM_CHARP(ch1))
        SigScm_ErrorObj("char=? : char required but got ", ch1);
    if (!SCM_CHARP(ch2))
        SigScm_ErrorObj("char=? : char required but got ", ch2);

    if (strcmp(SCM_CHAR_CH(ch1), SCM_CHAR_CH(ch2)) == 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_alphabeticp(ScmObj obj)
{
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_CH(obj)) != 1)
        return SCM_FALSE;

    /* check alphabet */
    if (isalpha(SCM_CHAR_CH(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_numericp(ScmObj obj)
{
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_CH(obj)) != 1)
        return SCM_FALSE;

    /* check digit */
    if (isdigit(SCM_CHAR_CH(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_whitespacep(ScmObj obj)
{
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_CH(obj)) != 1)
        return SCM_FALSE;

    /* check space */
    if (isspace(SCM_CHAR_CH(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_upper_casep(ScmObj obj)
{
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_CH(obj)) != 1)
        return SCM_FALSE;

    /* check uppercase */
    if (isupper(SCM_CHAR_CH(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_char_lower_casep(ScmObj obj)
{
    if (!SCM_CHARP(obj))
        SigScm_ErrorObj("char-alphabetic? : char required but got ", obj);

    /* check multibyte */
    if (strlen(SCM_CHAR_CH(obj)) != 1)
        return SCM_FALSE;

    /* check lowercase */
    if (islower(SCM_CHAR_CH(obj)[0]) != 0)
        return SCM_TRUE;

    return SCM_FALSE;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.5 Strings
==============================================================================*/
ScmObj ScmOp_stringp(ScmObj obj)
{
    if (SCM_STRINGP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_make_string(ScmObj arg, ScmObj env)
{
    int argc = SCM_INT_VALUE(ScmOp_length(arg));
    int len  = 0;
    ScmObj str = SCM_NIL;
    ScmObj ch  = SCM_NIL;

    if (argc != 1 && argc != 2)
        SigScm_Error("make-string : invalid use\n");
    if (!SCM_INTP(SCM_CAR(arg)))
        SigScm_ErrorObj("make-string : integer required but got ", SCM_CAR(arg));
    if (argc == 2 && !SCM_CHARP(SCM_CAR(SCM_CDR(arg))))
        SigScm_ErrorObj("make-string : character required but got ", SCM_CAR(SCM_CDR(arg)));

    len = SCM_INT_VALUE(SCM_CAR(arg));
    if (argc == 1) {
        return Scm_NewString_With_StrLen(NULL, len);
    }

    str = Scm_NewString_With_StrLen(NULL, len);
    ch  = SCM_CAR(SCM_CDR(arg));
    ScmOp_string_fill(str, ch);

    return str;
}

ScmObj ScmOp_string(ScmObj arg, ScmObj env)
{
    return ScmOp_list_to_string(arg);
}

ScmObj ScmOp_string_length(ScmObj str)
{
    if (!SCM_STRINGP(str))
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

    if (!SCM_STRINGP(str))
        SigScm_ErrorObj("string-ref : string required but got ", str);
    if (!SCM_INTP(k))
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

    if (!SCM_STRINGP(str))
        SigScm_ErrorObj("string-set! : string required but got ", str);
    if (!SCM_INTP(k))
        SigScm_ErrorObj("string-set! : number required but got ", k);
    if (!SCM_CHARP(ch))
        SigScm_ErrorObj("string-set! : character required but got ", ch);

    /* get indexes */
    c_start_index = SCM_INT_VALUE(k);
    string_str    = SCM_STRING_STR(str);
    ch_start_ptr  = SigScm_default_encoding_str_startpos(string_str, c_start_index);
    ch_end_ptr    = SigScm_default_encoding_str_endpos(string_str, c_start_index);

    /* calculate total size */
    front_size = strlen(string_str) - strlen(ch_start_ptr);
    newch_size = strlen(SCM_CHAR_CH(ch));
    back_size  = strlen(ch_end_ptr);
    total_size = front_size + newch_size + back_size;

    /* copy each parts */
    new_str = (char*)malloc(total_size + 1);
    memset(new_str, 0, total_size + 1);
    strncpy(new_str                           , string_str      , front_size);
    strncpy(new_str + front_size              , SCM_CHAR_CH(ch) , newch_size);
    strncpy(new_str + front_size + newch_size , ch_end_ptr      , back_size);

    /* set */
    if (SCM_STRING_STR(str))
        free(SCM_STRING_STR(str));

    SCM_SETSTRING_STR(str, new_str);

    return SCM_UNSPECIFIED;
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

    if (!SCM_STRINGP(str))
        SigScm_ErrorObj("string-ref : string required but got ", str);
    if (!SCM_INTP(start))
        SigScm_ErrorObj("string-ref : number required but got ", start);
    if (!SCM_INTP(end))
        SigScm_ErrorObj("string-ref : number required but got ", end);

    /* get start_ptr and end_ptr */
    c_start_index = SCM_INT_VALUE(start);
    c_end_index   = SCM_INT_VALUE(end);
    string_str    = SCM_STRING_STR(str);
    ch_start_ptr  = SigScm_default_encoding_str_startpos(string_str, c_start_index);
    ch_end_ptr    = SigScm_default_encoding_str_endpos(string_str, c_end_index);

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
    ScmObj strings = SCM_NIL;
    ScmObj obj     = SCM_NIL;
    char  *new_str = NULL;
    char  *p       = NULL;

    /* count total size of the new string */
    for (strings = arg; !SCM_NULLP(strings); strings = SCM_CDR(strings)) {
        obj = SCM_CAR(strings);
        if (!SCM_STRINGP(obj))
            SigScm_ErrorObj("string-append : list required but got ", obj);

        total_size += strlen(SCM_STRING_STR(obj));
        total_len  += SCM_STRING_LEN(obj);
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy string by string */
    p = new_str;
    for (strings = arg; !SCM_NULLP(strings); strings = SCM_CDR(strings)) {
        obj = SCM_CAR(strings);

        strcpy(p, SCM_STRING_STR(obj));
        p += strlen(SCM_STRING_STR(obj));
    }

    return Scm_NewString_With_StrLen(new_str, total_len);
}

ScmObj ScmOp_string_to_list(ScmObj string)
{
    char *string_str = NULL;
    int   str_len    = 0;
    ScmObj head = SCM_NIL;
    ScmObj prev = NULL;
    ScmObj next = NULL;
    int i = 0;
    const char *ch_start_ptr = NULL;
    const char *ch_end_ptr   = NULL;
    char *new_ch = NULL;

    if (!SCM_STRINGP(string))
        SigScm_ErrorObj("string->list : string required but got ", string);

    string_str = SCM_STRING_STR(string);
    str_len    = SCM_STRING_LEN(string);
    if (str_len == 0)
        return SCM_NIL;

    for (i = 0; i < str_len; i++) {
        ch_start_ptr = SigScm_default_encoding_str_startpos(string_str, i);
        ch_end_ptr   = SigScm_default_encoding_str_endpos(string_str, i);

        new_ch = (char*)malloc(sizeof(char) * (ch_end_ptr - ch_start_ptr + 1));
        memset(new_ch, 0, sizeof(char) * (ch_end_ptr - ch_start_ptr + 1));
        strncpy(new_ch, ch_start_ptr, (sizeof(char) * (ch_end_ptr - ch_start_ptr)));

        next = Scm_NewCons(Scm_NewChar(new_ch), SCM_NIL);
        if (prev)
            SCM_SETCDR(prev, next);
        else
            head = next;

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list_to_string(ScmObj list)
{
    int total_size = 0;
    ScmObj chars   = SCM_NIL;
    ScmObj obj     = SCM_NIL;
    char  *new_str = NULL;
    char  *p       = NULL;

    if (EQ(ScmOp_listp(list), SCM_FALSE))
        SigScm_ErrorObj("list->string : list required but got ", list);

    /* count total size of the string */
    for (chars = list; !SCM_NULLP(chars); chars = SCM_CDR(chars)) {
        obj = SCM_CAR(chars);
        if (!SCM_CHARP(obj))
            SigScm_ErrorObj("list->string : char required but got ", obj);

        total_size += strlen(SCM_CHAR_CH(obj));
    }

    /* allocate new string */
    new_str = (char*)malloc(sizeof(char) * total_size + 1);

    /* copy char by char */
    p = new_str;
    for (chars = list; !SCM_NULLP(chars); chars = SCM_CDR(chars)) {
        obj = SCM_CAR(chars);

        strcpy(p, SCM_CHAR_CH(obj));
        p += strlen(SCM_CHAR_CH(obj));
    }

    return Scm_NewString(new_str);
}

ScmObj ScmOp_string_copy(ScmObj string)
{
    if (!SCM_STRINGP(string))
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

    if (!SCM_STRINGP(string))
        SigScm_ErrorObj("string-fill! : string required but got ", string);
    if (!SCM_CHARP(ch))
        SigScm_ErrorObj("string-fill! : character required but got ", ch);

    /* create new str */
    char_size = strlen(SCM_CHAR_CH(ch));
    str_len   = SCM_STRING_LEN(string);
    new_str   = (char*)realloc(SCM_STRING_STR(string), sizeof(char) * str_len * char_size + 1);
    for (i = 0, p = new_str; i < char_size * str_len;) {
        strcpy(p, SCM_CHAR_CH(ch));

        p += char_size;
        i += char_size;
    }

    SCM_SETSTRING_STR(string, new_str);

    return SCM_UNSPECIFIED;
}

/*==============================================================================
  R5RS : 6.3 Other data types : 6.3.6 Vectors
==============================================================================*/
ScmObj ScmOp_vectorp(ScmObj obj)
{
    if (SCM_VECTORP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_make_vector(ScmObj arg, ScmObj env )
{
    ScmObj *vec   = NULL;
    ScmObj  scm_k = SCM_CAR(arg);
    ScmObj  fill  = SCM_NIL;
    int c_k = 0;
    int i   = 0;

    if (!SCM_INTP(scm_k))
        SigScm_ErrorObj("make-vector : integer required but got ", scm_k);

    /* allocate vector */
    c_k = SCM_INT_VALUE(scm_k);
    vec = (ScmObj*)malloc(sizeof(ScmObj) * c_k);

    /* fill vector */
    fill = SCM_UNSPECIFIED;
    if (!SCM_NULLP(SCM_CDR(arg)) && !SCM_NULLP(SCM_CAR(SCM_CDR(arg))))
        fill = SCM_CAR(SCM_CDR(arg));

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
        vec[i] = SCM_CAR(arg);
        arg = SCM_CDR(arg);
    }

    return Scm_NewVector(vec, c_len);
}

ScmObj ScmOp_vector_length(ScmObj vec)
{
    if (!SCM_VECTORP(vec))
        SigScm_ErrorObj("vector-length : vector required but got ", vec);

    return Scm_NewInt(SCM_VECTOR_LEN(vec));
}

ScmObj ScmOp_vector_ref(ScmObj vec, ScmObj scm_k)
{
    if (!SCM_VECTORP(vec))
        SigScm_ErrorObj("vector-ref : vector required but got ", vec);
    if (!SCM_INTP(scm_k))
        SigScm_ErrorObj("vector-ref : number required but got ", scm_k);

    return SCM_VECTOR_REF(vec, scm_k);
}

ScmObj ScmOp_vector_set(ScmObj vec, ScmObj scm_k, ScmObj obj)
{
    if (!SCM_VECTORP(vec))
        SigScm_ErrorObj("vector-set! : vector required but got ", vec);
    if (!SCM_INTP(scm_k))
        SigScm_ErrorObj("vector-set! : number required but got ", scm_k);

    SCM_SETVECTOR_REF(vec, scm_k, obj);

    return SCM_UNSPECIFIED;
}

ScmObj ScmOp_vector_to_list(ScmObj vec)
{
    ScmObj *v    = NULL;
    ScmObj  prev = NULL;
    ScmObj  next = NULL;
    ScmObj  head = NULL;
    int c_len = 0;
    int i = 0;

    if (!SCM_VECTORP(vec))
        SigScm_ErrorObj("vector->list : vector required but got ", vec);

    v = SCM_VECTOR_VEC(vec);
    c_len = SCM_VECTOR_LEN(vec);
    if (c_len == 0)
        return SCM_NIL;

    for (i = 0; i < c_len; i++) {
        next = Scm_NewCons(v[i], SCM_NIL);

        if (prev) {
            SCM_SETCDR(prev, next);
        } else {
            head = next;
        }

        prev = next;
    }

    return head;
}

ScmObj ScmOp_list_to_vector(ScmObj list)
{
    ScmObj  scm_len = SCM_NIL;
    ScmObj *v       = NULL;
    int c_len = 0;
    int i = 0;

    /* TOOD : canbe optimized. scanning list many times */
    if (EQ(ScmOp_listp(list), SCM_FALSE))
        SigScm_ErrorObj("list->vector : list required but got ", list);

    scm_len = ScmOp_length(list);
    c_len   = SCM_INT_VALUE(scm_len);
    v       = (ScmObj*)malloc(sizeof(ScmObj) * c_len);
    for (i = 0; i < c_len; i++) {
        v[i] = SCM_CAR(list);
        list = SCM_CDR(list);
    }

    return Scm_NewVector(v, c_len);
}

ScmObj ScmOp_vector_fill(ScmObj vec, ScmObj fill)
{
    int c_len = 0;
    int i = 0;

    if (!SCM_VECTORP(vec))
        SigScm_ErrorObj("vector->list : vector required but got ", vec);

    c_len = SCM_VECTOR_LEN(vec);
    for (i = 0; i < c_len; i++) {
        SCM_VECTOR_VEC(vec)[i] = fill;
    }

    return SCM_UNSPECIFIED;
}

/*=======================================
  R5RS : 6.4 Control Features
=======================================*/
ScmObj ScmOp_procedurep(ScmObj obj)
{
    if (SCM_FUNCP(obj) || SCM_CLOSUREP(obj))
        return SCM_TRUE;

    return SCM_FALSE;
}

ScmObj ScmOp_map(ScmObj map_arg, ScmObj env)
{
    int arg_len = SCM_INT_VALUE(ScmOp_length(map_arg));
    ScmObj proc = SCM_CAR(map_arg);
    ScmObj args = SCM_NIL;
    ScmObj ret  = SCM_NIL;
    ScmObj tmp  = SCM_NIL;

    ScmObj arg_vector = SCM_NIL;
    ScmObj arg1       = SCM_NIL;
    int vector_len = 0;
    int i = 0;

    /* arglen check */
    if (arg_len < 2)
        SigScm_Error("map : Wrong number of arguments\n");

    /* 1proc and 1arg case */
    if (arg_len == 2) {
        /* apply func to each item */
        for (args = SCM_CAR(SCM_CDR(map_arg)); !SCM_NULLP(args); args = SCM_CDR(args)) {
            /* create proc's arg */
            tmp = SCM_CAR(args);
            if (!SCM_CONSP(tmp)) {
                /* arg must be the list */
                tmp = Scm_NewCons(tmp, SCM_NIL);
            }

            /* create list for "apply" op */
            tmp = Scm_NewCons(proc,
                              Scm_NewCons(tmp,
                                          SCM_NIL));

            /* apply proc */
            ret = Scm_NewCons(ScmOp_apply(tmp, env), ret);
        }
        return ScmOp_reverse(ret);
    }

    /* 1proc and many args case */
    arg_vector = ScmOp_list_to_vector(SCM_CDR(map_arg));
    vector_len = SCM_VECTOR_LEN(arg_vector);
    while (1) {
        /* create arg */
        arg1 = SCM_NIL;
        for (i = 0; i < vector_len; i++) {
            tmp  = SCM_VECTOR_CREF(arg_vector, i);
            /* check if we can continue next loop */
            if (SCM_NULLP(tmp)) {
                /* if next item is SCM_NIL, let's return! */
                return ScmOp_reverse(ret);
            }

            arg1 = Scm_NewCons(SCM_CAR(tmp), arg1);
            SCM_SETVECTOR_CREF(arg_vector, i, SCM_CDR(tmp));
        }

        /* reverse arg */
        arg1 = ScmOp_reverse(arg1);

        /* apply proc to arg1 */
        ret = Scm_NewCons(ScmOp_apply(Scm_NewCons(proc,
                                                  Scm_NewCons(arg1,
                                                              SCM_NIL)),
                                      env),
                          ret);
    }

    /* never reaches here */
    SigScm_Error("map bug?\n");
    return SCM_NIL;
}

ScmObj ScmOp_for_each(ScmObj arg, ScmObj env)
{
    ScmOp_map(arg, env);

    return SCM_UNSPECIFIED;
}

ScmObj ScmOp_force(ScmObj arg, ScmObj env)
{
    if (SCM_INT_VALUE(ScmOp_length(arg)) != 1)
        SigScm_Error("force : Wrong number of arguments\n");
    if (!SCM_CLOSUREP(SCM_CAR(arg)))
        SigScm_Error("force : not proper delayed object\n");

    /* evaluated exp = ( SCM_CAR(arg) ) */
    return ScmOp_eval(Scm_NewCons(SCM_CAR(arg), SCM_NIL), env);
}

ScmObj ScmOp_call_with_current_continuation(ScmObj arg, ScmObj env)
{
    int jmpret  = 0;
    ScmObj proc = SCM_CAR(arg);
    ScmObj cont = SCM_NIL;

    if (!SCM_CLOSUREP(proc))
	SigScm_ErrorObj("call-with-current-continuation : closure required but got ", proc);
    
    cont = Scm_NewContinuation();
 
    /* setjmp and check result */
    jmpret = setjmp(SCM_CONTINUATION_JMPENV(cont));
    if (jmpret) {
	/* return by calling longjmp */
	return continuation_thrown_obj;
    }

    /* execute (proc cont) */
    SCM_SETCDR(arg, Scm_NewCons(cont, SCM_NIL));

    return ScmOp_eval(arg, env);
}

#if USE_SRFI1
#include "operations-srfi1.c"
#endif
