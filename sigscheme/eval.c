/*===========================================================================
 *  FileName : eval.c
 *  About    : Evaluation and basic Syntactic Expression
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

/*
 * Descrioption of Environment
 *
 * [1] Data Structure of Environment
 *     Environment is the simple list that is formed as below.
 *
 *     - Frame = ( (var1 var2 var3 ...)
 *                 (val1 val2 val3 ...) )
 *     - Env   = ( Frame1 Frame2 Frame3 ...)
 *
 */

/*=======================================
  System Include
=======================================*/

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
ScmObj continuation_thrown_obj = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env);
static ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env);
static ScmObj lookup_environment(ScmObj var, ScmObj env);
static ScmObj lookup_frame(ScmObj var, ScmObj frame);

static ScmObj symbol_value(ScmObj var, ScmObj env);

static ScmObj map_eval(ScmObj args, ScmObj env);
static ScmObj eval_unquote(ScmObj args, ScmObj env);
static ScmObj ScmOp_last_pair(ScmObj list);

/*=======================================
  Function Implementations
=======================================*/

static ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(vars) && SCM_NULLP(vals))
	return env;

    /* create new frame */
    frame   = Scm_NewCons(vars, vals);

    /* add to env */
    if (SCM_NULLP(env))
        env = Scm_NewCons(frame, SCM_NIL);
    else if (SCM_CONSP(env))
        env = Scm_NewCons(frame, env);
    else
        SigScm_Error("Broken environment.\n");

    return env;
}


static ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame, tmp;
    ScmObj new_varlist, new_vallist;

    /* sanity check */
    if (SCM_NULLP(var) && SCM_NULLP(val))
	return env;

    /* add (var val) pair to the newest frame in env */
    if (SCM_NULLP(env)) {
	env = Scm_NewCons(Scm_NewCons(var, val),
			  SCM_NIL);
    } else if (SCM_CONSP(env)) {
	newest_frame = SCM_CAR(env);
	new_varlist  = Scm_NewCons(var, SCM_CAR(newest_frame));
	new_vallist  = Scm_NewCons(val, SCM_CDR(newest_frame));

	tmp = Scm_NewCons(Scm_NewCons(new_varlist, new_vallist), SCM_CDR(env));
	*env = *tmp;
    } else {
	SigScm_Error("broken environment\n");
    }
    return env;
}

/*========================================================
  ScmObj lookup_environment(ScmObj var, ScmObj env)

  @return list which represent (val vals-in-frame).
          val is the value of var.

  TODO : describe more precicely
========================================================*/
static ScmObj lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame = SCM_NIL;
    ScmObj val   = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(env))
        return SCM_NIL;
    if (!SCM_CONSP(env))
        SigScm_Error("Broken environent.\n");

    /* lookup frames */
    for (; !SCM_NULLP(env); env = SCM_CDR(env)) {
        frame = SCM_CAR(env);
        val   = lookup_frame(var, frame);
        if (!SCM_NULLP(val))
            return val;
    }

    return SCM_NIL;
}

static ScmObj lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vals = SCM_NIL;
    ScmObj vars = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(frame))
        return SCM_NIL;
    else if (!SCM_CONSP(frame))
        SigScm_Error("Broken frame.\n");

    /* lookup in frame */
    vars = SCM_CAR(frame);
    vals = SCM_CDR(frame);

    for (; !SCM_NULLP(vars) && !SCM_NULLP(vals); vars = SCM_CDR(vars), vals = SCM_CDR(vals)) {
	/* handle dot list */
	if (SCM_CONSP(vars)) {
	    if (SCM_EQ(SCM_CAR(vars), var))
		return vals;
	} else {
	    if (SCM_EQ(vars, var))
		return Scm_NewCons(vals, SCM_NIL);
	}
    }

    return SCM_NIL;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj ScmOp_eval(ScmObj obj, ScmObj env)
{
    ScmObj tmp  = SCM_NIL;
    ScmObj arg  = SCM_NIL;

eval_loop:
    switch (SCM_GETTYPE(obj)) {
        case ScmSymbol:
            return symbol_value(obj, env);

	/*====================================================================
	  Evaluating Expression
	====================================================================*/
        case ScmCons:
            {
		/*============================================================
		  Evaluating CAR
		============================================================*/
                tmp = SCM_CAR(obj);
                switch (SCM_GETTYPE(tmp)) {
		    case ScmFunc:
			break;
		    case ScmClosure:
			break;
                    case ScmSymbol:
                        tmp = symbol_value(tmp, env);
                        break;
                    case ScmCons:
                        tmp = ScmOp_eval(tmp, env);
                        break;
		    case ScmEtc:
			/* QUOTE case */
			break;
		    default:
			SigScm_ErrorObj("eval : invalid operation ", tmp);
			break;
                }
		/*============================================================
		  Evaluating the rest of the List by the type of CAR
		============================================================*/
                switch (SCM_GETTYPE(tmp)) {
                    case ScmFunc:
			/*
			 * Description of ARGNUM handling.
			 *
			 * - ARGNUM_L
			 *     - evaluate all the args and pass it to func
			 *
			 * - ARGNUM_R_NotEval
			 *     - not evaluate all the arguments
			 *     - not evaluate the result of function
			 *
			 * - ARGNUM_R_Eval (for tail-recursion)
			 *     - not evaluate all the arguments
			 *     - evaluate the result of function
			 *
			 * - ARGNUM_2N
			 *     - call the function with each 2 objs
			 *
			 * - ARGNUM_0
			 * - ARGNUM_1
			 * - ARGNUM_2
			 * - ARGNUM_3
			 * - ARGNUM_4
			 * - ARGNUM_5
			 *     - call the function with 0-5 arguments
			 */
                        switch (SCM_FUNC_NUMARG(tmp)) {
                            case ARGNUM_L:
                                {
                                    return SCM_FUNC_EXEC_SUBRL(tmp,
                                                               map_eval(SCM_CDR(obj), env),
							       env);
                                }
			    case ARGNUM_R_NotEval:
				{
                                    return SCM_FUNC_EXEC_SUBRR(tmp,
                                                               SCM_CDR(obj),
							       &env);
				}
			    case ARGNUM_R_Eval:
				{
				    obj = SCM_FUNC_EXEC_SUBRR(tmp,
							      SCM_CDR(obj),
							      &env);

				    /*
				     * The core point of tail-recursion
				     *
				     * The return obj of ARGNUM_R_Eval func is the raw S-expression.
				     * So we need to evaluate it! This is for not to consume stack,
				     * that is, tail-recursion optimization.
				     */
				    goto eval_loop;
				}
			    case ARGNUM_2N:
				{
				    obj = SCM_CDR(obj);
				    arg = ScmOp_eval(SCM_CAR(obj), env);
				    for (obj = SCM_CDR(obj); !SCM_NULLP(obj); obj = SCM_CDR(obj)) {
					arg = SCM_FUNC_EXEC_SUBR2N(tmp,
								   arg,
								   ScmOp_eval(SCM_CAR(obj), env));
				    }
				    return arg;
				}
                            case ARGNUM_0:
                                return SCM_FUNC_EXEC_SUBR0(tmp);
                            case ARGNUM_1:
                                return SCM_FUNC_EXEC_SUBR1(tmp, ScmOp_eval(SCM_CAR(SCM_CDR(obj)),env));
                            case ARGNUM_2:
                                {
                                    obj = SCM_CDR(obj);
                                    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
                                    return SCM_FUNC_EXEC_SUBR2(tmp,
                                                               arg,
                                                               ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env)); /* 2nd arg */
                                }
			    case ARGNUM_3:
				{
				    obj = SCM_CDR(obj);
				    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
				    obj = SCM_CDR(obj);
				    return SCM_FUNC_EXEC_SUBR3(tmp,
							       arg,
							       ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env)); /* 3rd arg */
				}
			    case ARGNUM_4:
				{
				    obj = SCM_CDR(obj);
				    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
				    obj = SCM_CDR(obj);
				    return SCM_FUNC_EXEC_SUBR4(tmp,
							       arg,
							       ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env), /* 3rd arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(obj))), env)); /* 4th arg */
				}
			    case ARGNUM_5:
				{
				    obj = SCM_CDR(obj);
				    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
				    obj = SCM_CDR(obj);
				    return SCM_FUNC_EXEC_SUBR5(tmp,
							       arg,
							       ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env), /* 3rd arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(obj))), env), /* 4th arg */
							       ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(obj)))), env)); /* 5th arg */

				}
                        }
                        break;
		    case ScmClosure:
			{
			    /*	
			     * Description of the ScmClosure handling
			     *
			     * (lambda <formals> <body>)
			     *
			     * <formals> should have 3 forms.
			     *
			     *   (1) : <variable>
			     *   (2) : (<variable1> <variable2> ...)
			     *   (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
			     */
			    arg = SCM_CAR(SCM_CLOSURE_EXP(tmp)); /* arg is <formals> */

			    if (SCM_SYMBOLP(arg)) {
				/* (1) : <variable> */
				env = extend_environment(Scm_NewCons(arg, SCM_NIL),
							 Scm_NewCons(map_eval(SCM_CDR(obj), env),
								     SCM_NIL),
							 SCM_CLOSURE_ENV(tmp));
			    } else if (SCM_NULLP(arg) || SCM_CONSP(arg)) {
				/*
				 * (2) : (<variable1> <variable2> ...)
				 * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
				 *
				 *  - dot list is handled in lookup_frame().
				 */
				env = extend_environment(arg,
							 map_eval(SCM_CDR(obj), env),
							 SCM_CLOSURE_ENV(tmp));
			    } else {
				SigScm_ErrorObj("lambda : bad syntax with ", arg);
			    }

			    /*
			     * Notice
			     *
			     * The return obj of ScmExp_begin is the raw S-expression.
			     * Because it is defined as ARGNUM_R_Eval function. So we
			     * need to evaluate this!.
			     */
			    obj = ScmExp_begin(SCM_CDR(SCM_CLOSURE_EXP(tmp)), &env);
			    goto eval_loop;
			}
		    case ScmContinuation:
			{
                           /*
			    * Description of ScmContinuation handling
			    *
                            * (1) eval 1st arg
                            * (2) store it to global variable "continuation_thrown_obj"
                            * (3) then longjmp
			    *
			    * PROBLEM : setjmp/longjmp is stack based operation, so we
			    * cannot jump from the bottom of the stack to the top of
			    * the stack. Is there any efficient way to implement first
			    * class continuation? (TODO).
			    */
			    obj = SCM_CAR(SCM_CDR(obj));
			    continuation_thrown_obj = ScmOp_eval(obj, env);
			    longjmp(SCM_CONTINUATION_JMPENV(tmp), 1);
			}
			break;
		    case ScmEtc:
			if (EQ(tmp, SCM_QUOTE)) {
			    return SCM_CDR(obj);
			}
			if (EQ(tmp, SCM_QUASIQUOTE)) {
			    return eval_unquote(SCM_CDR(obj), env);
			}
			return tmp;
                    default:
                        /* What? */
                        SigScm_Error("eval : What type of function? ", tmp);
                }

            }
        default:
            return obj;
    }

    return SCM_NIL;
}

ScmObj ScmOp_apply(ScmObj args, ScmObj env)
{
    ScmObj proc  = SCM_NIL;
    ScmObj obj   = SCM_NIL;

    /* sanity check */
    if CHECK_2_ARGS(args)
	SigScm_Error("apply : Wrong number of arguments\n");

    /* 1st elem of list is proc */
    proc = SCM_CAR(args);

    /* 2nd elem of list is obj */
    obj  = SCM_CAR(SCM_CDR(args));

    /* apply proc */
    switch (SCM_GETTYPE(proc)) {
	case ScmFunc:
	    switch (SCM_FUNC_NUMARG(proc)) {
		case ARGNUM_L:
		    {
			return SCM_FUNC_EXEC_SUBRL(proc,
						   map_eval(obj, env),
						   env);
		    }
		case ARGNUM_2N:
		    {
			args = obj;
			obj  = SCM_CAR(args);
			for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
			    obj = SCM_FUNC_EXEC_SUBR2N(proc,
						       obj,
						       ScmOp_eval(SCM_CAR(args), env));
			}
			return obj;
		    }
		case ARGNUM_0:
		    {
			return SCM_FUNC_EXEC_SUBR0(proc);
		    }
		case ARGNUM_1:
		    {
			return SCM_FUNC_EXEC_SUBR1(proc,
						   obj);
		    }
		case ARGNUM_2:
		    {
			return SCM_FUNC_EXEC_SUBR2(proc,
						   obj,
						   SCM_CAR(SCM_CDR(SCM_CDR(args))));
		    }
		case ARGNUM_3:
		    {
			return SCM_FUNC_EXEC_SUBR3(proc,
						   obj,
						   SCM_CAR(SCM_CDR(SCM_CDR(args))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(args)))));
		    }
		case ARGNUM_4:
		    {
			return SCM_FUNC_EXEC_SUBR4(proc,
						   obj,
						   SCM_CAR(SCM_CDR(SCM_CDR(args))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(args)))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(args))))));
		    }
		case ARGNUM_5:
		    {
			return SCM_FUNC_EXEC_SUBR5(proc,
						   obj,
						   SCM_CAR(SCM_CDR(SCM_CDR(args))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(args)))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(args))))),
						   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(args)))))));
		    }
		default:
		    SigScm_ErrorObj("apply : invalid applycation ", args);
	    }
	    break;
	case ScmClosure:
	    {
		/*
		 * Description of the ScmClosure handling
		 *
		 * (lambda <formals> <body>)
		 *
		 * <formals> should have 3 forms.
		 *
		 *   (1) : <variable>
		 *   (2) : (<variable1> <variable2> ...)
		 *   (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
		 */
		obj = SCM_CAR(SCM_CLOSURE_EXP(proc)); /* arg is <formals> */

		if (SCM_SYMBOLP(obj)) {
		    /* (1) : <variable> */
		    env = extend_environment(Scm_NewCons(obj, SCM_NIL),
					     Scm_NewCons(SCM_CDR(args),
							 SCM_NIL),
					     SCM_CLOSURE_ENV(proc));
		} else if (SCM_NULLP(obj) || SCM_CONSP(obj)) {
		    /*
		     * (2) : (<variable1> <variable2> ...)
		     * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
		     *
		     *  - dot list is handled in lookup_frame().
		     */
		    env = extend_environment(obj,
					     SCM_CAR(SCM_CDR(args)),
					     SCM_CLOSURE_ENV(proc));
		} else {
		    SigScm_ErrorObj("lambda : bad syntax with ", obj);
		}
		

		/*
		 * Notice
		 *
		 * The return obj of ScmExp_begin is the raw S-expression.
		 * Because it is defined as ARGNUM_R_Eval function. So we
		 * need to evaluate this!.
		 */
		obj = ScmExp_begin(SCM_CDR(SCM_CLOSURE_EXP(proc)), &env);
		return ScmOp_eval(obj, env);
	    }
	default:
	    SigScm_ErrorObj("apply : invalid applycation ", args);
    }

    /* never reaches here */
    return SCM_NIL;
}


static ScmObj symbol_value(ScmObj var, ScmObj env)
{
    ScmObj val = SCM_NIL;

    /* sanity check */
    if (!SCM_SYMBOLP(var))
	SigScm_ErrorObj("symbol_value : not symbol : ", var);

    /* First, lookup the Environment */
    val = lookup_environment(var, env);
    if (!SCM_NULLP(val)) {
        /* Variable is found in Environment, so returns its value */
        return SCM_CAR(val);
    }

    /* Next, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND)) {
        SigScm_ErrorObj("symbol_value : unbound variable ", var);
    }

    return val;
}

ScmObj map_eval(ScmObj args, ScmObj env)
{
    ScmObj result  = SCM_NIL;
    ScmObj tail    = SCM_NIL;
    ScmObj newtail = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(args))
        return SCM_NIL;

    /* eval each element of args */
    result  = Scm_NewCons(ScmOp_eval(SCM_CAR(args), env), SCM_NIL);
    tail    = result;
    newtail = SCM_NIL;
    for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
	newtail = Scm_NewCons(ScmOp_eval(SCM_CAR(args), env), SCM_NIL);
	SCM_SETCDR(tail, newtail);
	tail = newtail;
    }

    return result;
}

/*
 * TODO : implement this properly as defined in R5RS!!
 *
 * Quasiquote forms may be nested. Substitutions are made only
 * for unquoted components appearing at the same nesting level
 * as the outermost backquote. The nesting level increases by
 * one inside each successive quasiquotation, and decreases by
 * one inside each unquotation.
 */
static ScmObj eval_unquote(ScmObj args, ScmObj env)
{
    ScmObj list = args;
    ScmObj prev = list;
    ScmObj obj  = SCM_NIL;

    /* scanning list */
    for (; !SCM_NULLP(list); list = SCM_CDR(list))
    {
	obj = SCM_CAR(list);

	/* handle quotes */
	if (SCM_CONSP(obj)) {
	    /* handle nested SCM_QUASIQUOTE(`) */
	    if (EQ(SCM_CDR(obj), SCM_QUASIQUOTE)) {
		continue; /* left untouched */
	    }

	    /* handle SCM_UNQUOTE(,) */
	    if (EQ(SCM_CAR(obj), SCM_UNQUOTE)) {
		SCM_SETCAR(list, ScmOp_eval(SCM_CDR(obj), env));
	    }

	    /* handle SCM_UNQUOTE_SPLICING(,@) */
	    if (EQ(SCM_CAR(obj), SCM_UNQUOTE_SPLICING)) {
		obj = ScmOp_eval(SCM_CDR(obj), env);
		if (!SCM_CONSP(obj))
		    SigScm_Error("invalid unquote-splicing (,@)\n");

		SCM_SETCDR(ScmOp_last_pair(obj), SCM_CDR(SCM_CDR(prev)));
		SCM_SETCDR(prev, obj);
	    }
	}

	prev = list;
    }

    return args;
}

static ScmObj ScmOp_last_pair(ScmObj list)
{
    /* sanity check */
    if (SCM_NULLP(list))
	return SCM_NIL;
    if (!SCM_CONSP(list))
	SigScm_ErrorObj("last_pair : list required but got ", list);

    while (1) {
        if (!SCM_CONSP(list) || SCM_NULLP(SCM_CDR(list)))
            return list;

        list = SCM_CDR(list);
    }

    return SCM_NIL;
}

/*=======================================
  R5RS : 4.1 Primitive expression types
=======================================*/
/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.2 Literal expressions
===========================================================================*/
ScmObj ScmOp_quote(ScmObj obj)
{
    ScmObj quotedObj = Scm_NewCons(SCM_QUOTE, obj);

    return quotedObj;
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj ScmExp_lambda(ScmObj exp, ScmObj *envp)
{
    ScmObj env = *envp;

    if CHECK_2_ARGS(exp)
	SigScm_Error("lambda : too few argument\n");

    return Scm_NewClosure(exp, env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj ScmExp_if(ScmObj exp, ScmObj *envp)
{
    ScmObj env       = *envp;
    ScmObj pred      = SCM_NIL;
    ScmObj false_exp = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(exp) || SCM_NULLP(SCM_CDR(exp)))
	SigScm_Error("if : syntax error\n");

    /* eval predicates */
    pred = ScmOp_eval(SCM_CAR(exp), env);

    /* if pred is SCM_TRUE */
    if (EQ(pred, SCM_TRUE)) {
	/* doesn't evaluate now for tail-recursion. */
	return SCM_CAR(SCM_CDR(exp));
    }

    /* if pred is SCM_FALSE */
    false_exp = SCM_CDR(SCM_CDR(exp));
    if (SCM_NULLP(false_exp))
	return SCM_UNDEF;

    /* doesn't evaluate now for tail-recursion. */
    return SCM_CAR(false_exp);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignment
===========================================================================*/
ScmObj ScmExp_set(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj sym = SCM_CAR(arg);
    ScmObj val = SCM_CAR(SCM_CDR(arg));
    ScmObj ret = SCM_NIL;
    ScmObj tmp = SCM_NIL;

    if (SCM_NULLP(val))
	SigScm_Error("set! : syntax error\n");

    ret = ScmOp_eval(val, env);
    tmp = lookup_environment(sym, env);
    if (SCM_NULLP(tmp)) {
	/*
	 * not found in the environment
	 * if symbol is not bounded, error occurs
	 */
	if (EQ(ScmOp_boundp(sym), SCM_FALSE))
	    SigScm_ErrorObj("set! : unbound variable ", sym);

	SCM_SETSYMBOL_VCELL(sym, ret);
    } else {
	/* found in the environment*/
	SCM_SETCAR(tmp, ret);
    }

    return ret;
}


/*=======================================
  R5RS : 4.2 Derived expression types
=======================================*/
/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.1 Conditionals
===========================================================================*/
ScmObj ScmExp_cond(ScmObj arg, ScmObj *envp)
{
    ScmObj env    = *envp;
    ScmObj clause = SCM_NIL;
    ScmObj test   = SCM_NIL;
    ScmObj exps   = SCM_NIL;
    /* looping in each clause */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
	clause = SCM_CAR(arg);
	test   = SCM_CAR(clause);
	exps   = SCM_CDR(clause);
	if (SCM_NULLP(clause) || SCM_NULLP(test) || SCM_NULLP(exps))
	    SigScm_Error("cond : syntax error\n");

	/* evaluate test and check the result */
	if (SCM_EQ(ScmOp_eval(test, env), SCM_TRUE)) {
	    return ScmExp_begin(exps, &env);
	}
    }

    SigScm_Error("cond : invalid expression\n");
    return SCM_NIL;
}

ScmObj ScmExp_case(ScmObj arg, ScmObj *envp)
{
    ScmObj env    = *envp;
    ScmObj key    = ScmOp_eval(SCM_CAR(arg), env);
    ScmObj clause = SCM_NIL;
    ScmObj datums = SCM_NIL;
    ScmObj exps   = SCM_NIL;

    /* looping in each clause */
    for (arg = SCM_CDR(arg); !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
	clause = SCM_CAR(arg);
	datums = SCM_CAR(clause);
	exps   = SCM_CDR(clause);
	if (SCM_NULLP(clause) || SCM_NULLP(datums) || SCM_NULLP(exps))
	    SigScm_Error("cond : syntax error\n");

	/* check "else" symbol */
	if (SCM_NULLP(SCM_CDR(arg)) && !SCM_CONSP(datums) && EQ(SCM_SYMBOL_VCELL(datums), SCM_TRUE))
	    return ScmExp_begin(exps, &env);

	/* evaluate datums and compare to key by eqv? */
	for (; !SCM_NULLP(datums); datums = SCM_CDR(datums)) {
	    if (EQ(ScmOp_eqvp(ScmOp_eval(SCM_CAR(datums), env), key), SCM_TRUE)) {
		return ScmExp_begin(exps, &env);;
	    }
	}
    }

    return SCM_UNSPECIFIED;
}

ScmObj ScmExp_and(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NIL;
    ScmObj ret = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg))
	return SCM_TRUE;
    if (EQ(ScmOp_listp(arg), SCM_FALSE))
	SigScm_ErrorObj("and : list required but got ", arg);

    /* check recursively */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
	obj = SCM_CAR(arg);
	ret = ScmOp_eval(obj, env);
	if (EQ(ret, SCM_FALSE))
	    return SCM_FALSE;

	/* return last item */
	if (SCM_NULLP(SCM_CDR(arg))) {
	    return ret;
	}
    }

    return SCM_NIL;
}

ScmObj ScmExp_or(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NIL;
    ScmObj ret = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg))
	return SCM_FALSE;
    if (EQ(ScmOp_listp(arg), SCM_FALSE))
	SigScm_ErrorObj("or : list required but got ", arg);

    /* check recursively */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
	obj = SCM_CAR(arg);
	ret = ScmOp_eval(obj, env);
	if (EQ(ret, SCM_TRUE))
	    return SCM_TRUE;

	/* return last item */
	if (SCM_NULLP(SCM_CDR(arg))) {
	    return ret;
	}
    }

    return SCM_NIL;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
ScmObj ScmExp_let(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NIL;
    ScmObj body     = SCM_NIL;
    ScmObj vars     = SCM_NIL;
    ScmObj vals     = SCM_NIL;
    ScmObj binding  = SCM_NIL;

    /* sanity check */
    if CHECK_2_ARGS(arg)
	SigScm_Error("let : syntax error\n");

    /* get bindings and body */
    bindings = SCM_CAR(arg);
    body     = SCM_CDR(arg);

    /*========================================================================
      (let <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (SCM_CONSP(bindings)) {
	for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
	    binding = SCM_CAR(bindings);
	    vars = Scm_NewCons(SCM_CAR(binding), vars);
	    vals = Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), vals);
	}

	/* create new environment for */
	env = extend_environment(vars, vals, env);
	*envp = env;

	return ScmExp_begin(body, &env);
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_let_star(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NIL;
    ScmObj body     = SCM_NIL;
    ScmObj vars     = SCM_NIL;
    ScmObj vals     = SCM_NIL;
    ScmObj binding  = SCM_NIL;

    /* sanity check */
    if CHECK_2_ARGS(arg)
	SigScm_Error("let* : syntax error\n");

    /* get bindings and body */
    bindings = SCM_CAR(arg);
    body     = SCM_CDR(arg);

    /*========================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (SCM_CONSP(bindings)) {
	for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
	    binding = SCM_CAR(bindings);
	    vars = Scm_NewCons(SCM_CAR(binding), SCM_NIL);
	    vals = Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), SCM_NIL);

	    /* add env to each time!*/
	    env = extend_environment(vars, vals, env);
	}

	/* set new env */
	*envp = env;
	
	return ScmExp_begin(body, &env);;
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_letrec(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NIL;
    ScmObj body     = SCM_NIL;
    ScmObj vars     = SCM_NIL;
    ScmObj vals     = SCM_NIL;
    ScmObj binding  = SCM_NIL;

    /* sanity check */
    if CHECK_2_ARGS(arg)
	SigScm_Error("letrec : syntax error\n");

    /* get bindings and body */
    bindings = SCM_CAR(arg);
    body     = SCM_CDR(arg);

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (SCM_CONSP(bindings)) {
	for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
	    binding = SCM_CAR(bindings);

	    /* first, temporally add symbol to the env*/
	    vars = Scm_NewCons(SCM_CAR(binding), SCM_NIL);
	    vals = Scm_NewCons(SCM_NIL, SCM_NIL);
	    env  = extend_environment(vars, vals, env);

	    /* then, evaluate <init> val and (set! var val) */
	    ScmExp_set(Scm_NewCons(SCM_CAR(binding),
				   Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), SCM_NIL)),
		       &env);
	}

	/* set new env */
	*envp = env;

	return ScmExp_begin(body, &env);
    }

    return SCM_UNDEF;
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj ScmExp_begin(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj exp = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg))
	return SCM_UNDEF;
    if (EQ(ScmOp_listp(arg), SCM_FALSE))
	SigScm_ErrorObj("begin : list required but got ", arg);

    /* eval recursively */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
	exp = SCM_CAR(arg);

	/* return last expression's result */
	if (EQ(SCM_CDR(arg), SCM_NIL)) {
	    *envp = env;

	    /* doesn't evaluate exp now for tail-recursion. */
	    return exp; 
	}

	ScmOp_eval(exp, env);
    }

    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
ScmObj ScmExp_do(ScmObj arg, ScmObj *envp)
{
    /*
     * (do ((<variable1> <init1> <step1>)
     *      (<variable2> <init2> <step2>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     */
    ScmObj env        = *envp;
    ScmObj bindings   = SCM_CAR(arg);
    ScmObj vars       = SCM_NIL;
    ScmObj vals       = SCM_NIL;
    ScmObj steps      = SCM_NIL;
    ScmObj binding    = SCM_NIL;
    ScmObj step       = SCM_NIL;
    ScmObj testframe  = SCM_NIL;
    ScmObj test       = SCM_NIL;
    ScmObj expression = SCM_NIL;
    ScmObj commands   = SCM_NIL;
    ScmObj tmp_steps  = SCM_NIL;

    /* sanity check */
    if (SCM_INT_VALUE(ScmOp_length(arg)) < 2)
	SigScm_Error("do : syntax error\n");

    /* construct Environment and steps */
    for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
	binding = SCM_CAR(bindings);
	vars = Scm_NewCons(SCM_CAR(binding), vars);
	vals = Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), vals);

	step = SCM_CDR(SCM_CDR(binding));
	if (!SCM_NULLP(step)) {
	    step = SCM_CAR(step);

	    /* append (<var> <step>) to steps */
	    steps = Scm_NewCons(Scm_NewCons(SCM_CAR(binding),
					    Scm_NewCons(step, SCM_NIL)),
				steps);
	}
    }

    /* now extend environment */
    env = extend_environment(vars, vals, env);

    /* construct test */
    testframe  = SCM_CAR(SCM_CDR(arg));
    test       = SCM_CAR(testframe);
    expression = SCM_CDR(testframe);

    /* construct commands */
    commands = SCM_CDR(SCM_CDR(arg));

    /* now excution phase! */
    while (!SCM_EQ(ScmOp_eval(test, env), SCM_TRUE)) {
	ScmOp_eval(ScmExp_begin(commands, &env), env);

	tmp_steps = steps;
	for (; !SCM_NULLP(tmp_steps); tmp_steps = SCM_CDR(tmp_steps)) {
	    ScmExp_set(SCM_CAR(tmp_steps), &env);
	}
    }

    /* set new env */
    *envp = env;

    return ScmExp_begin(expression, &env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
ScmObj ScmOp_delay(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;

    if (SCM_INT_VALUE(ScmOp_length(arg)) != 1)
        SigScm_Error("delay : Wrong number of arguments\n");

    /* closure exp = ( () SCM_CAR(arg) ) */
    return Scm_NewClosure(Scm_NewCons(SCM_NIL, Scm_NewCons(SCM_CAR(arg), SCM_NIL)), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/
ScmObj ScmOp_quasiquote(ScmObj temp)
{
    return SCM_FALSE;
}

ScmObj ScmOp_unquote(ScmObj exp)
{
    return SCM_FALSE;
}

ScmObj ScmOp_unquote_splicint(ScmObj exp)
{
    return SCM_FALSE;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
ScmObj ScmExp_define(ScmObj arg, ScmObj *envp)
{
    ScmObj env     = *envp;
    ScmObj var     = SCM_CAR(arg);
    ScmObj body    = SCM_CAR(SCM_CDR(arg));
    ScmObj val     = SCM_NIL;
    ScmObj formals = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(var))
	SigScm_Error("define : syntax error\n");

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SCM_SYMBOLP(var)) {
	if (SCM_NULLP(env)) {
	    /* given NIL environment */
	    SCM_SETSYMBOL_VCELL(var, ScmOp_eval(body, env));
	} else {
	    /* lookup environment */
	    val = lookup_environment(var, env);

	    if (!SCM_NULLP(val)) {
		/* found in the environment. set the new variable in env. */
		SCM_SETCAR(val, ScmOp_eval(body, env));
	    } else {
		/* add to environment (not create new frame) */
		add_environment(var, ScmOp_eval(body, env), env);
	    }
	}

	/* set new env */
	*envp = env;

	return var;
    }

    /*========================================================================
      (define (<val> <formals>) <body>)

      => (define <val>
             (lambda (<formals>) <body>))
    ========================================================================*/
    /*========================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda <formals> <body>))
    ========================================================================*/
    if (SCM_CONSP(var)) {
	val     = SCM_CAR(var);
	formals = SCM_CDR(var);
	body    = SCM_CDR(arg);

	/* (val (lambda formals body))  */
	arg = Scm_NewCons(val, Scm_NewCons(ScmExp_lambda(Scm_NewCons(formals, body), &env),
					   SCM_NIL));

	return ScmExp_define(arg, &env);
    }

    SigScm_Error("define : syntax error\n");
    return SCM_NIL;
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
ScmObj ScmOp_scheme_report_environment(ScmObj version)
{
    return SCM_NIL;
}

ScmObj ScmOp_null_environment(ScmObj version)
{
    return SCM_NIL;
}
