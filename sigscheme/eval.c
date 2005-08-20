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
#define SCM_INVALID NULL	/* TODO: make a more appropriate choice */

#define IS_LIST_LEN_1(args)  (SCM_CONSP(args) && SCM_NULLP(SCM_CDR(args)))
/* for the quasiquote family */
#define QQUOTE_SET_VERBATIM(x) ((x) = SCM_INVALID)
#define QQUOTE_IS_VERBATIM(x)  (EQ((x), SCM_INVALID))

/*=======================================
  Variable Declarations
=======================================*/
ScmObj continuation_thrown_obj = NULL; /* for storing continuation return object */
ScmObj letrec_env = NULL;              /* for storing environment obj of letrec */

struct trace_frame *trace_root = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env);
static ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env);
static ScmObj lookup_environment(ScmObj var, ScmObj env);
static ScmObj lookup_frame(ScmObj var, ScmObj frame);

static ScmObj symbol_value(ScmObj var, ScmObj env);

static ScmObj map_eval(ScmObj args, ScmObj env);
static ScmObj qquote_internal(ScmObj expr, ScmObj env, int nest);
static ScmObj qquote_vector(ScmObj vec, ScmObj env, int nest);

/*=======================================
  Function Implementations
=======================================*/

static ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame    = SCM_NIL;
    ScmObj tmp_vars = vars;
    ScmObj tmp_vals = vals;

    /* handle dot list */
    while (1) {
	if (SCM_NULLP(tmp_vars) || !SCM_CONSP(tmp_vars))
	    break;

	/* dot list appears */
	if (!SCM_NULLP(SCM_CDR(tmp_vars)) && !SCM_CONSP(SCM_CDR(tmp_vars))) {
	    /* create new value */
	    SCM_SETCDR(tmp_vals, Scm_NewCons(SCM_CDR(tmp_vals),
					     SCM_NIL));
	}

	tmp_vars = SCM_CDR(tmp_vars);
	tmp_vals = SCM_CDR(tmp_vals);
    }
    
    /* create new frame */
    frame = Scm_NewCons(vars, vals);

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
    if (SCM_NULLP(var))
        return env;

    /* add (var val) pair to the newest frame in env */
    if (SCM_NULLP(env)) {
        newest_frame = Scm_NewCons(Scm_NewCons(var, SCM_NIL),
                                   Scm_NewCons(val, SCM_NIL));
        env = Scm_NewCons(newest_frame,
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
        SigScm_ErrorObj("Broken environent : ", env);

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
        SigScm_ErrorObj("Broken frame : ", frame);

    /* lookup in frame */
    vars = SCM_CAR(frame);
    vals = SCM_CDR(frame);

    while (1) {
	if (SCM_NULLP(vars))
	    break;

	if (!SCM_CONSP(vars)) {
	    /* handle dot list */
	    if (SCM_EQ(vars, var))
		return vals;

	    break;
	} else {
	    /* normal binding */
	    if (SCM_EQ(SCM_CAR(vars), var))
		return vals;
	}

	vars = SCM_CDR(vars);
	vals = SCM_CDR(vals);
    }

    return SCM_NIL;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj ScmOp_eval(ScmObj obj, ScmObj env)
{
    ScmObj tmp = SCM_NIL;
    ScmObj arg = SCM_NIL;
    ScmObj ret = SCM_NIL;
    int tail_flag = 0;

    /* for debugging */
    struct trace_frame frame;
    frame.prev = trace_root;
    frame.obj  = obj;
    trace_root = &frame;

eval_loop:
    switch (SCM_GETTYPE(obj)) {
        case ScmSymbol:
	    {
		ret = symbol_value(obj, env);
		goto eval_done;
	    }

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
                        SigScm_ErrorObj("eval : invalid operation ", obj);
                        break;
                }
                /*============================================================
                  Evaluating the rest of the List by the type of CAR
                ============================================================*/
                switch (SCM_GETTYPE(tmp)) {
                    case ScmFunc:
                        /*
                         * Description of FUNCTYPE handling.
                         *
                         * - FUNCTYPE_L
                         *     - evaluate all the args and pass it to func
                         *
                         * - FUNCTYPE_R
                         *     - not evaluate all the arguments
                         *
                         * - FUNCTYPE_2N
                         *     - call the function with each 2 objs
                         *
                         * - FUNCTYPE_0
                         * - FUNCTYPE_1
                         * - FUNCTYPE_2
                         * - FUNCTYPE_3
                         * - FUNCTYPE_4
                         * - FUNCTYPE_5
                         *     - call the function with 0-5 arguments
                         */
                        switch (SCM_FUNC_NUMARG(tmp)) {
                            case FUNCTYPE_L:
                                {
                                    ret = SCM_FUNC_EXEC_SUBRL(tmp,
							      map_eval(SCM_CDR(obj), env),
							      env);
				    goto eval_done;
                                }
                            case FUNCTYPE_R:
                                {
                                    obj = SCM_FUNC_EXEC_SUBRR(tmp,
                                                              SCM_CDR(obj),
                                                              &env,
                                                              &tail_flag);

                                    /*
                                     * The core point of tail-recursion
                                     *
                                     * if tail_flag == 1, SCM_FUNC_EXEC_SUBRR returns raw S-expression.
                                     * So we need to evaluate it! This is for not to consume stack,
                                     * that is, tail-recursion optimization.
                                     */
                                    if (tail_flag == 1)
                                        goto eval_loop;

				    ret = obj;
				    goto eval_done;
                                }
                            case FUNCTYPE_2N:
                                {
                                    obj = SCM_CDR(obj);

                                    /* check 1st arg */
                                    if (SCM_NULLP(obj)) {
                                        ret =  SCM_FUNC_EXEC_SUBR2N(tmp, SCM_NIL, SCM_NIL);
					goto eval_done;
				    }

                                    /* eval 1st arg */
                                    ret = ScmOp_eval(SCM_CAR(obj), env);

                                    /* check 2nd arg  */
                                    if (SCM_NULLP(SCM_CDR(obj))) {
                                        ret = SCM_FUNC_EXEC_SUBR2N(tmp, ret, SCM_NIL);
					goto eval_done;
				    }

                                    /* call proc with each 2 objs */
                                    for (obj = SCM_CDR(obj); !SCM_NULLP(obj); obj = SCM_CDR(obj)) {
                                        ret = SCM_FUNC_EXEC_SUBR2N(tmp,
                                                                   ret,
                                                                   ScmOp_eval(SCM_CAR(obj), env));
                                    }
				    goto eval_done;
                                }
                            case FUNCTYPE_0:
				{
				    ret = SCM_FUNC_EXEC_SUBR0(tmp);
				    goto eval_done;
				}
                            case FUNCTYPE_1:
				{
				    ret = SCM_FUNC_EXEC_SUBR1(tmp, ScmOp_eval(SCM_CAR(SCM_CDR(obj)),env));
				    goto eval_done;
				}
                            case FUNCTYPE_2:
                                {
                                    obj = SCM_CDR(obj);
                                    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
                                    ret = SCM_FUNC_EXEC_SUBR2(tmp,
							      arg,
							      ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env)); /* 2nd arg */
				    goto eval_done;
                                }
                            case FUNCTYPE_3:
                                {
                                    obj = SCM_CDR(obj);
                                    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
                                    obj = SCM_CDR(obj);
                                    ret = SCM_FUNC_EXEC_SUBR3(tmp,
							      arg,
							      ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env)); /* 3rd arg */
				    goto eval_done;
                                }
                            case FUNCTYPE_4:
                                {
                                    obj = SCM_CDR(obj);
                                    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
                                    obj = SCM_CDR(obj);
                                    ret = SCM_FUNC_EXEC_SUBR4(tmp,
							      arg,
							      ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env), /* 3rd arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(obj))), env)); /* 4th arg */
				    goto eval_done;
                                }
                            case FUNCTYPE_5:
                                {
                                    obj = SCM_CDR(obj);
                                    arg = ScmOp_eval(SCM_CAR(obj), env); /* 1st arg */
                                    obj = SCM_CDR(obj);
                                    ret = SCM_FUNC_EXEC_SUBR5(tmp,
							      arg,
							      ScmOp_eval(SCM_CAR(obj), env), /* 2nd arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(obj)), env), /* 3rd arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(obj))), env), /* 4th arg */
							      ScmOp_eval(SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(obj)))), env)); /* 5th arg */
				    goto eval_done;
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
                            } else if (SCM_CONSP(arg)) {
                                /*
                                 * (2) : (<variable1> <variable2> ...)
                                 * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
                                 *
                                 *  - dot list is handled in lookup_frame().
                                 */
                                env = extend_environment(arg,
                                                         map_eval(SCM_CDR(obj), env),
                                                         SCM_CLOSURE_ENV(tmp));
                            } else if (SCM_NULLP(arg)) {
                                /*
                                 * (2') : <variable> is '()
                                 */
                                env = extend_environment(SCM_NIL,
                                                         SCM_NIL,
                                                         SCM_CLOSURE_ENV(tmp));
                            } else {
                                SigScm_ErrorObj("lambda : bad syntax with ", arg);
                            }

                            /*
                             * Notice
                             *
                             * The return obj of ScmExp_begin is the raw S-expression.
                             * So we need to re-evaluate this!.
                             */
                            obj = ScmExp_begin(SCM_CDR(SCM_CLOSURE_EXP(tmp)), &env, &tail_flag);
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
                        SigScm_ErrorObj("invalid application: ", obj);
                    default:
                        /* What? */
                        SigScm_ErrorObj("eval : What type of function? ", arg);
                }

            }
        default:
	    ret = obj;
	    goto eval_done;
    }

eval_done:
    trace_root = frame.prev;
    return ret;
}

ScmObj ScmOp_apply(ScmObj args, ScmObj env)
{
    ScmObj proc  = SCM_NIL;
    ScmObj obj   = SCM_NIL;
    int tail_flag = 0;

    /* sanity check */
    if CHECK_2_ARGS(args)
        SigScm_Error("apply : Wrong number of arguments\n");
    if (!SCM_NULLP(SCM_CDR(SCM_CDR(args))))
	SigScm_Error("apply : Doesn't support multiarg apply\n");

    /* 1st elem of list is proc */
    proc = SCM_CAR(args);

    /* 2nd elem of list is obj */
    obj  = SCM_CAR(SCM_CDR(args));

    /* apply proc */
    switch (SCM_GETTYPE(proc)) {
        case ScmFunc:
            switch (SCM_FUNC_NUMARG(proc)) {
                case FUNCTYPE_L:
                    {
                        return SCM_FUNC_EXEC_SUBRL(proc,
                                                   obj,
                                                   env);
                    }
                case FUNCTYPE_2N:
                    {
                        args = obj;

                        /* check 1st arg */
                        if (SCM_NULLP(args))
                            return SCM_FUNC_EXEC_SUBR2N(proc, SCM_NIL, SCM_NIL);

                        /* eval 1st arg */
                        obj  = SCM_CAR(args);

                        /* check 2nd arg */
                        if (SCM_NULLP(SCM_CDR(args)))
                            return SCM_FUNC_EXEC_SUBR2N(proc, obj, SCM_NIL);

                        /* call proc with each 2 objs */
                        for (args = SCM_CDR(args); !SCM_NULLP(args); args = SCM_CDR(args)) {
                            obj = SCM_FUNC_EXEC_SUBR2N(proc,
                                                       obj,
                                                       SCM_CAR(args));
                        }
                        return obj;
                    }
                case FUNCTYPE_0:
                    {
                        return SCM_FUNC_EXEC_SUBR0(proc);
                    }
                case FUNCTYPE_1:
                    {
                        return SCM_FUNC_EXEC_SUBR1(proc,
                                                   SCM_CAR(obj));
                    }
                case FUNCTYPE_2:
                    {
                        return SCM_FUNC_EXEC_SUBR2(proc,
                                                   SCM_CAR(obj),
                                                   SCM_CAR(SCM_CDR(obj)));
                    }
                case FUNCTYPE_3:
                    {
                        return SCM_FUNC_EXEC_SUBR3(proc,
                                                   SCM_CAR(obj),
                                                   SCM_CAR(SCM_CDR(obj)),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(obj))));
                    }
                case FUNCTYPE_4:
                    {
                        return SCM_FUNC_EXEC_SUBR4(proc,
                                                   SCM_CAR(obj),
                                                   SCM_CAR(SCM_CDR(obj)),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(obj))),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(obj)))));
                    }
                case FUNCTYPE_5:
                    {
                        return SCM_FUNC_EXEC_SUBR5(proc,
                                                   SCM_CAR(obj),
                                                   SCM_CAR(SCM_CDR(obj)),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(obj))),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(obj)))),
                                                   SCM_CAR(SCM_CDR(SCM_CDR(SCM_CDR(SCM_CDR(obj))))));
                    }
                default:
                    SigScm_ErrorObj("apply : invalid application ", proc);
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
                args = SCM_CAR(SCM_CLOSURE_EXP(proc)); /* arg is <formals> */

                if (SCM_SYMBOLP(args)) {
                    /* (1) : <variable> */
                    env = extend_environment(Scm_NewCons(args, SCM_NIL),
                                             Scm_NewCons(obj, SCM_NIL),
                                             SCM_CLOSURE_ENV(proc));
                } else if (SCM_CONSP(args)) {
                    /*
                     * (2) : (<variable1> <variable2> ...)
                     * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
                     *
                     *  - dot list is handled in lookup_frame().
                     */
                    env = extend_environment(args,
                                             obj,
                                             SCM_CLOSURE_ENV(proc));
                } else if (SCM_NULLP(args)) {
                    /*
                     * (2') : <variable> is '()
                     */
                    env = extend_environment(SCM_NIL,
                                             SCM_NIL,
                                             SCM_CLOSURE_ENV(proc));
                } else {
                    SigScm_ErrorObj("lambda : bad syntax with ", args);
                }

                /*
                 * Notice
                 *
                 * The return obj of ScmExp_begin is the raw S-expression.
                 * So we need to re-evaluate this!.
                 */
                obj = ScmExp_begin(SCM_CDR(SCM_CLOSURE_EXP(proc)), &env, &tail_flag);
                return ScmOp_eval(obj, env);
            }
        default:
            SigScm_ErrorObj("apply : invalid application ", args);
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

    /* first, lookup the environment */
    val = lookup_environment(var, env);
    if (!SCM_NULLP(val)) {
        /* variable is found in environment, so returns its value */
        return SCM_CAR(val);
    }

    /* next, lookup the special environment for letrec */
    val = lookup_environment(var, letrec_env);
    if (!SCM_NULLP(val)) {
        /* variable is found in letrec environment, so returns its value */
        return SCM_CAR(val);
    }

    /* finally, look at the VCELL */
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


/**
 * The big bad full-implementation of quasiquote.
 * 
 * @param qexpr The expression given to quasiquote.
 * @param env The effective environment.
 * @param nest Nesting level of quasiquote.  This function is recursive.
 * 
 * @return If qexpr or any of its subexpressions was evaluated, then
 * (do-unquotes qexpr) is returned.  Otherwise, the return
 * value will test true for QQUOTE_IS_VERBATIM().
 *
 * @see qquote_vector()
 */
static ScmObj qquote_internal(ScmObj qexpr, ScmObj env, int nest)
{
    ScmObj ls        = SCM_NIL;
    ScmObj obj       = SCM_NIL;
    ScmObj car       = SCM_NIL;
    ScmObj args      = SCM_NIL;
    ScmObj result    = SCM_NIL;
    ScmObj ret_list  = SCM_NIL;
    ScmObj *ret_tail = NULL;
    int splice_flag  = 0;

    /* local "functions" */
#define qquote_copy_delayed()   (QQUOTE_IS_VERBATIM(ret_list))
#define qquote_force_copy_upto(end) \
    do { \
	ScmObj src = qexpr; \
	ret_tail = &ret_list; \
	while (!EQ(src, end)) { \
	    *ret_tail = Scm_NewCons(SCM_CAR(src), SCM_NIL); \
	    ret_tail = &SCM_CDR(*ret_tail); \
	    src = SCM_CDR(src); \
	} \
    } while (0)


    QQUOTE_SET_VERBATIM(ret_list); /* default return value */

    if (SCM_CONSP(qexpr)) {
	car = SCM_CAR(qexpr);
	args = SCM_CDR(qexpr);

	if (EQ(car, SCM_UNQUOTE_SPLICING)) {
	    if (!IS_LIST_LEN_1(args))
		SigScm_ErrorObj("syntax error: ", qexpr);
	    if (--nest == 0)
		return ScmOp_eval(SCM_CAR(args), env);
	}
	else if (EQ(car, SCM_QUASIQUOTE)) {
	    if (!IS_LIST_LEN_1(args))
		SigScm_ErrorObj("syntax error: ", qexpr);
	    if (++nest <= 0)
		SigScm_Error("quasiquote: nesting too deep (circular list?)");
	}
    }

    for (ls = qexpr; SCM_CONSP(ls); ls = SCM_CDR(ls)) {
	obj = SCM_CAR(ls);
	splice_flag = 0;

	if (SCM_CONSP(obj)) {
	    result = qquote_internal(obj, env, nest);

	    if (EQ(SCM_CAR(obj), SCM_UNQUOTE_SPLICING) && nest == 1) {
		/* ,@x */
		splice_flag = 1;
	    }
	} else if (SCM_VECTORP(obj)) {
	    /* #(x) */
	    result = qquote_vector(obj, env, nest);
	} else if (EQ(obj, SCM_UNQUOTE) && IS_LIST_LEN_1(SCM_CDR(ls))) {
	    /* we're at the comma in (x . ,y) or qexpr was ,z */
	    if (--nest == 0) {
		result = ScmOp_eval(SCM_CADR(ls), env);
		goto append_last_item;
	    }
	    QQUOTE_SET_VERBATIM(result);
	} else {
	    /* atom */
	    QQUOTE_SET_VERBATIM(result);
	}

	if (QQUOTE_IS_VERBATIM(result)) {
	    if (!qquote_copy_delayed()) {
		*ret_tail = Scm_NewCons(obj, SCM_NIL);
		ret_tail = &SCM_CDR(*ret_tail);
	    }
	} else {
	    if (qquote_copy_delayed())
		qquote_force_copy_upto(ls);

	    if (splice_flag) {
		*ret_tail = result;
		/* find the new tail (which may be the current pos) */
		while (SCM_CONSP(*ret_tail))
		    ret_tail = &SCM_CDR(*ret_tail);
		if (!SCM_NULLP(*ret_tail))
		    SigScm_ErrorObj("unquote-splicing: bad list: ",
				    result);
	    } else {
		*ret_tail = Scm_NewCons(result, SCM_NIL);
		ret_tail = &SCM_CDR(*ret_tail);
	    }
	}
    } /* foreach ls in qexpr */

    /* Handle the leftover of an improper list; if qexpr is a proper
     * list, all the following will be a no-op. */    
    if (SCM_VECTORP(ls))
	result = qquote_vector(ls, env, nest);
    else
	QQUOTE_SET_VERBATIM(result);

  append_last_item:
    if (QQUOTE_IS_VERBATIM(result)) {
	if (!qquote_copy_delayed())
	    *ret_tail = ls;
    } else {
	if (qquote_copy_delayed())
	    qquote_force_copy_upto(ls);
	*ret_tail = result;
    }

    return ret_list;
#undef qquote_is_spliced
#undef qquote_copy_delayed
#undef qquote_force_copy_upto
}

/**
 * The semantics are the same as qquote_internal, except the first
 * argument should be a vector.  Adapted some ideas from Gauche,
 * another Scheme implementation by Shiro Kawai.
 *
 * @see qquote_internal()
 */
static ScmObj qquote_vector(ScmObj src, ScmObj env, int nest)
{
    ScmObj splices    = SCM_NIL;
    ScmObj expr       = SCM_NIL;
    ScmObj ret        = SCM_NIL;
    ScmObj *copy_buf  = NULL;
    ScmObj result     = SCM_NIL;
    ScmObj splice_len = SCM_NIL;
    int len = SCM_VECTOR_LEN(src);
    int growth = 0;
    int next_splice_index = -1;
    int i = 0;
    int j = 0;

    /* local "functions" */
#define qquote_copy_delayed() (copy_buf == NULL)
#define qquote_is_spliced(o)  \
    (SCM_CONSP(o) && EQ(SCM_CAR(o), SCM_UNQUOTE_SPLICING))
#define qquote_force_copy_upto(n) \
    do { \
	int k; \
	copy_buf = (ScmObj*)malloc((len + growth) * sizeof(ScmObj)); \
	memcpy(copy_buf, SCM_VECTOR_VEC(src), n*sizeof(ScmObj)); \
	/* wrap it now, or a cont invocation can leak it */ \
	ret = Scm_NewVector(copy_buf, len + growth); \
	/* fill with something the garbage collector recognizes */ \
	for (k=n; k < len + growth; k++) \
	    copy_buf[k] = SCM_NIL; \
    } while(0)

    QQUOTE_SET_VERBATIM(ret);
    copy_buf = NULL;

    if (nest == 1) {
	/* Evaluate all the splices first, in reverse order, and store
	 * them in a list ((ls . index) (ls . index)...). */
	for (i = len - 1; i >= 0; i--) {
	    expr = SCM_VECTOR_CREF(src, i);
	    if (qquote_is_spliced(expr)) {
		if (!IS_LIST_LEN_1(SCM_CDR(expr)))
		    SigScm_ErrorObj("syntax error: ", expr);

		result = ScmOp_eval(SCM_CADR(expr), env);

		splice_len = ScmOp_length(result);
		if (SCM_INT_VALUE(splice_len) < 0)
		    SigScm_Error("unquote-splicing: bad list");

		growth += SCM_INT_VALUE(splice_len) - 1;
		splices = Scm_NewCons(Scm_NewCons(result, Scm_NewInt(i)),
				      splices);
	    }
	}
	if (!SCM_NULLP(splices)) {
	    next_splice_index = SCM_INT_VALUE(SCM_CDAR(splices));
	    qquote_force_copy_upto(0);
	}
    }

    for (i = j = 0; i < len; i++) {
	/* j will be the index for copy_buf */
	if (i == next_splice_index) {
	    /* spliced */
	    for (expr=SCM_CAAR(splices); !SCM_NULLP(expr); expr=SCM_CDR(expr))
		copy_buf[j++] = SCM_CAR(expr);
	    splices = SCM_CDR(splices);

	    if (SCM_NULLP(splices))
		next_splice_index = -1;
	    else
		next_splice_index = SCM_INT_VALUE(SCM_CDAR(splices));
	    /* continue; */
	} else {
	    expr = SCM_VECTOR_CREF(src, i);
	    if (SCM_CONSP(expr))
		result = qquote_internal(expr, env, nest);
	    else if (SCM_VECTORP(expr))
		result = qquote_vector(expr, env, nest);
	    else
		QQUOTE_SET_VERBATIM(result);

	    if (!QQUOTE_IS_VERBATIM(result)) {
		if (qquote_copy_delayed())
		    qquote_force_copy_upto(i);

		copy_buf[j] = result;
	    } else if (!qquote_copy_delayed()) {
		copy_buf[j] = expr;
	    }

	    j++;
	}
    }

    return ret;
#undef qquote_copy_delayed
#undef qquote_force_copy_upto
}

/*=======================================
  R5RS : 4.1 Primitive expression types
=======================================*/
/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.2 Literal expressions
===========================================================================*/
ScmObj ScmOp_quote(ScmObj obj, ScmObj *envp, int *tail_flag)
{
    if (!SCM_CONSP(obj) || !SCM_NULLP(SCM_CDR(obj)))
	SigScm_ErrorObj("quote: bad argument list: ", obj);
    *tail_flag = 0;
    return SCM_CAR(obj);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj ScmExp_lambda(ScmObj exp, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;

    /* set tail_flag */
    (*tail_flag) = 0;

    if CHECK_2_ARGS(exp)
        SigScm_ErrorObj("lambda : too few argument ", exp);

    return Scm_NewClosure(exp, env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj ScmExp_if(ScmObj exp, ScmObj *envp, int *tail_flag)
{
    ScmObj env       = *envp;
    ScmObj pred      = SCM_NIL;
    ScmObj false_exp = SCM_NIL;

    /* set tail_flag */
    (*tail_flag) = 1;

    /* sanity check */
    if (SCM_NULLP(exp) || SCM_NULLP(SCM_CDR(exp)))
        SigScm_Error("if : syntax error\n");

    /* eval predicates */
    pred = ScmOp_eval(SCM_CAR(exp), env);

    /* if pred is SCM_TRUE */
    if (!EQ(pred, SCM_FALSE)) {
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
ScmObj ScmExp_set(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj sym = SCM_CAR(arg);
    ScmObj val = SCM_CAR(SCM_CDR(arg));
    ScmObj ret = SCM_NIL;
    ScmObj tmp = SCM_NIL;

    /* set tail_flag */
    (*tail_flag) = 0;

    ret = ScmOp_eval(val, env);
    tmp = lookup_environment(sym, env);
    if (SCM_NULLP(tmp)) {
        /*
         * not found in the environment
         * if symbol is not bounded, error occurs
         */
        if (EQ(ScmOp_symbol_boundp(sym), SCM_FALSE))
            SigScm_ErrorObj("set! : unbound variable ", sym);

        SCM_SETSYMBOL_VCELL(sym, ret);
    } else {
        /* found in the environment*/
        SCM_SETCAR(tmp, ret);
    }

    /* set new env */
    *envp = env;

    return ret;
}


/*=======================================
  R5RS : 4.2 Derived expression types
=======================================*/
/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.1 Conditionals
===========================================================================*/
ScmObj ScmExp_cond(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    /*
     * (cond <clause1> <clause2> ...)
     *
     * <clause> should be the form:
     *     (<test> <expression1> <expression2> ...)
     *
     * <clause> may be of the form
     *     (<test> => <expression)
     */
    ScmObj env    = *envp;
    ScmObj clause = SCM_NIL;
    ScmObj test   = SCM_NIL;
    ScmObj exps   = SCM_NIL;
    ScmObj proc   = SCM_NIL;

    /* set tail_flag */
    (*tail_flag) = 0;

    /* looping in each clause */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
        clause = SCM_CAR(arg);
        test   = SCM_CAR(clause);
        exps   = SCM_CDR(clause);

        if (SCM_NULLP(clause) || SCM_NULLP(test))
            SigScm_Error("cond : syntax error\n");

        /* evaluate test */
        test = ScmOp_eval(test, env);

        /* check the result */
        if (!SCM_EQ(test, SCM_FALSE)) {
            /*
             * if the selected <clause> contains only the <test> and no <expression>s,
             * then the value of the <test> is returned as the result.
             */
            if (SCM_NULLP(exps))
                return test;

            /*
             * If the selected <clause> uses the => alternate form, then the <expression>
             * is evaluated. Its value must be a procedure that accepts one argument;
             * this procedure is then called on the value of the <test> and the value
             * returned by this procedure is returned by the cond expression.
             */
            if (SCM_EQ(Scm_Intern("=>"), SCM_CAR(exps))) {
                proc = ScmOp_eval(SCM_CAR(SCM_CDR(exps)), env);
                if (EQ(ScmOp_procedurep(proc), SCM_FALSE))
                    SigScm_ErrorObj("cond : the value of exp after => must be the procedure but got ", proc);
                
                return ScmOp_apply(Scm_NewCons(proc,
                                               Scm_NewCons(Scm_NewCons(test, SCM_NIL),
                                                           SCM_NIL)),
                                   env);
            }
            
            return ScmExp_begin(exps, &env, tail_flag);
        }
    }

    return SCM_UNSPECIFIED;
}

ScmObj ScmExp_case(ScmObj arg, ScmObj *envp, int *tail_flag)
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
            SigScm_Error("case : syntax error\n");

        /* check "else" symbol */
        if (SCM_NULLP(SCM_CDR(arg)) && !SCM_CONSP(datums) && EQ(SCM_SYMBOL_VCELL(datums), SCM_TRUE))
            return ScmExp_begin(exps, &env, tail_flag);

        /* evaluate datums and compare to key by eqv? */
        for (; !SCM_NULLP(datums); datums = SCM_CDR(datums)) {
            if (EQ(ScmOp_eqvp(SCM_CAR(datums), key), SCM_TRUE)) {
                return ScmExp_begin(exps, &env, tail_flag);
            }
        }
    }

    return SCM_UNSPECIFIED;
}

ScmObj ScmExp_and(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg))
        return SCM_TRUE;
    if (EQ(ScmOp_listp(arg), SCM_FALSE))
        SigScm_ErrorObj("and : list required but got ", arg);

    /* check recursively */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
        obj = SCM_CAR(arg);

        /* return last item */
        if (SCM_NULLP(SCM_CDR(arg))) {
            /* set tail_flag */
            (*tail_flag) = 1;

            return obj;
        }

        /* evaluate obj */
        obj = ScmOp_eval(obj, env);
        if (EQ(obj, SCM_FALSE)) {
            /* set tail_flag */
            (*tail_flag) = 0;

            return SCM_FALSE;
        }
    }

    return SCM_NIL;
}

ScmObj ScmExp_or(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg))
        return SCM_FALSE;
    if (EQ(ScmOp_listp(arg), SCM_FALSE))
        SigScm_ErrorObj("or : list required but got ", arg);

    /* check recursively */
    for (; !SCM_NULLP(arg); arg = SCM_CDR(arg)) {
        obj = SCM_CAR(arg);

        /* return last item */
        if (SCM_NULLP(SCM_CDR(arg))) {
            /* set tail_flag */
            (*tail_flag) = 1;

            return obj;
        }

        obj = ScmOp_eval(obj, env);
        if (!EQ(obj, SCM_FALSE)) {
            /* set tail_flag */
            (*tail_flag) = 0;

            return obj;
        }

    }

    return SCM_NIL;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
ScmObj ScmExp_let(ScmObj arg, ScmObj *envp, int *tail_flag)
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

    /* guess whether syntax is "Named let" */
    if (SCM_SYMBOLP(SCM_CAR(arg)))
        goto named_let;

    /* get bindings and body */
    bindings = SCM_CAR(arg);
    body     = SCM_CDR(arg);

    /*========================================================================
      (let <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (SCM_CONSP(bindings) || SCM_NULLP(bindings)) {
        for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
            binding = SCM_CAR(bindings);
            vars = Scm_NewCons(SCM_CAR(binding), vars);
            vals = Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), vals);
        }

        /* create new environment for */
        env = extend_environment(vars, vals, env);
        *envp = env;

        return ScmExp_begin(body, &env, tail_flag);
    }

    return ScmExp_begin(body, &env, tail_flag);

named_let:
    /*========================================================================
      (let <variable> <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    bindings = SCM_CAR(SCM_CDR(arg));
    body     = SCM_CDR(SCM_CDR(arg));
    for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
        binding = SCM_CAR(bindings);
        vars = Scm_NewCons(SCM_CAR(binding), vars);
        vals = Scm_NewCons(SCM_CAR(SCM_CDR(binding)), vals);
    }

    vars = ScmOp_reverse(vars);
    vals = ScmOp_reverse(vals);

    /* (define (<variable> <variable1> <variable2> ...>) <body>) */
    ScmExp_define(Scm_NewCons(Scm_NewCons(SCM_CAR(arg),
                                          vars),
                              body),
                  &env, tail_flag);

    /* set tail_flag */
    (*tail_flag) = 1;

    /* (func <init1> <init2> ...) */
    return Scm_NewCons(SCM_CAR(arg), vals);
}

ScmObj ScmExp_let_star(ScmObj arg, ScmObj *envp, int *tail_flag)
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
        /* evaluate */
        return ScmExp_begin(body, &env, tail_flag);
    } else if (SCM_NULLP(bindings)) {
        /* extend null environment */
        env = extend_environment(SCM_NIL,
                                 SCM_NIL,
                                 env);

        /* set new env */
        *envp = env;
        /* evaluate */
        return ScmExp_begin(body, &env, tail_flag);
    }

    /* set tail_flag */
    (*tail_flag) = 0;

    return SCM_UNDEF;
}

ScmObj ScmExp_letrec(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NIL;
    ScmObj body     = SCM_NIL;
    ScmObj vars     = SCM_NIL;
    ScmObj vals     = SCM_NIL;
    ScmObj binding  = SCM_NIL;
    ScmObj var      = SCM_NIL;
    ScmObj val      = SCM_NIL;
    ScmObj frame    = SCM_NIL;

    /* sanity check */
    if (SCM_NULLP(arg) || SCM_NULLP(SCM_CDR(arg)))
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
    if (SCM_CONSP(bindings) || SCM_NULLP(bindings)) {
        for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
            binding = SCM_CAR(bindings);
            var = SCM_CAR(binding);
            val = SCM_CAR(SCM_CDR(binding));

            /* construct vars and vals list */
            vars = Scm_NewCons(var, vars);
            vals = Scm_NewCons(val, vals);
        }

        /* construct new frame for letrec_env */
        frame = Scm_NewCons(vars, vals);
        letrec_env = Scm_NewCons(frame, letrec_env);

        /* extend environment by letrec_env */
        env = extend_environment(SCM_CAR(frame), SCM_CDR(frame), env);

        /* ok, vars of letrec is extended to env */
        letrec_env = SCM_NIL;

        /* set new env */
        *envp = env;

        /* evaluate vals */
        for (; !SCM_NULLP(vals); vals = SCM_CDR(vals)) {
            SCM_SETCAR(vals, ScmOp_eval(SCM_CAR(vals), env));
        }
        
        /* evaluate body */
        return ScmExp_begin(body, &env, tail_flag);
    }

    /* set tail_flag */
    (*tail_flag) = 0;

    SigScm_Error("letrec : syntax error\n");
    return SCM_UNDEF;
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj ScmExp_begin(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj exp = SCM_NIL;
    
    /* set tail_flag */
    (*tail_flag) = 1;

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
            /* doesn't evaluate exp now for tail-recursion. */
            return exp; 
        }

        /* evaluate exp */
        ScmOp_eval(exp, env);

        /* set new env */
        *envp = env;    
    }

    /* set tail_flag */
    (*tail_flag) = 0;

    return SCM_UNDEF;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
ScmObj ScmExp_do(ScmObj arg, ScmObj *envp, int *tail_flag)
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
    ScmObj tmp_vars   = SCM_NIL;
    ScmObj tmp_steps  = SCM_NIL;
    ScmObj obj        = SCM_NIL;

    /* sanity check */
    if (SCM_INT_VALUE(ScmOp_length(arg)) < 2)
        SigScm_Error("do : syntax error\n");

    /* construct Environment and steps */
    for (; !SCM_NULLP(bindings); bindings = SCM_CDR(bindings)) {
        binding = SCM_CAR(bindings);
        vars = Scm_NewCons(SCM_CAR(binding), vars);
        vals = Scm_NewCons(ScmOp_eval(SCM_CAR(SCM_CDR(binding)), env), vals);

        /* append <step> to steps */
        step = SCM_CDR(SCM_CDR(binding));
        if (SCM_NULLP(step))
            steps = Scm_NewCons(SCM_CAR(binding), steps);       
        else
            steps = Scm_NewCons(SCM_CAR(step), steps);
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
    while (SCM_EQ(ScmOp_eval(test, env), SCM_FALSE)) {
        /* execute commands */
        ScmOp_eval(ScmExp_begin(commands, &env, tail_flag), env);

        /*
         * Notice
         *
         * the result of the execution of <step>s must not depend on each other's
         * results. each excution must be done independently. So, we store the
         * results to the "vals" variable and set it in hand.
         */
        vals = SCM_NIL;
        for (tmp_steps = steps; !SCM_NULLP(tmp_steps); tmp_steps = SCM_CDR(tmp_steps)) {
            vals = Scm_NewCons(ScmOp_eval(SCM_CAR(tmp_steps), env), vals);
        }
        vals = ScmOp_reverse(vals);

        /* set it */
        for (tmp_vars = vars; !SCM_NULLP(tmp_vars) && !SCM_NULLP(vals); tmp_vars = SCM_CDR(tmp_vars), vals = SCM_CDR(vals)) {
            obj = lookup_environment(SCM_CAR(tmp_vars), env);
            if (!SCM_NULLP(obj)) {
                SCM_SETCAR(obj, SCM_CAR(vals));
            } else {
                SigScm_Error("do : broken env\n");
            }
        }
    }

    /* set new env */
    *envp = env;

    return ScmExp_begin(expression, &env, tail_flag);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
ScmObj ScmOp_delay(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;

    /* set tail_flag */
    (*tail_flag) = 0;

    if (SCM_INT_VALUE(ScmOp_length(arg)) != 1)
        SigScm_Error("delay : Wrong number of arguments\n");

    /* closure exp = ( () SCM_CAR(arg) ) */
    return Scm_NewClosure(Scm_NewCons(SCM_NIL, Scm_NewCons(SCM_CAR(arg), SCM_NIL)), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/
ScmObj ScmOp_quasiquote(ScmObj obj, ScmObj *envp, int *tail_flag)
{
    ScmObj ret;
    if (!IS_LIST_LEN_1(obj))
	SigScm_ErrorObj("quasiquote: bad argument list: ", obj);
    obj = SCM_CAR(obj);
    ret = qquote_internal(obj, *envp, 1);

    *tail_flag = 0;
    if (QQUOTE_IS_VERBATIM(ret))
	return obj;
    return ret;
}

ScmObj ScmOp_unquote(ScmObj obj, ScmObj *envp, int *tail_flag)
{
    if (!SCM_CONSP(obj) || !SCM_NULLP(SCM_CDR(obj)))
	SigScm_ErrorObj("unquote: bad argument list: ", obj);
    SigScm_Error("unquote outside quasiquote");
    return SCM_NIL;
}

ScmObj ScmOp_unquote_splicing(ScmObj obj, ScmObj *envp, int *tail_flag)
{
    if (!SCM_CONSP(obj) || !SCM_NULLP(SCM_CDR(obj)))
	SigScm_ErrorObj("unquote-splicing: bad argument list: ", obj);
    SigScm_Error("unquote-splicing outside quasiquote");
    return SCM_NIL;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
ScmObj ScmExp_define(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env     = *envp;
    ScmObj var     = SCM_CAR(arg);
    ScmObj body    = SCM_CAR(SCM_CDR(arg));
    ScmObj val     = SCM_NIL;
    ScmObj formals = SCM_NIL;

    /* set tail_flag */
    (*tail_flag) = 0;

    /* sanity check */
    if (SCM_NULLP(var))
        SigScm_ErrorObj("define : syntax error ", arg);

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SCM_SYMBOLP(var)) {
        if (SCM_NULLP(env)) {
            /* given NIL environment */
            SCM_SETSYMBOL_VCELL(var, ScmOp_eval(body, env));
        } else {
            /* add val to the environment */
            env = add_environment(var, ScmOp_eval(body, env), env);
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
        arg = Scm_NewCons(val, Scm_NewCons(ScmExp_lambda(Scm_NewCons(formals, body), &env, tail_flag),
                                           SCM_NIL));

        return ScmExp_define(arg, &env, tail_flag);
    }

    SigScm_ErrorObj("define : syntax error ", arg);
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

#if SCM_COMPAT_SIOD
/*=======================================
  SIOD compatible procedures

  TODO : remove these functions!
=======================================*/
ScmObj ScmOp_symbol_boundp(ScmObj obj)
{
    if (SCM_SYMBOLP(obj)
        && !SCM_EQ(SCM_SYMBOL_VCELL(obj), SCM_UNBOUND))
    {
        return SCM_TRUE;
    }

    return SCM_FALSE;
}

ScmObj ScmOp_symbol_value(ScmObj var)
{
    if (!SCM_SYMBOLP(var))
	SigScm_ErrorObj("symbol-value : require symbol but got ", var);

    return symbol_value(var, SCM_NIL);
}

ScmObj ScmOp_set_symbol_value(ScmObj var, ScmObj val)
{
    /* sanity check */
    if (!SCM_SYMBOLP(var))
	SigScm_ErrorObj("set-symbol-value! : require symbol but got ", var);

    return SCM_SYMBOL_VCELL(var);
}

ScmObj ScmOp_bit_and(ScmObj obj1, ScmObj obj2)
{
    if (!SCM_INTP(obj1))
	SigScm_ErrorObj("bit-and : number required but got ", obj1);
    if (!SCM_INTP(obj2))
	SigScm_ErrorObj("bit-and : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) & SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_or(ScmObj obj1, ScmObj obj2)
{
    if (!SCM_INTP(obj1))
	SigScm_ErrorObj("bit-or : number required but got ", obj1);
    if (!SCM_INTP(obj2))
	SigScm_ErrorObj("bit-or : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) | SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_xor(ScmObj obj1, ScmObj obj2)
{
    if (!SCM_INTP(obj1))
	SigScm_ErrorObj("bit-xor : number required but got ", obj1);
    if (!SCM_INTP(obj2))
	SigScm_ErrorObj("bit-xor : number required but got ", obj2);

    return Scm_NewInt(SCM_INT_VALUE(obj1) ^ SCM_INT_VALUE(obj2));
}

ScmObj ScmOp_bit_not(ScmObj obj)
{
    if (!SCM_INTP(obj))
	SigScm_ErrorObj("bit-not : number required but got ", obj);

    return Scm_NewInt(~SCM_INT_VALUE(obj));
}

ScmObj ScmOp_the_environment(ScmObj arg, ScmObj env)
{
    return env;
}

ScmObj ScmOp_closure_code(ScmObj closure)
{
    if (!SCM_CLOSUREP(closure))
	SigScm_ErrorObj("%%closure-code : closure required but got ", closure);

    return SCM_CLOSURE_EXP(closure);
}
#endif
