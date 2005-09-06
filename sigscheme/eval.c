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
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#define SCM_INVALID NULL        /* TODO: make a more appropriate choice */

#define IS_LIST_LEN_1(args)  (CONSP(args) && NULLP(CDR(args)))
/* for the quasiquote family */
#define QQUOTE_SET_VERBATIM(x) ((x) = SCM_INVALID)
#define QQUOTE_IS_VERBATIM(x)  (EQ((x), SCM_INVALID))

#define SCM_ERRMSG_WRONG_NR_ARG " Wrong number of arguments "

/*=======================================
  Variable Declarations
=======================================*/
ScmObj scm_continuation_thrown_obj = NULL; /* for storing continuation return object */
ScmObj scm_letrec_env = NULL;              /* for storing environment obj of letrec */

struct trace_frame *scm_trace_root = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj map_eval(ScmObj args, ScmObj env);
static ScmObj qquote_internal(ScmObj expr, ScmObj env, int nest);
static ScmObj qquote_vector(ScmObj vec, ScmObj env, int nest);

/*=======================================
  Function Implementations
=======================================*/
ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame    = SCM_NULL;
    ScmObj tmp_vars = vars;
    ScmObj tmp_vals = vals;

    /* handle dot list */
    while (1) {
        if (NULLP(tmp_vars) || !CONSP(tmp_vars))
            break;

        /* dot list appears */
        if (!NULLP(CDR(tmp_vars)) && !CONSP(CDR(tmp_vars))) {
            /* create new value */
            SET_CDR(tmp_vals, CONS(CDR(tmp_vals),
                                   SCM_NULL));
        }

        tmp_vars = CDR(tmp_vars);
        tmp_vals = CDR(tmp_vals);
    }

    /* create new frame */
    frame = CONS(vars, vals);

    /* add to env */
    if (NULLP(env))
        env = CONS(frame, SCM_NULL);
    else if (CONSP(env))
        env = CONS(frame, env);
    else
        SigScm_Error("Broken environment.\n");

    return env;
}

ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame, tmp;
    ScmObj new_varlist, new_vallist;

    /* sanity check */
    if (NULLP(var))
        return env;

    /* add (var val) pair to the newest frame in env */
    if (NULLP(env)) {
        newest_frame = CONS(CONS(var, SCM_NULL),
                            CONS(val, SCM_NULL));
        env = CONS(newest_frame,
                          SCM_NULL);
    } else if (CONSP(env)) {
        newest_frame = CAR(env);
        new_varlist  = CONS(var, CAR(newest_frame));
        new_vallist  = CONS(val, CDR(newest_frame));

        tmp = CONS(CONS(new_varlist, new_vallist), CDR(env));
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
ScmObj lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame = SCM_NULL;
    ScmObj val   = SCM_NULL;

    /* sanity check */
    if (NULLP(env))
        return SCM_NULL;
    if (!CONSP(env))
        SigScm_ErrorObj("Broken environent : ", env);

    /* lookup frames */
    for (; !NULLP(env); env = CDR(env)) {
        frame = CAR(env);
        val   = lookup_frame(var, frame);
        if (!NULLP(val))
            return val;
    }

    return SCM_NULL;
}

ScmObj lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vals = SCM_NULL;
    ScmObj vars = SCM_NULL;

    /* sanity check */
    if (NULLP(frame))
        return SCM_NULL;
    else if (!CONSP(frame))
        SigScm_ErrorObj("Broken frame : ", frame);

    /* lookup in frame */
    vars = CAR(frame);
    vals = CDR(frame);

    while (1) {
        if (NULLP(vars))
            break;

        if (!CONSP(vars)) {
            /* handle dot list */
            if (EQ(vars, var))
                return vals;

            break;
        } else {
            /* normal binding */
            if (EQ(CAR(vars), var))
                return vals;
        }

        vars = CDR(vars);
        vals = CDR(vals);
    }

    return SCM_NULL;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj ScmOp_eval(ScmObj obj, ScmObj env)
{
    ScmObj tmp  = SCM_NULL;
    ScmObj arg  = SCM_NULL;
    ScmObj arg0, arg1, arg2, arg3, arg4;
    ScmObj rest = SCM_NULL;
    ScmObj args = SCM_NULL;
    ScmObj ret  = SCM_NULL;
    int tail_flag = 0;

    /* for debugging */
    struct trace_frame frame;
    frame.prev = scm_trace_root;
    frame.obj  = obj;
    scm_trace_root = &frame;

eval_loop:
    switch (SCM_TYPE(obj)) {
    case ScmSymbol:
        ret = symbol_value(obj, env);
        goto eval_done;

    /*====================================================================
      Evaluating Expression
    ====================================================================*/
    case ScmCons:
        /*============================================================
          Evaluating CAR
        ============================================================*/
        tmp = CAR(obj);
        switch (SCM_TYPE(tmp)) {
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
            break;
        default:
            SigScm_ErrorObj("eval : invalid operation ", obj);
            break;
        }

        /*============================================================
          Evaluating the rest of the List by the type of CAR
        ============================================================*/
        switch (SCM_TYPE(tmp)) {
        case ScmFunc:
            /*
             * FUNCTYPE_RAW_LIST_TAIL_REC represents a form that contains tail
             * expressions, which must be evaluated without consuming storage
             * (proper tail recursion).  A function of this type returns an
             * S-expression that the caller must evaluate to obtain the
             * resultant value of the entire form.
             * FUNCYTPE_RAW_LIST_WITH_TAIL_FLAG has the same semantics, except
             * that the return value must be evaluated if and only if the
             * callee sets tail_flag (an int passed by reference) to nonzero.
             * The two types receive a *reference* to the effective environment
             * so that they can extend it as necessary.
             * 
             * FUNCTYPE_0 through 5 and FUNCTYPE_EVALED_LIST require the caller
             * to evaluate arguments.  Others do it on their own.
             * 
             * For FUNCTYPE_0 through 5, the caller checks the number of
             * arguments, and passes only the arguments.  For other types,
             * checking is the callee's reponsibility, and they receive the
             * current environment.
             */
            switch (SCM_FUNC_TYPECODE(tmp)) {
            case FUNCTYPE_EVALED_LIST:
                ret = SCM_FUNC_EXEC_SUBRL(tmp,
                                          map_eval(CDR(obj), env),
                                          env);
                goto eval_done;

            case FUNCTYPE_RAW_LIST:
                ret = SCM_FUNC_EXEC_SUBRL(tmp,
                                          CDR(obj),
                                          env);
                goto eval_done;

            case FUNCTYPE_RAW_LIST_TAIL_REC:
                obj = SCM_FUNC_EXEC_SUBRR(tmp,
                                          CDR(obj),
                                          &env);
                goto eval_loop;

            case FUNCTYPE_RAW_LIST_WITH_TAIL_FLAG:
                obj = SCM_FUNC_EXEC_SUBRF(tmp,
                                          CDR(obj),
                                          &env,
                                          &tail_flag);

                /*
                 * If tail_flag is nonzero, SCM_FUNC_EXEC_SUBRR returns a raw
                 * S-expression.  So we need to evaluate it! This is not to
                 * consume stack, that is, tail-recursion optimization.
                 */
                if (tail_flag)
                    goto eval_loop;

                ret = obj;
                goto eval_done;

            case FUNCTYPE_0:
                ret = SCM_FUNC_EXEC_SUBR0(tmp);
                goto eval_done;

            case FUNCTYPE_1:
                args = rest = CDR(obj);
                if (!NULLP(SCM_SHIFT_EVALED_1(arg0, rest, env)))
                    SigScm_ErrorObj("func1 :" SCM_ERRMSG_WRONG_NR_ARG, args);
                ret = SCM_FUNC_EXEC_SUBR1(tmp, arg0);
                goto eval_done;

            case FUNCTYPE_2:
                args = rest = CDR(obj);
                if (!NULLP(SCM_SHIFT_EVALED_2(arg0, arg1, rest, env)))
                    SigScm_ErrorObj("func2 :" SCM_ERRMSG_WRONG_NR_ARG, args);
                ret = SCM_FUNC_EXEC_SUBR2(tmp, arg0, arg1);
                goto eval_done;

            case FUNCTYPE_3:
                args = rest = CDR(obj);
                if (!NULLP(SCM_SHIFT_EVALED_3(arg0, arg1, arg2, rest, env)))
                    SigScm_ErrorObj("func3 :" SCM_ERRMSG_WRONG_NR_ARG, args);
                ret = SCM_FUNC_EXEC_SUBR3(tmp, arg0, arg1, arg2);
                goto eval_done;

            case FUNCTYPE_4:
                args = rest = CDR(obj);
                if (!NULLP(SCM_SHIFT_EVALED_4(arg0, arg1, arg2, arg3,
                                              rest, env)))
                    SigScm_ErrorObj("func4 :" SCM_ERRMSG_WRONG_NR_ARG, args);
                ret = SCM_FUNC_EXEC_SUBR4(tmp, arg0, arg1, arg2, arg3);
                goto eval_done;

            case FUNCTYPE_5:
                args = rest = CDR(obj);
                if (!NULLP(SCM_SHIFT_EVALED_5(arg0, arg1, arg2, arg3, arg4,
                                              rest, env)))
                    SigScm_ErrorObj("func5 :" SCM_ERRMSG_WRONG_NR_ARG, args);
                ret = SCM_FUNC_EXEC_SUBR5(tmp, arg0, arg1, arg2, arg3, arg4);
                goto eval_done;

            default:
                SigScm_Error("eval : unknown functype\n");
            }

        case ScmClosure:
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
            arg = CAR(SCM_CLOSURE_EXP(tmp)); /* arg is <formals> */
            
            if (SYMBOLP(arg)) {
                /* (1) : <variable> */
                env = extend_environment(CONS(arg, SCM_NULL),
                                         CONS(map_eval(CDR(obj), env),
                                              SCM_NULL),
                                         SCM_CLOSURE_ENV(tmp));
            } else if (CONSP(arg)) {
                /*
                 * (2) : (<variable1> <variable2> ...)
                 * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
                 *
                 *  - dot list is handled in lookup_frame().
                 */
                env = extend_environment(arg,
                                         map_eval(CDR(obj), env),
                                         SCM_CLOSURE_ENV(tmp));
            } else if (NULLP(arg)) {
                /*
                 * (2') : <variable> is '()
                 */
                env = extend_environment(SCM_NULL,
                                         SCM_NULL,
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
            obj = ScmExp_begin(CDR(SCM_CLOSURE_EXP(tmp)), &env);
            goto eval_loop;

        case ScmContinuation:
            /*
             * Description of ScmContinuation handling
             *
             * (1) eval 1st arg
             * (2) store it to global variable "scm_continuation_thrown_obj"
             * (3) then longjmp
             *
             * PROBLEM : setjmp/longjmp is stack based operation, so we
             * cannot jump from the bottom of the stack to the top of
             * the stack. Is there any efficient way to implement first
             * class continuation? (TODO).
             */
            obj = CADR(obj);
            scm_continuation_thrown_obj = ScmOp_eval(obj, env);
            longjmp(SCM_CONTINUATION_JMPENV(tmp), 1);
            break;

        case ScmEtc:
            SigScm_ErrorObj("eval : invalid application: ", obj);

        default:
            SigScm_ErrorObj("eval : What type of function? ", arg);
        }

    default:
        ret = obj;
        goto eval_done;
    }

eval_done:
    scm_trace_root = frame.prev;
    return ret;
}

ScmObj ScmOp_apply(ScmObj args, ScmObj env)
{
    ScmObj proc  = SCM_NULL;
    ScmObj obj   = SCM_NULL;
    ScmObj rest  = SCM_NULL;
    ScmObj arg0, arg1, arg2, arg3, arg4;
    int tail_flag = 0;

    /* sanity check */
    if CHECK_2_ARGS(args)
        SigScm_Error("apply : Wrong number of arguments\n");
    if (!NULLP(CDDR(args)))
        SigScm_Error("apply : Doesn't support multiarg apply\n");

    /* 1st elem of list is proc */
    proc = CAR(args);

    /* 2nd elem of list is obj */
    obj  = CADR(args);

    /* apply proc */
    switch (SCM_TYPE(proc)) {
    case ScmFunc:
        switch (SCM_FUNC_TYPECODE(proc)) {
        case FUNCTYPE_EVALED_LIST:
            return SCM_FUNC_EXEC_SUBRL(proc,
                                       obj,
                                       env);

        case FUNCTYPE_RAW_LIST_WITH_TAIL_FLAG:
            obj = SCM_FUNC_EXEC_SUBRF(proc, obj, &env, &tail_flag);
            if (tail_flag)
                obj = EVAL(obj, env);
            return obj;

        case FUNCTYPE_0:
            return SCM_FUNC_EXEC_SUBR0(proc);

        case FUNCTYPE_1:
            rest = obj;
            if (!NULLP(SCM_SHIFT_RAW_1(arg0, rest)))
                SigScm_ErrorObj("apply func1 :" SCM_ERRMSG_WRONG_NR_ARG, obj);
            return SCM_FUNC_EXEC_SUBR1(proc, arg0);

        case FUNCTYPE_2:
            rest = obj;
            if (!NULLP(SCM_SHIFT_RAW_2(arg0, arg1, rest)))
                SigScm_ErrorObj("apply func2 :" SCM_ERRMSG_WRONG_NR_ARG, obj);
            return SCM_FUNC_EXEC_SUBR2(proc, arg0, arg1);

        case FUNCTYPE_3:
            rest = obj;
            if (!NULLP(SCM_SHIFT_RAW_3(arg0, arg1, arg2, rest)))
                SigScm_ErrorObj("apply func3 :" SCM_ERRMSG_WRONG_NR_ARG, obj);
            return SCM_FUNC_EXEC_SUBR3(proc, arg0, arg1, arg2);

        case FUNCTYPE_4:
            rest = obj;
            if (!NULLP(SCM_SHIFT_RAW_4(arg0, arg1, arg2, arg3, rest)))
                SigScm_ErrorObj("apply func4 :" SCM_ERRMSG_WRONG_NR_ARG, obj);
            return SCM_FUNC_EXEC_SUBR4(proc, arg0, arg1, arg2, arg3);

        case FUNCTYPE_5:
            rest = obj;
            if (!NULLP(SCM_SHIFT_RAW_5(arg0, arg1, arg2, arg3, arg4, rest)))
                SigScm_ErrorObj("apply func5 :" SCM_ERRMSG_WRONG_NR_ARG, obj);
            return SCM_FUNC_EXEC_SUBR5(proc, arg0, arg1, arg2, arg3, arg4);

        case FUNCTYPE_RAW_LIST:
            return SCM_FUNC_EXEC_SUBRL(proc,
                                       map_eval(obj, env),
                                       env);

        case FUNCTYPE_RAW_LIST_TAIL_REC:
        default:
            SigScm_ErrorObj("apply : invalid application ", proc);
        }

    case ScmClosure:
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
        args = CAR(SCM_CLOSURE_EXP(proc)); /* arg is <formals> */
        if (SYMBOLP(args)) {
            /* (1) : <variable> */
            env = extend_environment(CONS(args, SCM_NULL),
                                     CONS(obj, SCM_NULL),
                                     SCM_CLOSURE_ENV(proc));
        } else if (CONSP(args)) {
            /*
             * (2) : (<variable1> <variable2> ...)
             * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
             *
             *  - dot list is handled in lookup_frame().
             */
            env = extend_environment(args,
                                     obj,
                                     SCM_CLOSURE_ENV(proc));
        } else if (NULLP(args)) {
            /*
             * (2') : <variable> is '()
             */
            env = extend_environment(SCM_NULL,
                                     SCM_NULL,
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
        obj = ScmExp_begin(CDR(SCM_CLOSURE_EXP(proc)), &env);
        return EVAL(obj, env);

    default:
        SigScm_ErrorObj("apply : invalid application ", args);
    }

    /* never reaches here */
    return SCM_NULL;
}

ScmObj symbol_value(ScmObj var, ScmObj env)
{
    ScmObj val = SCM_NULL;

    /* sanity check */
    if (!SYMBOLP(var))
        SigScm_ErrorObj("symbol_value : not symbol : ", var);

    /* first, lookup the environment */
    val = lookup_environment(var, env);
    if (!NULLP(val)) {
        /* variable is found in environment, so returns its value */
        return CAR(val);
    }

    /* next, lookup the special environment for letrec */
    val = lookup_environment(var, scm_letrec_env);
    if (!NULLP(val)) {
        /* variable is found in letrec environment, so returns its value */
        return CAR(val);
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
    ScmObj result  = SCM_NULL;
    ScmObj tail    = SCM_NULL;
    ScmObj newtail = SCM_NULL;

    /* sanity check */
    if (NULLP(args))
        return SCM_NULL;

    /* eval each element of args */
    result  = CONS(EVAL(CAR(args), env), SCM_NULL);
    tail    = result;
    newtail = SCM_NULL;
    for (args = CDR(args); !NULLP(args); args = CDR(args)) {
        newtail = CONS(EVAL(CAR(args), env), SCM_NULL);
        SET_CDR(tail, newtail);
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
    ScmObj ls        = SCM_NULL;
    ScmObj obj       = SCM_NULL;
    ScmObj car       = SCM_NULL;
    ScmObj args      = SCM_NULL;
    ScmObj result    = SCM_NULL;
    ScmObj ret_list  = SCM_NULL;
    ScmObj *ret_tail = NULL;
    int splice_flag  = 0;

    /* local "functions" */
#define qquote_copy_delayed()   (QQUOTE_IS_VERBATIM(ret_list))
#define qquote_force_copy_upto(end) \
    do { \
        ScmObj src = qexpr; \
        ret_tail = &ret_list; \
        while (!EQ(src, end)) { \
            *ret_tail = CONS(CAR(src), SCM_NULL); \
            ret_tail = &CDR(*ret_tail); \
            src = CDR(src); \
        } \
    } while (0)


    QQUOTE_SET_VERBATIM(ret_list); /* default return value */

    if (CONSP(qexpr)) {
        car = CAR(qexpr);
        args = CDR(qexpr);

        if (EQ(car, SCM_UNQUOTE_SPLICING)) {
            if (!IS_LIST_LEN_1(args))
                SigScm_ErrorObj("syntax error: ", qexpr);
            if (--nest == 0)
                return EVAL(CAR(args), env);
        } else if (EQ(car, SCM_QUASIQUOTE)) {
            if (!IS_LIST_LEN_1(args))
                SigScm_ErrorObj("syntax error: ", qexpr);
            if (++nest <= 0)
                SigScm_Error("quasiquote: nesting too deep (circular list?)");
        }
    }

    for (ls = qexpr; CONSP(ls); ls = CDR(ls)) {
        obj = CAR(ls);
        splice_flag = 0;

        if (CONSP(obj)) {
            result = qquote_internal(obj, env, nest);

            if (EQ(CAR(obj), SCM_UNQUOTE_SPLICING) && nest == 1) {
                /* ,@x */
                splice_flag = 1;
            }
        } else if (VECTORP(obj)) {
            /* #(x) */
            result = qquote_vector(obj, env, nest);
        } else if (EQ(obj, SCM_UNQUOTE) && IS_LIST_LEN_1(CDR(ls))) {
            /* we're at the comma in (x . ,y) or qexpr was ,z */
            if (--nest == 0) {
                result = EVAL(CADR(ls), env);
                goto append_last_item;
            }
            QQUOTE_SET_VERBATIM(result);
        } else {
            /* atom */
            QQUOTE_SET_VERBATIM(result);
        }

        if (QQUOTE_IS_VERBATIM(result)) {
            if (!qquote_copy_delayed()) {
                *ret_tail = CONS(obj, SCM_NULL);
                ret_tail = &CDR(*ret_tail);
            }
        } else {
            if (qquote_copy_delayed())
                qquote_force_copy_upto(ls);

            if (splice_flag) {
                *ret_tail = result;
                /* find the new tail (which may be the current pos) */
                while (CONSP(*ret_tail))
                    ret_tail = &CDR(*ret_tail);
                if (!NULLP(*ret_tail))
                    SigScm_ErrorObj("unquote-splicing: bad list: ",
                                    result);
            } else {
                *ret_tail = CONS(result, SCM_NULL);
                ret_tail = &CDR(*ret_tail);
            }
        }
    } /* foreach ls in qexpr */

    /* Handle the leftover of an improper list; if qexpr is a proper
     * list, all the following will be a no-op. */
    if (VECTORP(ls))
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
    ScmObj splices    = SCM_NULL;
    ScmObj expr       = SCM_NULL;
    ScmObj ret        = SCM_NULL;
    ScmObj *copy_buf  = NULL;
    ScmObj result     = SCM_NULL;
    ScmObj splice_len = SCM_NULL;
    int len = SCM_VECTOR_LEN(src);
    int growth = 0;
    int next_splice_index = -1;
    int i = 0;
    int j = 0;

    /* local "functions" */
#define qquote_copy_delayed() (copy_buf == NULL)
#define qquote_is_spliced(o)  \
    (CONSP(o) && EQ(CAR(o), SCM_UNQUOTE_SPLICING))
#define qquote_force_copy_upto(n) \
    do { \
        int k; \
        copy_buf = (ScmObj*)malloc((len + growth) * sizeof(ScmObj)); \
        memcpy(copy_buf, SCM_VECTOR_VEC(src), n*sizeof(ScmObj)); \
        /* wrap it now, or a cont invocation can leak it */ \
        ret = Scm_NewVector(copy_buf, len + growth); \
        /* fill with something the garbage collector recognizes */ \
        for (k=n; k < len + growth; k++) \
            copy_buf[k] = SCM_NULL; \
    } while(0)

    QQUOTE_SET_VERBATIM(ret);
    copy_buf = NULL;

    if (nest == 1) {
        /* Evaluate all the splices first, in reverse order, and store
         * them in a list ((ls . index) (ls . index)...). */
        for (i = len - 1; i >= 0; i--) {
            expr = SCM_VECTOR_CREF(src, i);
            if (qquote_is_spliced(expr)) {
                if (!IS_LIST_LEN_1(CDR(expr)))
                    SigScm_ErrorObj("syntax error: ", expr);

                result = EVAL(CADR(expr), env);

                splice_len = ScmOp_length(result);
                if (SCM_INT_VALUE(splice_len) < 0)
                    SigScm_Error("unquote-splicing: bad list");

                growth += SCM_INT_VALUE(splice_len) - 1;
                splices = CONS(CONS(result, Scm_NewInt(i)),
                               splices);
            }
        }
        if (!NULLP(splices)) {
            next_splice_index = SCM_INT_VALUE(CDAR(splices));
            qquote_force_copy_upto(0);
        }
    }

    for (i = j = 0; i < len; i++) {
        /* j will be the index for copy_buf */
        if (i == next_splice_index) {
            /* spliced */
            for (expr=CAAR(splices); !NULLP(expr); expr=CDR(expr))
                copy_buf[j++] = CAR(expr);
            splices = CDR(splices);

            if (NULLP(splices))
                next_splice_index = -1;
            else
                next_splice_index = SCM_INT_VALUE(CDAR(splices));
            /* continue; */
        } else {
            expr = SCM_VECTOR_CREF(src, i);
            if (CONSP(expr))
                result = qquote_internal(expr, env, nest);
            else if (VECTORP(expr))
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
ScmObj ScmOp_quote(ScmObj arglist, ScmObj env)
{
    if (!CONSP(arglist) || !NULLP(CDR(arglist)))
        SigScm_ErrorObj("quote: bad argument list: ", arglist);
    return CAR(arglist);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj ScmExp_lambda(ScmObj exp, ScmObj env)
{
    if CHECK_2_ARGS(exp)
        SigScm_ErrorObj("lambda : too few argument ", exp);

    return Scm_NewClosure(exp, env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj ScmExp_if(ScmObj exp, ScmObj *envp)
{
    ScmObj env       = *envp;
    ScmObj pred      = SCM_NULL;
    ScmObj false_exp = SCM_NULL;

    /* sanity check */
    if (NULLP(exp) || NULLP(CDR(exp)))
        SigScm_ErrorObj("if : syntax error : ", exp);

    /* eval predicates */
    pred = EVAL(CAR(exp), env);

    /* if pred is true value */
    if (NFALSEP(pred)) {
        /* doesn't evaluate now for tail-recursion. */
        return CADR(exp);
    }

    /* if pred is SCM_FALSE */
    false_exp = CDDR(exp);
    if (NULLP(false_exp))
        return SCM_UNDEF;

    /* doesn't evaluate now for tail-recursion. */
    return CAR(false_exp);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignment
===========================================================================*/
ScmObj ScmExp_set(ScmObj arg, ScmObj env)
{
    ScmObj sym = CAR(arg);
    ScmObj val = CADR(arg);
    ScmObj ret = SCM_NULL;
    ScmObj tmp = SCM_NULL;

    ret = EVAL(val, env);
    tmp = lookup_environment(sym, env);
    if (NULLP(tmp)) {
        if (!SYMBOLP(sym))
            SigScm_ErrorObj("set! : symbol required but got ", sym);
        /* Not found in the environment
           If symbol is not bound, error occurs */
        if (!SCM_SYMBOL_BOUNDP(sym))
            SigScm_ErrorObj("set! : unbound variable ", sym);

        SCM_SYMBOL_SET_VCELL(sym, ret);
    } else {
        /* found in the environment*/
        SET_CAR(tmp, ret);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return ret;
#endif
}


/*=======================================
  R5RS : 4.2 Derived expression types
=======================================*/
/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.1 Conditionals
===========================================================================*/
ScmObj ScmExp_cond(ScmObj arg, ScmObj *envp)
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
    ScmObj clause = SCM_NULL;
    ScmObj test   = SCM_NULL;
    ScmObj exps   = SCM_NULL;
    ScmObj proc   = SCM_NULL;

    /* looping in each clause */
    for (; !NULLP(arg); arg = CDR(arg)) {
        clause = CAR(arg);
        if (!CONSP(clause))
            SigScm_ErrorObj("cond : bad clause: ", clause);

        test = CAR(clause);
        exps = CDR(clause);

        /* evaluate test */
        test = EVAL(test, env);

        /* check the result */
        if (NFALSEP(test)) {
            /*
             * if the selected <clause> contains only the <test> and no <expression>s,
             * then the value of the <test> is returned as the result.
             */
            if (NULLP(exps))
                return test;

            /*
             * If the selected <clause> uses the => alternate form, then the <expression>
             * is evaluated. Its value must be a procedure that accepts one argument;
             * this procedure is then called on the value of the <test> and the value
             * returned by this procedure is returned by the cond expression.
             */
            if (EQ(Scm_Intern("=>"), CAR(exps))) {
                proc = EVAL(CADR(exps), env);
                if (FALSEP(ScmOp_procedurep(proc)))
                    SigScm_ErrorObj("cond : the value of exp after => must be the procedure but got ", proc);

                return ScmOp_apply(SCM_LIST_2(proc,
                                              CONS(test, SCM_NULL)),
                                   env);
            }

            return ScmExp_begin(exps, &env);
        }
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_case(ScmObj arg, ScmObj *envp)
{
    ScmObj env    = *envp;
    ScmObj key    = EVAL(CAR(arg), env);
    ScmObj clause = SCM_NULL;
    ScmObj data   = SCM_NULL;
    ScmObj exps   = SCM_NULL;

    /* looping in each clause */
    for (arg = CDR(arg); !NULLP(arg); arg = CDR(arg)) {
        clause = CAR(arg);
        data   = CAR(clause);
        exps   = CDR(clause);
        if (NULLP(clause) || NULLP(data) || NULLP(exps))
            SigScm_Error("case : syntax error\n");

        /* check "else" symbol */
        if (NULLP(CDR(arg)) && !CONSP(data) && NFALSEP(SCM_SYMBOL_VCELL(data)))
            return ScmExp_begin(exps, &env);

        /* evaluate data and compare to key by eqv? */
        for (; !NULLP(data); data = CDR(data)) {
            if (NFALSEP(ScmOp_eqvp(CAR(data), key))) {
                return ScmExp_begin(exps, &env);
            }
        }
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_and(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NULL;

    /* sanity check */
    if (NULLP(arg))
        return SCM_TRUE;
    if (FALSEP(ScmOp_listp(arg)))
        SigScm_ErrorObj("and : list required but got ", arg);

    /* check recursively */
    for (; !NULLP(arg); arg = CDR(arg)) {
        obj = CAR(arg);

        /* return last item */
        if (NULLP(CDR(arg))) {
            /* set tail_flag */
            (*tail_flag) = 1;

            return obj;
        }

        /* evaluate obj */
        obj = EVAL(obj, env);
        if (FALSEP(obj)) {
            /* set tail_flag */
            (*tail_flag) = 0;

            return SCM_FALSE;
        }
    }

    return SCM_NULL;
}

ScmObj ScmExp_or(ScmObj arg, ScmObj *envp, int *tail_flag)
{
    ScmObj env = *envp;
    ScmObj obj = SCM_NULL;

    /* sanity check */
    if (NULLP(arg))
        return SCM_FALSE;
    if (FALSEP(ScmOp_listp(arg)))
        SigScm_ErrorObj("or : list required but got ", arg);

    /* check recursively */
    for (; !NULLP(arg); arg = CDR(arg)) {
        obj = CAR(arg);

        /* return last item */
        if (NULLP(CDR(arg))) {
            /* set tail_flag */
            (*tail_flag) = 1;

            return obj;
        }

        obj = EVAL(obj, env);
        if (NFALSEP(obj)) {
            /* set tail_flag */
            (*tail_flag) = 0;

            return obj;
        }

    }

    return SCM_NULL;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
ScmObj ScmExp_let(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NULL;
    ScmObj body     = SCM_NULL;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj binding  = SCM_NULL;

    /* sanity check */
    if CHECK_2_ARGS(arg)
        SigScm_Error("let : syntax error\n");

    /* guess whether syntax is "Named let" */
    if (SYMBOLP(CAR(arg)))
        goto named_let;

    /* get bindings and body */
    bindings = CAR(arg);
    body     = CDR(arg);

    /*========================================================================
      (let <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (CONSP(bindings) || NULLP(bindings)) {
        for (; !NULLP(bindings); bindings = CDR(bindings)) {
            binding = CAR(bindings);

#if SCM_STRICT_ARGCHECK
            if (NULLP(binding) || NULLP(CDR(binding)))
                SigScm_ErrorObj("let : invalid binding form : ", binding);
#else
            if (NULLP(CDR(binding)))
                SET_CDR(binding, CONS(SCM_NULL, SCM_NULL));
#endif

            vars = CONS(CAR(binding), vars);
            vals = CONS(EVAL(CADR(binding), env), vals);
        }

        /* create new environment for */
        env = extend_environment(vars, vals, env);
        *envp = env;

        return ScmExp_begin(body, &env);
    }

    return ScmExp_begin(body, &env);

named_let:
    /*========================================================================
      (let <variable> <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    bindings = CADR(arg);
    body     = CDDR(arg);
    for (; !NULLP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
        vars = CONS(CAR(binding), vars);
        vals = CONS(CADR(binding), vals);
    }

    vars = ScmOp_reverse(vars);
    vals = ScmOp_reverse(vals);

    /* (define (<variable> <variable1> <variable2> ...>) <body>) */
    ScmExp_define(CONS(CONS(CAR(arg),
                            vars),
                       body),
                  env);

    /* (func <init1> <init2> ...) */
    return CONS(CAR(arg), vals);
}

ScmObj ScmExp_let_star(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NULL;
    ScmObj body     = SCM_NULL;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj binding  = SCM_NULL;

    /* sanity check */
    if CHECK_2_ARGS(arg)
        SigScm_Error("let* : syntax error\n");

    /* get bindings and body */
    bindings = CAR(arg);
    body     = CDR(arg);

    /*========================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (CONSP(bindings)) {
        for (; !NULLP(bindings); bindings = CDR(bindings)) {
            binding = CAR(bindings);

#if SCM_STRICT_ARGCHECK
            if (NULLP(binding) || NULLP(CDR(binding)))
                SigScm_ErrorObj("let* : invalid binding form : ", binding);
#else
            if (NULLP(CDR(binding)))
                SET_CDR(binding, CONS(SCM_NULL, SCM_NULL));
#endif

            vars = CONS(CAR(binding), SCM_NULL);
            vals = CONS(EVAL(CADR(binding), env), SCM_NULL);

            /* add env to each time!*/
            env = extend_environment(vars, vals, env);
        }
        /* set new env */
        *envp = env;
        /* evaluate */
        return ScmExp_begin(body, &env);
    } else if (NULLP(bindings)) {
        /* extend null environment */
        env = extend_environment(SCM_NULL,
                                 SCM_NULL,
                                 env);

        /* set new env */
        *envp = env;
        /* evaluate */
        return ScmExp_begin(body, &env);
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_letrec(ScmObj arg, ScmObj *envp)
{
    ScmObj env      = *envp;
    ScmObj bindings = SCM_NULL;
    ScmObj body     = SCM_NULL;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj binding  = SCM_NULL;
    ScmObj var      = SCM_NULL;
    ScmObj val      = SCM_NULL;
    ScmObj frame    = SCM_NULL;

    /* sanity check */
    if (NULLP(arg) || NULLP(CDR(arg)))
        SigScm_Error("letrec : syntax error\n");

    /* get bindings and body */
    bindings = CAR(arg);
    body     = CDR(arg);

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (CONSP(bindings) || NULLP(bindings)) {
        for (; !NULLP(bindings); bindings = CDR(bindings)) {
            binding = CAR(bindings);

#if SCM_STRICT_ARGCHECK
            if (NULLP(binding) || NULLP(CDR(binding)))
                SigScm_ErrorObj("letrec : invalid binding form : ", binding);
#else
            if (NULLP(CDR(binding)))
                SET_CDR(binding, CONS(SCM_NULL, SCM_NULL));
#endif

            var = CAR(binding);
            val = CADR(binding);

            /* construct vars and vals list */
            vars = CONS(var, vars);
            vals = CONS(val, vals);
        }

        /* construct new frame for scm_letrec_env */
        frame = CONS(vars, vals);
        scm_letrec_env = CONS(frame, scm_letrec_env);

        /* extend environment by scm_letrec_env */
        env = extend_environment(CAR(frame), CDR(frame), env);

        /* ok, vars of letrec is extended to env */
        scm_letrec_env = SCM_NULL;

        /* set new env */
        *envp = env;

        /* evaluate vals */
        for (; !NULLP(vals); vals = CDR(vals)) {
            SET_CAR(vals, EVAL(CAR(vals), env));
        }

        /* evaluate body */
        return ScmExp_begin(body, &env);
    }

    SigScm_Error("letrec : syntax error\n");
    return SCM_UNDEF;
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj ScmExp_begin(ScmObj arg, ScmObj *envp)
{
    ScmObj env = *envp;
    ScmObj exp = SCM_NULL;

    /* sanity check */
    if (NULLP(arg))
        return SCM_UNDEF;
    if (FALSEP(ScmOp_listp(arg)))
        SigScm_ErrorObj("begin : list required but got ", arg);

    /* eval recursively */
    for (; !NULLP(arg); arg = CDR(arg)) {
        exp = CAR(arg);

        /* return last expression's result */
        if (EQ(CDR(arg), SCM_NULL)) {
            /* doesn't evaluate exp now for tail-recursion. */
            return exp;
        }

        /* evaluate exp */
        EVAL(exp, env);

        /* set new env */
        *envp = env;
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
    ScmObj bindings   = CAR(arg);
    ScmObj vars       = SCM_NULL;
    ScmObj vals       = SCM_NULL;
    ScmObj steps      = SCM_NULL;
    ScmObj binding    = SCM_NULL;
    ScmObj step       = SCM_NULL;
    ScmObj testframe  = SCM_NULL;
    ScmObj test       = SCM_NULL;
    ScmObj expression = SCM_NULL;
    ScmObj commands   = SCM_NULL;
    ScmObj tmp_vars   = SCM_NULL;
    ScmObj tmp_steps  = SCM_NULL;
    ScmObj obj        = SCM_NULL;

    /* sanity check */
    if (SCM_INT_VALUE(ScmOp_length(arg)) < 2)
        SigScm_Error("do : syntax error\n");

    /* construct Environment and steps */
    for (; !NULLP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
        vars = CONS(CAR(binding), vars);
        vals = CONS(EVAL(CADR(binding), env), vals);

        /* append <step> to steps */
        step = CDDR(binding);
        if (NULLP(step))
            steps = CONS(CAR(binding), steps);
        else
            steps = CONS(CAR(step), steps);
    }

    /* now extend environment */
    env = extend_environment(vars, vals, env);

    /* construct test */
    testframe  = CADR(arg);
    test       = CAR(testframe);
    expression = CDR(testframe);

    /* construct commands */
    commands = CDDR(arg);

    /* now excution phase! */
    while (FALSEP(EVAL(test, env))) {
        /* execute commands */
        EVAL(ScmExp_begin(commands, &env), env);

        /*
         * Notice
         *
         * the result of the execution of <step>s must not depend on each other's
         * results. each excution must be done independently. So, we store the
         * results to the "vals" variable and set it in hand.
         */
        vals = SCM_NULL;
        for (tmp_steps = steps;
             !NULLP(tmp_steps);
             tmp_steps = CDR(tmp_steps))
        {
            vals = CONS(EVAL(CAR(tmp_steps), env), vals);
        }
        vals = ScmOp_reverse(vals);

        /* set it */
        for (tmp_vars = vars;
             !NULLP(tmp_vars) && !NULLP(vals);
             tmp_vars = CDR(tmp_vars), vals = CDR(vals))
        {
            obj = lookup_environment(CAR(tmp_vars), env);
            if (!NULLP(obj)) {
                SET_CAR(obj, CAR(vals));
            } else {
                SigScm_Error("do : broken env\n");
            }
        }
    }

    /* set new env */
    *envp = env;

    return ScmExp_begin(expression, &env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
ScmObj ScmOp_delay(ScmObj arg, ScmObj env)
{
    if (SCM_INT_VALUE(ScmOp_length(arg)) != 1)
        SigScm_Error("delay : Wrong number of arguments\n");

    /* closure exp = ( () CAR(arg) ) */
    return Scm_NewClosure(SCM_LIST_2(SCM_NULL, CAR(arg)), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/
ScmObj ScmOp_quasiquote(ScmObj obj, ScmObj env)
{
    ScmObj ret;
    if (!IS_LIST_LEN_1(obj))
        SigScm_ErrorObj("quasiquote: bad argument list: ", obj);
    obj = CAR(obj);
    ret = qquote_internal(obj, env, 1);

    if (QQUOTE_IS_VERBATIM(ret))
        return obj;
    return ret;
}

ScmObj ScmOp_unquote(ScmObj obj, ScmObj env)
{
    if (!CONSP(obj) || !NULLP(CDR(obj)))
        SigScm_ErrorObj("unquote: bad argument list: ", obj);
    SigScm_Error("unquote outside quasiquote");
    return SCM_NULL;
}

ScmObj ScmOp_unquote_splicing(ScmObj obj, ScmObj env)
{
    if (!CONSP(obj) || !NULLP(CDR(obj)))
        SigScm_ErrorObj("unquote-splicing: bad argument list: ", obj);
    SigScm_Error("unquote-splicing outside quasiquote");
    return SCM_NULL;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
ScmObj ScmExp_define(ScmObj arg, ScmObj env)
{
    ScmObj var     = CAR(arg);
    ScmObj body    = CADR(arg);
    ScmObj val     = SCM_NULL;
    ScmObj formals = SCM_NULL;

    /* sanity check */
    if (NULLP(var))
        SigScm_ErrorObj("define : syntax error ", arg);

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SYMBOLP(var)) {
        if (NULLP(env)) {
            /* given NIL environment */
            SCM_SYMBOL_SET_VCELL(var, EVAL(body, env));
        } else {
            /* add val to the environment */
            env = add_environment(var, EVAL(body, env), env);
        }

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
    if (CONSP(var)) {
        val     = CAR(var);
        formals = CDR(var);
        body    = CDR(arg);

        /* (val (lambda formals body))  */
        arg = SCM_LIST_2(val,
                         ScmExp_lambda(CONS(formals, body), env));

        return ScmExp_define(arg, env);
    }

    SigScm_ErrorObj("define : syntax error ", arg);
    return SCM_NULL;
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
ScmObj ScmOp_scheme_report_environment(ScmObj version)
{
    /* FIXME: check arg, warn incompatibility */
    return SCM_NULL;
}

ScmObj ScmOp_null_environment(ScmObj version)
{
    /* FIXME: check arg, warn incompatibility */
    return SCM_NULL;
}

ScmObj ScmOp_interaction_environment(void)
{
    return SCM_NULL;
}
