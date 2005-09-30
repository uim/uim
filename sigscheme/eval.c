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
 *     - Frame = (cons (var1 var2 var3 ...)
 *                     (val1 val2 val3 ...))
 *     - Env   = (Frame1 Frame2 Frame3 ...)
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
#define SCM_ERRMSG_NON_R5RS_ENV " the environment is not conformed to R5RS"

/*=======================================
  Variable Declarations
=======================================*/
ScmObj scm_continuation_thrown_obj = NULL; /* for storing continuation return object */

struct trace_frame *scm_trace_root = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env,
                     int suppress_eval);
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state);
static ScmObj call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
                   int suppress_eval);
static ScmObj map_eval(ScmObj args, ScmObj env);
static ScmObj qquote_internal(ScmObj expr, ScmObj env, int nest);
static ScmObj qquote_vector(ScmObj vec, ScmObj env, int nest);

/*=======================================
  Function Implementations
=======================================*/
/**
 * Add a frame to an env
 *
 * @param vars Symbol list as variable names. It accepts dot list to handle
 *             function arguments directly.
 * @param vals Arbitrary Scheme object list as values. Side effect:
 *             destructively modifyies the vals when vars is a dot list.
 * @see ScmOp_eval()
 */
ScmObj extend_environment(ScmObj vars, ScmObj vals, ScmObj env)
{
    ScmObj frame     = SCM_NULL;
    ScmObj rest_vars, rest_vals;

    /* sanity check & dot list handling */
    for (rest_vars = vars, rest_vals = vals;
         !NULLP(rest_vars);
         rest_vars = CDR(rest_vars), rest_vals = CDR(rest_vals))
    {
        if (!CONSP(rest_vars) || !SYMBOLP(CAR(rest_vars)))
            SigScm_ErrorObj("broken environment handling : ", rest_vars);

        /* dot list appeared: fold the rest values into a variable */
        if (SYMBOLP(CDR(rest_vars))) {
            SET_CDR(rest_vals, LIST_1(CDR(rest_vals)));
            break;
        }
    }

    /* create new frame */
    frame = CONS(vars, vals);

    /* add to env */
    if (NULLP(env))
        env = CONS(frame, SCM_NULL);
    else if (CONSP(env))
        env = CONS(frame, env);
    else
        SigScm_Error("broken environment.");

    return env;
}

/** Add a binding to newest frame of an env */
ScmObj add_environment(ScmObj var, ScmObj val, ScmObj env)
{
    ScmObj newest_frame;
    ScmObj new_vars, new_vals;

    /* sanity check */
    if (!SYMBOLP(var))
        SigScm_ErrorObj("broken environment handling : ", var);

    /* add (var, val) pair to the newest frame in env */
    if (NULLP(env)) {
        newest_frame = CONS(CONS(var, SCM_NULL),
                            CONS(val, SCM_NULL));
        env = CONS(newest_frame, SCM_NULL);
    } else if (CONSP(env)) {
        newest_frame = CAR(env);
        new_vars = CONS(var, CAR(newest_frame));
        new_vals = CONS(val, CDR(newest_frame));

        SET_CAR(env, CONS(new_vars, new_vals));
    } else {
        SigScm_ErrorObj("broken environent : ", env);
    }
    return env;
}

/**
 * Lookup a variable of an env
 *
 * @return a variable which represented as (val . rest-vals-in-frame).  val is
 *         the value of var. Since the result is the part of the frame, caller
 *         can modify the variable by (set-car! the-list new-val).
 *
 * @todo describe more precicely
 */
ScmObj lookup_environment(ScmObj var, ScmObj env)
{
    ScmObj frame = SCM_NULL;
    ScmObj val   = SCM_NULL;

    /* sanity check */
    if (NULLP(env))
        return SCM_NULL;
    if (!CONSP(env))
        SigScm_ErrorObj("broken environent : ", env);

    /* lookup in frames */
    for (; !NULLP(env); env = CDR(env)) {
        frame = CAR(env);
        val   = lookup_frame(var, frame);
        if (!NULLP(val))
            return val;
    }

    return SCM_NULL;
}

/** Lookup a variable of a frame */
ScmObj lookup_frame(ScmObj var, ScmObj frame)
{
    ScmObj vals = SCM_NULL;
    ScmObj vars = SCM_NULL;

    /* sanity check */
    if (NULLP(frame))
        return SCM_NULL;
    else if (!CONSP(frame))
        SigScm_ErrorObj("broken frame : ", frame);

    /* lookup in frame */
    for (vars = CAR(frame), vals = CDR(frame);
         !NULLP(vars);
         vars = CDR(vars), vals = CDR(vals))
    {
        if (SYMBOLP(vars)) {
            /* handle dot list */
            return (EQ(vars, var)) ? vals : SCM_NULL;
        } else {
            /* normal binding */
            if (EQ(CAR(vars), var))
                return vals;
        }
    }

    return SCM_NULL;
}

/* Wrapper for call().  Just like ScmOp_apply(), except ARGS is used
 * as given---nothing special is done about the last item in the
 * list. */
ScmObj Scm_call(ScmObj proc, ScmObj args)
{
    ScmEvalState state;
    ScmObj ret;

    /* We don't need a nonempty environemnt, because this function
     * will never be called directly from Scheme code.  If PROC is a
     * closure, it'll have its own environment, if it's a syntax, it's
     * an error, and if it's a C procedure, it doesn't have any free
     * variables at the Scheme level. */
    state.env       = SCM_NULL;
    state.ret_type  = SCM_RETTYPE_AS_IS;

    ret = call(proc, args, &state, 1);
    if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
        ret = EVAL(ret, state.env);
    return ret;
}

/* ARGS should NOT be evaluated yet. */
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env, int suppress_eval)
{
    ScmObj left;
    ScmObj right;
    enum ScmReductionState state;

    state = SCM_REDUCE_0;
    if (NULLP(args))
        return (*func)(SCM_INVALID, SCM_INVALID, &state);

    state = SCM_REDUCE_1;
    SCM_SHIFT_RAW(left, args);
    if (!suppress_eval)
        left = EVAL(left, env);
    if (NULLP(args))
        return (*func)(left, left, &state);

    /* Reduce upto all but the last argument. */
    state = SCM_REDUCE_PARTWAY;
    while (SCM_SHIFT_RAW(right, args), !NULLP(args)) {
        if (!suppress_eval)
            right = EVAL(right, env);
        left = (*func)(left, right, &state);
        if (state == SCM_REDUCE_STOP)
            return left;
    }

    /* Make the last call. */
    state = SCM_REDUCE_LAST;
    if (!suppress_eval)
        right = EVAL(right, env);
    return (*func)(left, right, &state);
}

/* ARGS should already be evaluated. */
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj formals;
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
    formals = CAR(SCM_CLOSURE_EXP(proc));

    if (SYMBOLP(formals)) {
        /* (1) : <variable> */
        eval_state->env = extend_environment(LIST_1(formals),
                                             LIST_1(args),
                                             SCM_CLOSURE_ENV(proc));
    } else if (CONSP(formals)) {
        /*
         * (2) : (<variable1> <variable2> ...)
         * (3) : (<variable1> <variable2> ... <variable n-1> . <variable n>)
         *
         *  - dot list is handled in lookup_frame().
         */
        eval_state->env = extend_environment(formals,
                                             args,
                                             SCM_CLOSURE_ENV(proc));
    } else if (NULLP(formals)) {
        /*
         * (2') : <variable> is '()
         */
        eval_state->env
            = extend_environment(SCM_NULL,
                                 SCM_NULL,
                                 SCM_CLOSURE_ENV(proc));
    } else {
        SigScm_ErrorObj("lambda : bad formals list: ", formals);
    }

    eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
    return ScmExp_begin(CDR(SCM_CLOSURE_EXP(proc)), eval_state);
}

/**
 * @param proc The procedure or syntax to call.
 *
 * @param args The argument list.
 *
 * @param eval_state The calling evaluator's state.
 *
 * @param suppress_eval PROC and ARGS are assumed to have already gone
 * through all necessary evaluations if this flag is nonzero.
 */
static ScmObj call(ScmObj proc, ScmObj args, ScmEvalState *eval_state, int suppress_eval)
{
    ScmObj env                = eval_state->env;
    ScmObj (*func)()          = NULL;
    enum ScmFuncTypeCode type = -1;
    int mand_count            = 0; /* Number of mandatory args. */

    void* argbuf[SCM_FUNCTYPE_MAND_MAX+2] = {0}; /* The +2 is for rest and env/eval_state. */
    int i = 0;     /* Number of arguments already stored in argbuf. */

    if (!suppress_eval)
        proc = EVAL(proc, env);

    switch (SCM_TYPE(proc)) {
    case ScmClosure:
        return call_closure(proc,
                            suppress_eval ? args : map_eval(args, env),
                            eval_state);
    case ScmContinuation:
        if (NULLP(args)) {
            SigScm_Error("Continuation invocation lacks an argument.");
        }
        scm_continuation_thrown_obj = EVAL(CAR(args), env);
        longjmp(SCM_CONTINUATION_JMPENV(proc), 1);
        return SCM_INVALID;

    case ScmFunc:
        type = SCM_FUNC_TYPECODE(proc);
        break;
    default:
        SigScm_ErrorObj("bad operator: ", proc);
    }

    /* We have a C function. */

    func = SCM_FUNC_CFUNC(proc);

    if (type == SCM_REDUCTION_OPERATOR)
        return reduce(func, args, env, suppress_eval);

    /* Suppress argument evaluation for syntaxes. */
    if (suppress_eval) {
        if (type & SCM_FUNCTYPE_SYNTAX)
            SigScm_ErrorObj("can't apply/map a syntax: ", proc);
    } else {
        suppress_eval = type & SCM_FUNCTYPE_SYNTAX;
    }

    /* Collect mandatory arguments. */
    mand_count = type & SCM_FUNCTYPE_MAND_MASK;
    if (mand_count > SCM_FUNCTYPE_MAND_MAX)
        SigScm_Error("Corrupted function: typecode=0x%x", type);
    for (i=0; i < mand_count; i++) {
        if (NULLP(args))
            SigScm_Error("%d or more argument(s) required but got only %d",
                         mand_count, i);
        SCM_SHIFT_RAW(argbuf[i], args);
        if (!suppress_eval)
            argbuf[i] = EVAL(argbuf[i], env);
    }

    if (type & SCM_FUNCTYPE_VARIADIC) {
        if (!suppress_eval)
            args = map_eval(args, env);
        argbuf[i++] = args;
    }
#if SCM_STRICT_ARGCHECK
    else if (!NULLP(args)) {
        SigScm_ErrorObj("superfluous arguments: ", args);
    }
#endif

    if (type & SCM_FUNCTYPE_TAIL_REC) {
        eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
        argbuf[i++] = eval_state;
    } else {
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        if (type & SCM_FUNCTYPE_SYNTAX)
            argbuf[i++] = env;
    }

    switch (i) {
    case 0:
        return (*func)();
    case 1:
        return (*func)(argbuf[0]);
    case 2:
        return (*func)(argbuf[0], argbuf[1]);
#if SCM_FUNCTYPE_MAND_MAX >= 1
    case 3:
        return (*func)(argbuf[0], argbuf[1], argbuf[2]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 2
    case 4:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 3
    case 5:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 4
    case 6:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5]);
#endif
#if SCM_FUNCTYPE_MAND_MAX >= 5
    case 7:
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5], argbuf[6]);
#endif
    default:
        SigScm_Error("Corrupted function: typecode=0x%x", type);
    }
    return SCM_INVALID;
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj ScmOp_eval(ScmObj obj, ScmObj env)
{
    ScmObj ret  = SCM_NULL;
    ScmEvalState state = {0};

#if SCM_DEBUG
    struct trace_frame frame;
    frame.prev = scm_trace_root;
    frame.obj  = obj;
    frame.env  = env;
    scm_trace_root = &frame;
#endif

    state.env = env;
    state.ret_type = SCM_RETTYPE_AS_IS;

eval_loop:
#if SCM_STRICT_R5RS
    /* () is allowed by default for efficiency */
    if (NULLP(obj))
        SigScm_Error("() is not a valid R5RS form. use '() instead");
#endif
    switch (SCM_TYPE(obj)) {
    case ScmSymbol:
        ret = symbol_value(obj, state.env);
        break;

    case ScmCons:
        obj = call(CAR(obj), CDR(obj), &state, 0);
        if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
            goto eval_loop;
        /* FALLTHROUGH */
    default:
        ret = obj;
        break;
    }

#if SCM_DEBUG
    scm_trace_root = frame.prev;
#endif
    return ret;
}

ScmObj ScmOp_apply(ScmObj proc, ScmObj arg0, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj args = SCM_INVALID;
    ScmObj tail = SCM_INVALID;
    ScmObj last = SCM_INVALID;
    ScmObj lst  = SCM_INVALID;

    if (NULLP(rest)) {
        args = last = arg0;
    } else {
        /* More than one argument given. */
        tail = args = LIST_1(arg0);
        for (lst=rest; CONSP(CDR(lst)); lst = CDR(lst)) {
            SET_CDR(tail, LIST_1(CAR(lst)));
            tail = CDR(tail);
        }
        last = CAR(lst);
        SET_CDR(tail, last); /* The last one is spliced. */
        if (!NULLP(CDR(lst)))
            SigScm_ErrorObj("apply : improper argument list: ", CONS(arg0, rest));
    }

    if (FALSEP(ScmOp_listp(last)))
        SigScm_ErrorObj("apply : list required but got: ", last);

    /* The last argument inhibits argument re-evaluation. */
    return call(proc, args, eval_state, 1);
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

    /* finally, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND)) {
        SigScm_ErrorObj("symbol_value : unbound variable ", var);
    }

    return val;
}

static ScmObj map_eval(ScmObj args, ScmObj env)
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
    ScmObj ret_lst   = SCM_NULL;
    ScmObj *ret_tail = NULL;
    int splice_flag  = 0;

    /* local "functions" */
#define qquote_copy_delayed()   (QQUOTE_IS_VERBATIM(ret_lst))
#define qquote_force_copy_upto(end) \
    do { \
        ScmObj src = qexpr; \
        ret_tail = &ret_lst; \
        while (!EQ(src, end)) { \
            *ret_tail = CONS(CAR(src), SCM_NULL); \
            ret_tail = &CDR(*ret_tail); \
            src = CDR(src); \
        } \
    } while (0)


    QQUOTE_SET_VERBATIM(ret_lst); /* default return value */

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

    return ret_lst;
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
/* FIXME: rename to ScmExp_quote since quote is a syntax */
ScmObj ScmOp_quote(ScmObj datum, ScmObj env)
{
    return datum;
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
ScmObj ScmExp_lambda(ScmObj args, ScmObj env)
{
    if (CHECK_2_ARGS(args))
        SigScm_ErrorObj("lambda : bad form : ", args);

    return Scm_NewClosure(args, env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
ScmObj ScmExp_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj env  = eval_state->env;

    /*========================================================================
      (if <test> <consequent>)
      (if <test> <consequent> <alternate>)
    ========================================================================*/

    if (NFALSEP(EVAL(test, env)))
        return conseq;
    else
        return NULLP(rest) ? SCM_UNDEF : CAR(rest);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignment
===========================================================================*/
ScmObj ScmExp_set(ScmObj sym, ScmObj exp, ScmObj env)
{
    ScmObj evaled        = SCM_FALSE;
    ScmObj locally_bound = SCM_NULL;

    evaled = EVAL(exp, env);
    locally_bound = lookup_environment(sym, env);
    if (NULLP(locally_bound)) {
        if (!SYMBOLP(sym))
            SigScm_ErrorObj("set! : symbol required but got ", sym);
        /* Not found in the environment
           If symbol is not bound, error occurs */
        if (!SCM_SYMBOL_BOUNDP(sym))
            SigScm_ErrorObj("set! : unbound variable ", sym);

        SCM_SYMBOL_SET_VCELL(sym, evaled);
    } else {
        /* found in the environment*/
        SET_CAR(locally_bound, evaled);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return evaled;
#endif
}


/*=======================================
  R5RS : 4.2 Derived expression types
=======================================*/
/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.1 Conditionals
===========================================================================*/
/*
 * FIXME: following else handlings
 * - depending on its own true value
 * - can appeared in other than last clause
 */
ScmObj ScmExp_cond(ScmObj args, ScmEvalState *eval_state)
{
    /*
     * (cond <clause1> <clause2> ...)
     *
     * <clause> should be the form:
     *     (<test> <expression1> <expression2> ...)
     *
     * <clause> may be of the form
     *     (<test> => <expression>)
     *
     * last <clause> may be of the form
     *     (else <expression1> <expression2> ...)
     */
    ScmObj env    = eval_state->env;
    ScmObj clause = SCM_NULL;
    ScmObj test   = SCM_NULL;
    ScmObj exps   = SCM_NULL;
    ScmObj proc   = SCM_NULL;

    /* looping in each clause */
    for (; !NULLP(args); args = CDR(args)) {
        clause = CAR(args);
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
            /* FIXME: remove expensive Scm_Intern() */
            if (EQ(Scm_Intern("=>"), CAR(exps))) {
                proc = EVAL(CADR(exps), env);
                if (FALSEP(ScmOp_procedurep(proc)))
                    SigScm_ErrorObj("cond : the value of exp after => must be the procedure but got ", proc);

                return Scm_call(proc, LIST_1(test));
            }

            return ScmExp_begin(exps, eval_state);
        }
    }

    return SCM_UNDEF;
}

/* FIXME: argument extraction */
ScmObj ScmExp_case(ScmObj key, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env    = eval_state->env;
    ScmObj clause = SCM_NULL;
    ScmObj data   = SCM_NULL;
    ScmObj exps   = SCM_NULL;

    /* get key */
    key = EVAL(key, env);

    /* looping in each clause */
    for (; !NULLP(args); args = CDR(args)) {
        clause = CAR(args);
        data   = CAR(clause);
        exps   = CDR(clause);
        if (NULLP(clause) || NULLP(data) || NULLP(exps))
            SigScm_Error("case : syntax error");

        /* check "else" symbol */
        if (NULLP(CDR(args)) && !CONSP(data) && NFALSEP(SCM_SYMBOL_VCELL(data)))
            return ScmExp_begin(exps, eval_state);

        /* evaluate data and compare to key by eqv? */
        for (; !NULLP(data); data = CDR(data)) {
            if (NFALSEP(ScmOp_eqvp(CAR(data), key))) {
                return ScmExp_begin(exps, eval_state);
            }
        }
    }

    return SCM_UNDEF;
}

ScmObj ScmExp_and(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env = eval_state->env;
    ScmObj lst = args;
    ScmObj val = SCM_FALSE;

    if (NULLP(lst))
        return SCM_TRUE;
    
    for (; CONSP(CDR(lst)); lst = CDR(lst)) {
        val = EVAL(CAR(lst), env);
        if (FALSEP(val)) {
            eval_state->ret_type = SCM_RETTYPE_AS_IS;
            return SCM_FALSE;
        }
    }

    if (!NULLP(CDR(lst)))
        SigScm_ErrorObj("and: improper argument list: ", lst);

    return CAR(lst);
}

ScmObj ScmExp_or(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env = eval_state->env;
    ScmObj lst = args;
    ScmObj val = SCM_FALSE;

    if (NULLP(lst))
        return SCM_FALSE;
    
    for (; CONSP(CDR(lst)); lst = CDR(lst)) {
        val = EVAL(CAR(lst), env);
        if (NFALSEP(val)) {
            eval_state->ret_type = SCM_RETTYPE_AS_IS;
            return val;
        }
    }

    if (!NULLP(CDR(lst)))
        SigScm_ErrorObj("or: improper argument list: ", lst);

    return CAR(lst);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
ScmObj ScmExp_let(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env      = eval_state->env;
    ScmObj bindings = SCM_NULL;
    ScmObj body     = SCM_NULL;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj var      = SCM_NULL;
    ScmObj val      = SCM_NULL;
    ScmObj binding  = SCM_NULL;

    /* sanity check */
    if CHECK_2_ARGS(args)
        SigScm_Error("let : syntax error");

    /* guess whether syntax is "Named let" */
    if (SYMBOLP(CAR(args)))
        goto named_let;

    /* get bindings and body */
    bindings = CAR(args);
    body     = CDR(args);

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
            SCM_SHIFT_RAW_2(var, val, binding);

            vars = CONS(var, vars);
            vals = CONS(EVAL(val, env), vals);
        }

        /* create new environment for */
        env = extend_environment(vars, vals, env);
        eval_state->env = env;

        return ScmExp_begin(body, eval_state);
    }

    return ScmExp_begin(body, eval_state);

named_let:
    /* This code needs reworking.  <init>s should be evaluated in an
       environment where <procname> is not bound to the closure.
       <procname>'s scope also penetrates to the surrounding
       environment. */
    /*========================================================================
      (let <procname> <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    bindings = CADR(args);
    body     = CDDR(args);
    for (; !NULLP(bindings); bindings = CDR(bindings)) {
        binding = CAR(bindings);
        SCM_SHIFT_RAW_2(var, val, binding);
        vars = CONS(var, vars);
        vals = CONS(val, vals);
    }

    vars = ScmOp_reverse(vars);
    vals = ScmOp_reverse(vals);

    /* (define (<variable> <variable1> <variable2> ...>) <body>) */
    ScmExp_define(CAR(args),
                  LIST_1(Scm_NewClosure(CONS(vars, body), env)),
                  env);

    /* (func <init1> <init2> ...) */
    return CONS(CAR(args), vals);
}

ScmObj ScmExp_let_star(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env     = eval_state->env;
    ScmObj var     = SCM_NULL;
    ScmObj val     = SCM_NULL;
    ScmObj binding = SCM_NULL;

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
            SCM_SHIFT_RAW_2(var, val, binding);      
            val = EVAL(val, env);

            /* add env to each time!*/
            env = extend_environment(LIST_1(var), LIST_1(val), env);
        }
    } else if (NULLP(bindings)) {
        /* extend null environment */
        env = extend_environment(SCM_NULL,
                                 SCM_NULL,
                                 env);
    } else {
        SigScm_ErrorObj("let* : invalid binding form : ", bindings);
    }

    /* set new env */
    eval_state->env = env;
    /* evaluate */
    return ScmExp_begin(body, eval_state);
}

/* TODO: Simplify and optimize with SCM_SHIFT_*() macro */
ScmObj ScmExp_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env      = eval_state->env;
    ScmObj frame    = SCM_FALSE;
    ScmObj vars     = SCM_NULL;
    ScmObj vals     = SCM_NULL;
    ScmObj rest_vals = SCM_FALSE;
    ScmObj binding  = SCM_NULL;
    ScmObj var      = SCM_NULL;
    ScmObj val      = SCM_NULL;

    /*========================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    ========================================================================*/
    if (CONSP(bindings) || NULLP(bindings)) {
        /* extend env by placeholder frame for subsequent lambda evaluations */
        frame = CONS(SCM_NULL, SCM_NULL);
        env = CONS(frame, env);
        eval_state->env = env;

        for (; !NULLP(bindings); bindings = CDR(bindings)) {
            binding = CAR(bindings);

#if SCM_STRICT_ARGCHECK
            if (NULLP(binding) || NULLP(CDR(binding)))
                SigScm_ErrorObj("letrec : invalid binding form : ", binding);
#else
            if (NULLP(CDR(binding)))
                SET_CDR(binding, CONS(SCM_NULL, SCM_NULL));
#endif
            SCM_SHIFT_RAW_2(var, val, binding);

            /* construct vars and vals list: any <init> must not refer a
               <variable> at this time */
            vars = CONS(var, vars);
            vals = CONS(EVAL(val, env), vals);
        }

        /* fill placeholders */
        SET_CAR(frame, vars);
        SET_CDR(frame, vals);

        /* evaluate body */
        return ScmExp_begin(body, eval_state);
    }

    SigScm_Error("letrec : syntax error");
    return SCM_UNDEF;
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
ScmObj ScmExp_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env = eval_state->env;
    ScmObj lst = args;

    /* sanity check */
    if (NULLP(lst))
        return SCM_UNDEF;

    for (; CONSP(CDR(lst)); lst = CDR(lst))
        EVAL(CAR(lst), env);

    if (!NULLP(CDR(lst)))
        SigScm_ErrorObj("begin: improper argument list: ", args);

    /* Return tail expression. */
    return CAR(lst);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
ScmObj ScmExp_do(ScmObj bindings, ScmObj testframe, ScmObj commands, ScmEvalState *eval_state)
{
    /*
     * (do ((<variable1> <init1> <step1>)
     *      (<variable2> <init2> <step2>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     */
    ScmObj env        = eval_state->env;
    ScmObj vars       = SCM_NULL;
    ScmObj vals       = SCM_NULL;
    ScmObj steps      = SCM_NULL;
    ScmObj binding    = SCM_NULL;
    ScmObj step       = SCM_NULL;
    ScmObj test       = SCM_NULL;
    ScmObj expression = SCM_NULL;
    ScmObj tmp_vars   = SCM_NULL;
    ScmObj tmp_steps  = SCM_NULL;
    ScmObj obj        = SCM_NULL;

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
    test       = CAR(testframe);
    expression = CDR(testframe);

    /* now execution phase! */
    while (FALSEP(EVAL(test, env))) {
        /* execute commands */
        EVAL(ScmExp_begin(commands, eval_state), env);

        /*
         * Notice
         *
         * the result of the execution of <step>s must not depend on each other's
         * results. each execution must be done independently. So, we store the
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
                SigScm_Error("do : broken env");
            }
        }
    }

    eval_state->env = env;

    return ScmExp_begin(expression, eval_state);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
/* FIXME: rename to ScmExp_delay since delay is a syntax */
ScmObj ScmOp_delay(ScmObj expr, ScmObj env)
{
    /* (lambda () exp) */
    return Scm_NewClosure(SCM_LIST_2(SCM_NULL, expr), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/
/* FIXME: rename to ScmExp_quasiquote since quasiquote is a syntax */
ScmObj ScmOp_quasiquote(ScmObj datum, ScmObj env)
{
    ScmObj ret;
    ret = qquote_internal(datum, env, 1);

    if (QQUOTE_IS_VERBATIM(ret))
        return datum;
    return ret;
}

/* FIXME: rename to ScmExp_unquote since unquote is a syntax */
ScmObj ScmOp_unquote(ScmObj dummy, ScmObj env)
{
    SigScm_Error("unquote outside quasiquote");
    return SCM_NULL;
}

/*
 * FIXME: rename to ScmExp_unquote_splicing since unquote_splicing is a
 * syntax
 */
ScmObj ScmOp_unquote_splicing(ScmObj dummy, ScmObj env)
{
    SigScm_Error("unquote-splicing outside quasiquote");
    return SCM_NULL;
}


/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
ScmObj ScmExp_define(ScmObj var, ScmObj rest, ScmObj env)
{
    ScmObj exp      = SCM_NULL;
    ScmObj procname = SCM_NULL;
    ScmObj body     = SCM_NULL;
    ScmObj formals  = SCM_NULL;

    /*========================================================================
      (define <variable> <expression>)
    ========================================================================*/
    if (SYMBOLP(var)) {
        if (!NULLP(SCM_SHIFT_RAW_1(exp, rest)))
            SigScm_Error("define : missing expression");

        if (NULLP(env)) {
            /* given top-level environment */
            SCM_SYMBOL_SET_VCELL(var, EVAL(exp, env));
        } else {
            /* add val to the environment */
            env = add_environment(var, EVAL(exp, env), env);
        }

#if SCM_STRICT_R5RS
        return SCM_UNDEF;
#else
        return var;
#endif
    }

    /*========================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda (<formals>) <body>))
    ========================================================================*/
    if (CONSP(var)) {
        procname   = CAR(var);
        formals    = CDR(var);
        body       = rest;

        if (NULLP(body))
            SigScm_Error("define : missing function body");

        if (!SYMBOLP(procname))
            SigScm_ErrorObj("define : symbol required but got ", procname);

        return ScmExp_define(procname,
                             LIST_1(Scm_NewClosure(CONS(formals, body), env)),
                             env);
    }

    SigScm_ErrorObj("define : symbol required but got ", var);
    return SCM_UNDEF;
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
ScmObj ScmOp_scheme_report_environment(ScmObj version)
{
    /* sanity check */
    if (!INTP(version))
        SigScm_ErrorObj("scheme-report-environment : int required but got ", version);
    if (SCM_INT_VALUE(version) != 5)
        SigScm_ErrorObj("scheme-report-environment : version must be 5 but got ", version);

#if SCM_STRICT_R5RS
    SigScm_Error("scheme-report-environment :" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "scheme-report-environment : warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj ScmOp_null_environment(ScmObj version)
{
    /* sanity check */
    if (!INTP(version))
        SigScm_ErrorObj("null-environment : int required but got ", version);
    if (SCM_INT_VALUE(version) != 5)
        SigScm_ErrorObj("null-environment : version must be 5 but got ", version);

#if SCM_STRICT_R5RS
    SigScm_Error("null-environment :" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "null-environment : warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_INTERACTION_ENV;
}

ScmObj ScmOp_interaction_environment(void)
{
    return SCM_INTERACTION_ENV;
}
