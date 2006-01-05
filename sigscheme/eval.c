/*===========================================================================
 *  FileName : eval.c
 *  About    : Evaluation and function calling
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
#define EVAL_ARGS          scm_false
#define SUPPRESS_EVAL_ARGS scm_true

#define SCM_ERRMSG_WRONG_NR_ARG " Wrong number of arguments "
#define SCM_ERRMSG_NON_R5RS_ENV " the environment is not conformed to R5RS"

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env,
                     scm_bool suppress_eval);
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state);
static ScmObj call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
                   scm_bool suppress_eval);
static ScmObj map_eval(ScmObj args, ScmObj env);

/*=======================================
  Function Implementations
=======================================*/
ScmObj
scm_symbol_value(ScmObj var, ScmObj env)
{
    ScmRef ref;
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("scm_symbol_value");

    SCM_ASSERT(SYMBOLP(var));

    /* first, lookup the environment */
    ref = scm_lookup_environment(var, env);
    if (ref != SCM_INVALID_REF) {
        /* variable is found in environment, so returns its value */
        return DEREF(ref);
    }

    /* finally, look at the VCELL */
    val = SCM_SYMBOL_VCELL(var);
    if (EQ(val, SCM_UNBOUND))
        ERR_OBJ("unbound variable", var);

    return val;
}

/* A wrapper for call() for internal proper tail recursion */
ScmObj
scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    eval_state->ret_type = SCM_RETTYPE_AS_IS;
    return call(proc, args, eval_state, SUPPRESS_EVAL_ARGS);
}

/* Wrapper for call().  Just like scm_p_apply(), except ARGS is used
 * as given---nothing special is done about the last item in the
 * list. */
ScmObj
scm_call(ScmObj proc, ScmObj args)
{
    ScmEvalState state;
    ScmObj ret;

    /* We don't need a nonempty environemnt, because this function
     * will never be called directly from Scheme code.  If PROC is a
     * closure, it'll have its own environment, if it's a syntax, it's
     * an error, and if it's a C procedure, it doesn't have any free
     * variables at the Scheme level. */
    state.env      = SCM_INTERACTION_ENV;
    state.ret_type = SCM_RETTYPE_AS_IS;

    ret = call(proc, args, &state, SUPPRESS_EVAL_ARGS);
    if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
        ret = EVAL(ret, state.env);
    return ret;
}

/* ARGS should NOT have been evaluated yet. */
static ScmObj
reduce(ScmObj (*func)(), ScmObj args, ScmObj env, scm_bool suppress_eval)
{
    ScmObj left;
    ScmObj right;
    enum ScmReductionState state;
    DECLARE_INTERNAL_FUNCTION("(reduction)");

    state = SCM_REDUCE_0;
    if (NO_MORE_ARG(args))
        return (*func)(SCM_INVALID, SCM_INVALID, &state);

    state = SCM_REDUCE_1;
    left = POP_ARG(args);
    if (!suppress_eval)
        left = EVAL(left, env);
    if (NO_MORE_ARG(args))
        return (*func)(left, left, &state);

    /* Reduce upto all but the last argument. */
    state = SCM_REDUCE_PARTWAY;
    while (right = POP_ARG(args), !NO_MORE_ARG(args)) {
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
static ScmObj
call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    ScmObj formals;
    DECLARE_INTERNAL_FUNCTION("call_closure");

    /*
     * Description of the ScmClosure handling
     *
     * (lambda <formals> <body>)
     *
     * <formals> should have 3 forms.
     *
     *   (1) <variable>
     *   (2) (<variable1> <variable2> ...)
     *   (3) (<variable1> <variable2> ... <variable n-1> . <variable n>)
     */
    formals = CAR(SCM_CLOSURE_EXP(proc));

    if (SYMBOLP(formals)) {
        /* (1) <variable> */
        eval_state->env = scm_extend_environment(LIST_1(formals),
                                                 LIST_1(args),
                                                 SCM_CLOSURE_ENV(proc));
    } else if (CONSP(formals)) {
        /*
         * (2) (<variable1> <variable2> ...)
         * (3) (<variable1> <variable2> ... <variable n-1> . <variable n>)
         *
         *  - dot list is handled in lookup_frame().
         */
        eval_state->env = scm_extend_environment(formals,
                                                 args,
                                                 SCM_CLOSURE_ENV(proc));
    } else if (NULLP(formals)) {
        /*
         * (2') <variable> is '()
         */
        eval_state->env = scm_extend_environment(SCM_NULL,
                                                 SCM_NULL,
                                                 SCM_CLOSURE_ENV(proc));
    } else {
        ERR_OBJ("lambda: bad formals list", formals);
    }

    eval_state->ret_type = SCM_RETTYPE_NEED_EVAL;
    return scm_s_begin(CDR(SCM_CLOSURE_EXP(proc)), eval_state);
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
static ScmObj
call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
     scm_bool suppress_eval)
{
    ScmObj env, cont;
    ScmObj (*func)();
    enum ScmFuncTypeCode type;
    int mand_count, i;
    /* The +2 is for rest and env/eval_state. */
    void *argbuf[SCM_FUNCTYPE_MAND_MAX + 2];
    DECLARE_INTERNAL_FUNCTION("(function call)");

    env = eval_state->env;

    if (!suppress_eval)
        proc = EVAL(proc, env);

    if (!FUNCP(proc)) {
        if (CLOSUREP(proc)) {
            args = (suppress_eval) ? args : map_eval(args, env);
            return call_closure(proc, args, eval_state);
        }
        if (CONTINUATIONP(proc)) {
            if (!LIST_1_P(args))
                ERR("continuation takes exactly one argument");
            cont = (suppress_eval) ? CAR(args) : EVAL(CAR(args), env);
            scm_call_continuation(proc, cont);
            /* NOTREACHED */
        }
        SCM_ASSERT(scm_false);
    }

    /* We have a C function. */

    type = SCM_FUNC_TYPECODE(proc);
    func = SCM_FUNC_CFUNC(proc);

    if (type == SCM_REDUCTION_OPERATOR)
        return reduce(func, args, env, suppress_eval);

    /* Suppress argument evaluation for syntaxes. */
    if (suppress_eval) {
        if (type & SCM_FUNCTYPE_SYNTAX)
            ERR_OBJ("can't apply/map a syntax", proc);
    } else {
        suppress_eval = type & SCM_FUNCTYPE_SYNTAX;
    }

    /* Collect mandatory arguments. */
    mand_count = type & SCM_FUNCTYPE_MAND_MASK;
    SCM_ASSERT(mand_count <= SCM_FUNCTYPE_MAND_MAX);
    for (i = 0; i < mand_count; i++) {
        argbuf[i] = MUST_POP_ARG(args);
        if (!suppress_eval)
            argbuf[i] = EVAL(argbuf[i], env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP((ScmObj)argbuf[i]))
            ERR_OBJ("multiple values are not allowed here", (ScmObj)argbuf[i]);
#endif
    }

    if (type & SCM_FUNCTYPE_VARIADIC) {
        if (!suppress_eval)
            args = map_eval(args, env);
        argbuf[i++] = args;
    } else {
        ASSERT_NO_MORE_ARG(args);
    }

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
        SCM_ASSERT(scm_false);
        return SCM_INVALID;
    }
}

/*===========================================================================
  S-Expression Evaluation
===========================================================================*/
ScmObj
scm_p_eval(ScmObj obj, ScmObj env)
{
    DECLARE_FUNCTION("eval", procedure_fixed_2);

    ENSURE_ENV(env);

    return scm_eval(obj, env);
}

ScmObj
scm_eval(ScmObj obj, ScmObj env)
{
    ScmObj ret;
    ScmEvalState state;

#if SCM_DEBUG
    scm_push_trace_frame(obj, env);
#endif

    state.env = env;

eval_loop:
#if SCM_STRICT_R5RS
    /* () is allowed by default for efficiency */
    if (NULLP(obj))
        ERR("eval: () is not a valid R5RS form. use '() instead");
#endif
    switch (SCM_TYPE(obj)) {
    case ScmSymbol:
        ret = scm_symbol_value(obj, state.env);
        break;

    case ScmCons:
        obj = call(CAR(obj), CDR(obj), &state, EVAL_ARGS);
        if (state.ret_type == SCM_RETTYPE_NEED_EVAL)
            goto eval_loop;
        /* FALLTHROUGH */
    default:
        ret = obj;
        break;
    }

#if SCM_DEBUG
    scm_pop_trace_frame();
#endif
    return ret;
}

ScmObj
scm_p_apply(ScmObj proc, ScmObj arg0, ScmObj rest, ScmEvalState *eval_state)
{
    ScmQueue q;
    ScmObj args, arg, last;
    DECLARE_FUNCTION("apply", procedure_variadic_tailrec_2);

    if (NULLP(rest)) {
        args = last = arg0;
    } else {
        /* More than one argument given. */
        args = LIST_1(arg0);
        q = REF_CDR(args);
        while (arg = POP_ARG(rest), !NO_MORE_ARG(rest))
            SCM_QUEUE_ADD(q, arg);
        /* The last one is spliced. */
        SCM_QUEUE_SLOPPY_APPEND(q, arg);
        last = arg;
    }

    ENSURE_LIST(last);

    /* The last argument inhibits argument re-evaluation. */
    return call(proc, args, eval_state, SUPPRESS_EVAL_ARGS);
}

static ScmObj
map_eval(ScmObj args, ScmObj env)
{
    ScmQueue q;
    ScmObj res, elm;
    DECLARE_INTERNAL_FUNCTION("(function call)");

    if (NULLP(args))
        return SCM_NULL;

    res = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, res);
    /* does not use POP_ARG() to increace performance */
    for (; CONSP(args); args = CDR(args)) {
        elm = EVAL(CAR(args), env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP(elm))
            ERR_OBJ("multiple values are not allowed here", elm);
#endif
        SCM_QUEUE_ADD(q, elm);
    }
    /* dot list */
    if (!NULLP(args)) {
        elm = EVAL(args, env);
        SCM_QUEUE_SLOPPY_APPEND(q, elm);
    }

    return res;
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
ScmObj
scm_p_scheme_report_environment(ScmObj version)
{
    DECLARE_FUNCTION("scheme-report-environment", procedure_fixed_1);

    ENSURE_INT(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR("scheme-report-environment:" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "scheme-report-environment: warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_R5RS_ENV;
}

ScmObj
scm_p_null_environment(ScmObj version)
{
    DECLARE_FUNCTION("null-environment", procedure_fixed_1);

    ENSURE_INT(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR("null-environment:" SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "null-environment: warning:" SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_NULL_ENV;
}

ScmObj
scm_p_interaction_environment(void)
{
    DECLARE_FUNCTION("interaction-environment", procedure_fixed_0);

    return SCM_INTERACTION_ENV;
}
