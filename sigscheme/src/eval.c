/*===========================================================================
 *  Filename : eval.c
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
#define SCM_ERRMSG_NON_R5RS_ENV "the environment is not conformed to R5RS"

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj reduce(ScmObj (*func)(), ScmObj args, ScmObj env,
                     enum ScmValueType need_eval);
static void call_continuation(ScmObj cont, ScmObj args,
                              ScmEvalState *eval_state,
                              enum ScmValueType need_eval) SCM_NORETURN;
static ScmObj call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
                           enum ScmValueType need_eval);
static ScmObj call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
                   enum ScmValueType need_eval);
static ScmObj map_eval(ScmObj args, scm_int_t *args_len, ScmObj env);

/*=======================================
  Function Definitions
=======================================*/
/* A wrapper for call() for internal proper tail recursion */
SCM_EXPORT ScmObj
scm_tailcall(ScmObj proc, ScmObj args, ScmEvalState *eval_state)
{
    SCM_ASSERT(PROPER_LISTP(args));

    eval_state->ret_type = SCM_VALTYPE_AS_IS;
    return call(proc, args, eval_state, SCM_VALTYPE_AS_IS);
}

/* Wrapper for call().  Just like scm_p_apply(), except ARGS is used
 * as given---nothing special is done about the last item in the
 * list. */
SCM_EXPORT ScmObj
scm_call(ScmObj proc, ScmObj args)
{
    ScmEvalState state;
    ScmObj ret;

    SCM_ASSERT(PROPER_LISTP(args));

    /* We don't need a nonempty environemnt, because this function
     * will never be called directly from Scheme code.  If PROC is a
     * closure, it'll have its own environment, if it's a syntax, it's
     * an error, and if it's a C procedure, it doesn't have any free
     * variables at the Scheme level. */
    SCM_EVAL_STATE_INIT2(state, SCM_INTERACTION_ENV, SCM_VALTYPE_AS_IS);

    ret = call(proc, args, &state, SCM_VALTYPE_AS_IS);
    return SCM_FINISH_TAILREC_CALL(ret, &state);
}

/* ARGS should NOT have been evaluated yet. */
static ScmObj
reduce(ScmObj (*func)(), ScmObj args, ScmObj env, enum ScmValueType need_eval)
{
    ScmObj left, right;
    enum ScmReductionState state;
    DECLARE_INTERNAL_FUNCTION("(reduction)");

    if (NO_MORE_ARG(args)) {
        state = SCM_REDUCE_0;
        return (*func)(SCM_INVALID, SCM_INVALID, &state);
    }

    left = POP(args);
    if (need_eval)
        left = EVAL(left, env);

    if (NO_MORE_ARG(args)) {
        state = SCM_REDUCE_1;
        return (*func)(left, left, &state);
    }

    /* Reduce upto the penult. */
    state = SCM_REDUCE_PARTWAY;
    FOR_EACH_BUTLAST (right, args) {
        if (need_eval)
            right = EVAL(right, env);
        left = (*func)(left, right, &state);
        if (state == SCM_REDUCE_STOP)
            return left;
    }
    ASSERT_NO_MORE_ARG(args);

    /* Make the last call. */
    state = SCM_REDUCE_LAST;
    if (need_eval)
        right = EVAL(right, env);
    return (*func)(left, right, &state);
}

static void
call_continuation(ScmObj cont, ScmObj args, ScmEvalState *eval_state,
                  enum ScmValueType need_eval)
{
    ScmObj ret;
    DECLARE_INTERNAL_FUNCTION("call_continuation");

    if (!LIST_1_P(args))
        ERR_OBJ("continuation takes exactly one argument but got", args);
    ret = CAR(args);
    if (need_eval)
        ret = EVAL(ret, eval_state->env);
    scm_call_continuation(cont, ret);
    /* NOTREACHED */
}

static ScmObj
call_closure(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
             enum ScmValueType need_eval)
{
    ScmObj exp, formals, body, proc_env;
    scm_int_t formals_len, args_len;
    DECLARE_INTERNAL_FUNCTION("call_closure");

    /*
     * Description of the ScmClosure handling
     *
     * (lambda <formals> <body>)
     *
     * <formals> may have 3 forms.
     *
     *   (1) <variable>
     *   (2) (<variable1> <variable2> ...)
     *   (3) (<variable1> <variable2> ... <variable n-1> . <variable n>)
     */
    exp      = SCM_CLOSURE_EXP(proc);
    formals  = CAR(exp);
    body     = CDR(exp);
    proc_env = SCM_CLOSURE_ENV(proc);
    if (need_eval) {
        args = map_eval(args, &args_len, eval_state->env);
    } else {
        args_len = scm_validate_actuals(args);
        if (SCM_LISTLEN_ERRORP(args_len))
            goto err_improper;
    }

    if (IDENTIFIERP(formals)) {
        /* (1) <variable> */
        formals = LIST_1(formals);
        args    = LIST_1(args);
    } else if (CONSP(formals)) {
        /*
         * (2) (<variable1> <variable2> ...)
         * (3) (<variable1> <variable2> ... <variable n-1> . <variable n>)
         *
         *  - dotted list is handled in env.c
         */
        /* scm_finite_length() is enough since formals is fully validated
         * previously */
        formals_len = scm_finite_length(formals);
        if (!scm_valid_environment_extension_lengthp(formals_len, args_len))
            goto err_improper;
    } else if (NULLP(formals)) {
        /*
         * (2') <variable> is '()
         */
        if (args_len)
            goto err_improper;

        formals = args = SCM_NULL;
    } else {
        SCM_ASSERT(scm_false);
    }

    eval_state->env = scm_extend_environment(formals, args, proc_env);
    eval_state->ret_type = SCM_VALTYPE_NEED_EVAL;
    return scm_s_body(body, eval_state);

 err_improper:
    ERR_OBJ("unmatched number or improper args", args);
}

/**
 * @param proc The procedure or syntax to call.
 * @param args The argument list.
 * @param eval_state The calling evaluator's state.
 * @param need_eval Indicates that @a args need be evaluated.
 */
static ScmObj
call(ScmObj proc, ScmObj args, ScmEvalState *eval_state,
     enum ScmValueType need_eval)
{
    ScmObj env;
    ScmObj (*func)();
    enum ScmFuncTypeCode type;
    scm_bool syntaxp;
    int mand_count, i;
    scm_int_t variadic_len;
    /* The +2 is for rest and env/eval_state. */
    ScmObj argbuf[SCM_FUNCTYPE_MAND_MAX + 2];
    DECLARE_INTERNAL_FUNCTION("(function call)");

    env = eval_state->env;

    if (need_eval)
        proc = EVAL(proc, env);

    if (!FUNCP(proc)) {
        if (CLOSUREP(proc))
            return call_closure(proc, args, eval_state, need_eval);
#if SCM_USE_HYGIENIC_MACRO
        if (HMACROP(proc)) {
            if (!need_eval)
                ERR_OBJ("can't apply/map a macro", proc);
            return scm_expand_macro(proc, args, eval_state);
        }
#endif
        if (CONTINUATIONP(proc)) {
            call_continuation(proc, args, eval_state, need_eval);
            /* NOTREACHED */
        }
        ERR_OBJ("procedure or syntax required but got", proc);
    }

    /* We have a C function. */

    type = SCM_FUNC_TYPECODE(proc);
    func = SCM_FUNC_CFUNC(proc);

    if (type == SCM_REDUCTION_OPERATOR)
        return reduce(func, args, env, need_eval);

    syntaxp = type & SCM_FUNCTYPE_SYNTAX;
    if (syntaxp) {
        if (need_eval)
            need_eval = scm_false;
        else
            ERR_OBJ("can't apply/map a syntax", proc);
    }

    /* Collect mandatory arguments. */
    mand_count = type & SCM_FUNCTYPE_MAND_MASK;
    SCM_ASSERT(mand_count <= SCM_FUNCTYPE_MAND_MAX);
    for (i = 0; i < mand_count; i++) {
        argbuf[i] = MUST_POP_ARG(args);
        if (need_eval)
            argbuf[i] = EVAL(argbuf[i], env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP((ScmObj)argbuf[i]))
            ERR_OBJ("multiple values are not allowed here", (ScmObj)argbuf[i]);
#endif
    }

    if (type & SCM_FUNCTYPE_VARIADIC) {
        if (need_eval)
            args = map_eval(args, &variadic_len, env);
#if 0
        /* Since this check is expensive, each syntax should do. Other
         * procedures are already ensured that having proper args here. */
        else if (syntaxp && !PROPER_LISTP(args))
            ERR_OBJ(SCM_ERRMSG_IMPROPER_ARGS, args);
#endif
        argbuf[i++] = args;
    } else {
        ASSERT_NO_MORE_ARG(args);
    }

    if (type & SCM_FUNCTYPE_TAILREC) {
        eval_state->ret_type = SCM_VALTYPE_NEED_EVAL;
        argbuf[i++] = (ScmObj)eval_state;
    } else {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        if (type & SCM_FUNCTYPE_SYNTAX)
            argbuf[i++] = env;
    }

    switch (i) {
    case 0:
        return (*func)();
    case 1:
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)((void *)argbuf[0]);
        return (*func)(argbuf[0]);
    case 2:
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], (void *)argbuf[1]);
        return (*func)(argbuf[0], argbuf[1]);
    case 3:
#if SCM_FUNCTYPE_MAND_MAX >= 1
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], argbuf[1], (void *)argbuf[2]);
        return (*func)(argbuf[0], argbuf[1], argbuf[2]);
#endif
    case 4:
#if SCM_FUNCTYPE_MAND_MAX >= 2
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], argbuf[1], argbuf[2], (void *)argbuf[3]);
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3]);
#endif
    case 5:
#if SCM_FUNCTYPE_MAND_MAX >= 3
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], (void *)argbuf[4]);
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4]);
#endif
    case 6:
#if SCM_FUNCTYPE_MAND_MAX >= 4
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], (void *)argbuf[5]);
        return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5]);
#endif
    case 7:
#if SCM_FUNCTYPE_MAND_MAX >= 5
        if ((sizeof(void *) < sizeof(ScmObj)) && (type & SCM_FUNCTYPE_TAILREC))
            return (*func)(argbuf[0], argbuf[1], argbuf[2], argbuf[3], argbuf[4], argbuf[5], (void *)argbuf[6]);
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
SCM_EXPORT ScmObj
scm_p_eval(ScmObj obj, ScmObj env)
{
    DECLARE_FUNCTION("eval", procedure_fixed_2);

    ENSURE_VALID_ENV(env);

    return scm_eval(obj, env);
}

SCM_EXPORT ScmObj
scm_eval(ScmObj obj, ScmObj env)
{
    ScmEvalState state;

#if SCM_STRICT_TOPLEVEL_DEFINITIONS
    /* FIXME: temporary hack */
    if (EQ(env, SCM_INTERACTION_ENV_INDEFINABLE)) {
        env = SCM_INTERACTION_ENV;
        SCM_EVAL_STATE_INIT1(state, env);
        state.nest = SCM_NEST_COMMAND;
    } else if (EQ(env, SCM_INTERACTION_ENV)) {
        SCM_EVAL_STATE_INIT1(state, env);
        state.nest = SCM_NEST_PROGRAM;
    } else {
        SCM_EVAL_STATE_INIT1(state, env);
    }
#else
    /* intentionally does not use SCM_EVAL_STATE_INIT() to avoid overhead */
    state.env = env;
#endif

#if SCM_DEBUG
    scm_push_trace_frame(obj, env);
#endif

eval_loop:
    if (IDENTIFIERP(obj)) {
        obj = scm_symbol_value(obj, state.env);
    } else if (CONSP(obj)) {
        obj = call(CAR(obj), CDR(obj), &state, SCM_VALTYPE_NEED_EVAL);
        if (state.ret_type == SCM_VALTYPE_NEED_EVAL) {
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
            if (state.nest == SCM_NEST_RETTYPE_BEGIN)
                state.nest = SCM_NEST_COMMAND_OR_DEFINITION;
            else
                state.nest = SCM_NEST_COMMAND;
#endif
            goto eval_loop;
        }
    }
#if SCM_STRICT_NULL_FORM
    /* () is allowed by default for efficiency */
    else if (NULLP(obj))
        PLAIN_ERR("eval: () is not a valid R5RS form. use '() instead");
#endif
#if SCM_STRICT_VECTOR_FORM
    else if (VECTORP(obj))
        PLAIN_ERR("eval: #() is not a valid R5RS form. use '#() instead");
#endif

#if SCM_DEBUG
    scm_pop_trace_frame();
#endif
    return obj;
}

SCM_EXPORT ScmObj
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
        FOR_EACH_BUTLAST (arg, rest)
            SCM_QUEUE_ADD(q, arg);
        /* The last one is spliced. */
        SCM_QUEUE_SLOPPY_APPEND(q, arg);
        last = arg;
    }

    ENSURE_LIST(last);

    return call(proc, args, eval_state, SCM_VALTYPE_AS_IS);
}

static ScmObj
map_eval(ScmObj args, scm_int_t *args_len, ScmObj env)
{
    ScmQueue q;
    ScmObj ret, elm, rest;
    scm_int_t len;
    DECLARE_INTERNAL_FUNCTION("(function call)");

    if (NULLP(args)) {
        *args_len = 0;
        return SCM_NULL;
    }

    ret = SCM_NULL;
    SCM_QUEUE_POINT_TO(q, ret);

    len = 0;
    FOR_EACH_PAIR (rest, args) {
        len++;
        elm = EVAL(CAR(rest), env);
#if SCM_STRICT_ARGCHECK
        if (VALUEPACKETP(elm))
            ERR_OBJ("multiple values are not allowed here", elm);
#endif
        SCM_QUEUE_ADD(q, elm);
    }
    if (!NULLP(rest))
        ERR_OBJ(SCM_ERRMSG_IMPROPER_ARGS, args);

    *args_len = len;
    return ret;
}

/*=======================================
  R5RS : 6.5 Eval
=======================================*/
SCM_EXPORT ScmObj
scm_p_scheme_report_environment(ScmObj version)
{
    DECLARE_FUNCTION("scheme-report-environment", procedure_fixed_1);

    ENSURE_INT(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR(SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "scheme-report-environment: warning: " SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_R5RS_ENV;
}

SCM_EXPORT ScmObj
scm_p_null_environment(ScmObj version)
{
    DECLARE_FUNCTION("null-environment", procedure_fixed_1);

    ENSURE_INT(version);
    if (SCM_INT_VALUE(version) != 5)
        ERR_OBJ("version must be 5 but got", version);

#if SCM_STRICT_R5RS
    ERR(SCM_ERRMSG_NON_R5RS_ENV);
#else
    CDBG((SCM_DBG_COMPAT,
          "null-environment: warning: " SCM_ERRMSG_NON_R5RS_ENV));
#endif

    return SCM_NULL_ENV;
}

SCM_EXPORT ScmObj
scm_p_interaction_environment(void)
{
    DECLARE_FUNCTION("interaction-environment", procedure_fixed_0);

    return SCM_INTERACTION_ENV;
}
