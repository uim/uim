/*===========================================================================
 *  Filename : syntax.c
 *  About    : R5RS syntaxes
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
#define ERRMSG_CLAUSE_REQUIRED     "at least 1 clause required"
#define ERRMSG_EXPRESSION_REQUIRED "at least 1 expression required"
#define ERRMSG_INVALID_BINDINGS    "invalid bindings form"
#define ERRMSG_INVALID_BINDING     "invalid binding form"
#define ERRMSG_SYNTAX_AS_VALUE     "syntactic keyword is passed as value"
#define ERRMSG_DUPLICATE_VARNAME   "duplicate variable name"
#define ERRMSG_BAD_DEFINE_FORM     "bad definition form"

#if SCM_USE_INTERNAL_DEFINITIONS
#define ERRMSG_BAD_DEFINE_PLACEMENT "definitions are valid only at toplevel" \
                                    " or beginning of a binding construct"
#else
#define ERRMSG_BAD_DEFINE_PLACEMENT "internal definitions feature is disabled"
#endif

/* FIXME: temporary hack */
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
#define FORBID_TOPLEVEL_DEFINITIONS(env)                                     \
    (EQ((env), SCM_INTERACTION_ENV) ? SCM_INTERACTION_ENV_INDEFINABLE : (env))
#else
#define FORBID_TOPLEVEL_DEFINITIONS(env) (env)
#endif

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#include "functable-r5rs-syntax.c"

SCM_DEFINE_EXPORTED_VARS(syntax);

SCM_GLOBAL_VARS_BEGIN(static_syntax);
#define static
static ScmObj l_sym_else, l_sym_yields, l_sym_define;
#if SCM_USE_INTERNAL_DEFINITIONS
static ScmObj l_sym_begin, l_syn_lambda;
#endif /* SCM_USE_INTERNAL_DEFINITIONS */
#undef static
SCM_GLOBAL_VARS_END(static_syntax);
#define l_sym_else   SCM_GLOBAL_VAR(static_syntax, l_sym_else)
#define l_sym_yields SCM_GLOBAL_VAR(static_syntax, l_sym_yields)
#define l_sym_define SCM_GLOBAL_VAR(static_syntax, l_sym_define)
#define l_sym_begin  SCM_GLOBAL_VAR(static_syntax, l_sym_begin)
#define l_syn_lambda SCM_GLOBAL_VAR(static_syntax, l_syn_lambda)
SCM_DEFINE_STATIC_VARS(static_syntax);

/*=======================================
  File Local Function Declarations
=======================================*/
#if SCM_USE_INTERNAL_DEFINITIONS
static ScmObj filter_definitions(ScmObj body, ScmObj *formals, ScmObj *actuals,
                                 ScmQueue *def_expq);
#endif
static void define_internal(ScmObj var, ScmObj exp, ScmObj env);

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_init_syntax(void)
{
    SCM_GLOBAL_VARS_INIT(syntax);
    SCM_GLOBAL_VARS_INIT(static_syntax);

    scm_register_funcs(scm_r5rs_syntax_func_info_table);

    scm_sym_quote            = scm_intern("quote");
    scm_sym_quasiquote       = scm_intern("quasiquote");
    scm_sym_unquote          = scm_intern("unquote");
    scm_sym_unquote_splicing = scm_intern("unquote-splicing");
    scm_sym_ellipsis         = scm_intern("...");

    l_sym_else   = scm_intern("else");
    l_sym_yields = scm_intern("=>");
    l_sym_define = scm_intern("define");
#if SCM_USE_INTERNAL_DEFINITIONS
    l_sym_begin  = scm_intern("begin");
    scm_gc_protect_with_init(&l_syn_lambda,
                             scm_symbol_value(scm_intern("lambda"),
                                              SCM_INTERACTION_ENV));
#endif
}

/*=======================================
  R5RS : 4.1 Primitive expression types
=======================================*/
/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.2 Literal expressions
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_quote(ScmObj datum, ScmObj env)
{
    DECLARE_FUNCTION("quote", syntax_fixed_1);

#if SCM_USE_HYGIENIC_MACRO
    /* Passing objects that contain a circular list to SCM_UNWRAP_SYNTAX()
     * causes infinite loop. For instance, (error circular-list) raises it via
     * the error object which contains the circular list.
     *   -- YamaKen 2006-10-02 */
    if (ERROBJP(datum))
        return datum;
#endif

    return SCM_UNWRAP_SYNTAX(datum);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.4 Procedures
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_lambda(ScmObj formals, ScmObj body, ScmObj env)
{
    DECLARE_FUNCTION("lambda", syntax_variadic_1);

#if SCM_STRICT_ARGCHECK
    if (SCM_LISTLEN_ERRORP(scm_validate_formals(formals)))
        ERR_OBJ("bad formals", formals);

    /* Keeping variable name unique is user's responsibility. R5RS: "It is an
     * error for a <variable> to appear more than once in <formals>.". */
#else
    /* Crashless no-validation:
     * Regard any non-list object as symbol. Since the lookup operation search
     * for a variable by EQ, this is safe although loosely allows
     * R5RS-incompatible code. */
#endif

    /* Internal definitions-only body such as ((define foo bar)) is
     * invalid. But since checking it here is inefficient, it is deferred to
     * scm_s_body() on being called. */
    if (!CONSP(body))
        ERR_OBJ(ERRMSG_EXPRESSION_REQUIRED, body);

    return MAKE_CLOSURE(CONS(formals, body), env);
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.5 Conditionals
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_if(ScmObj test, ScmObj conseq, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj env, alt;
    DECLARE_FUNCTION("if", syntax_variadic_tailrec_2);

    env = eval_state->env;

    /*=======================================================================
      (if <test> <consequent>)
      (if <test> <consequent> <alternate>)
    =======================================================================*/

    if (test = EVAL(test, env), TRUEP(test)) {
#if SCM_STRICT_ARGCHECK
        SAFE_POP(rest);
        ASSERT_NO_MORE_ARG(rest);
#endif
        return conseq;
    } else {
        alt = (CONSP(rest)) ? CAR(rest) : SCM_UNDEF;
#if SCM_STRICT_ARGCHECK
        SAFE_POP(rest);
        ASSERT_NO_MORE_ARG(rest);
#endif
        return alt;
    }
}

/*===========================================================================
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignments
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_setx(ScmObj sym, ScmObj exp, ScmObj env)
{
    ScmObj evaled;
    ScmRef locally_bound;
    DECLARE_FUNCTION("set!", syntax_fixed_2);

    ENSURE_SYMBOL(sym);

    evaled = EVAL(exp, env);
    locally_bound = scm_lookup_environment(sym, env);
    if (locally_bound != SCM_INVALID_REF) {
        SET(locally_bound, evaled);
    } else {
        if (!SCM_SYMBOL_BOUNDP(sym))
            ERR_OBJ("unbound variable", sym);

        SCM_SYMBOL_SET_VCELL(sym, evaled);
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
/* body of 'cond' and 'guard' of SRFI-34 */
SCM_EXPORT ScmObj
scm_s_cond_internal(ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj env, clause, test, exps, proc;
    DECLARE_INTERNAL_FUNCTION("cond" /* , syntax_variadic_tailrec_0 */);

    env = eval_state->env;
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
    eval_state->nest = SCM_NEST_COMMAND;
#endif

    /*
     * (cond <cond clause>+)
     * (cond <cond clause>* (else <sequence>))
     *
     * <cond clause> --> (<test> <sequence>)
     *       | (<test>)
     *       | (<test> => <recipient>)
     * <recipient> --> <expression>
     * <test> --> <expression>
     * <sequence> --> <command>* <expression>
     * <command> --> <expression>
     */

    if (NO_MORE_ARG(clauses))
        ERR(ERRMSG_CLAUSE_REQUIRED);

    /* looping in each clause */
    FOR_EACH (clause, clauses) {
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

#if 0
        test = SCM_UNWRAP_SYNTAX(test);  /* FIXME: needed? */
#endif
        if (EQ(test, l_sym_else)) {
            ASSERT_NO_MORE_ARG(clauses);
            return scm_s_begin(exps, eval_state);
        }
        
        if (test = EVAL(test, env), TRUEP(test)) {
            /*
             * if the selected <clause> contains only the <test> and no
             * <expression>s, then the value of the <test> is returned as the
             * result.
             */
            if (NULLP(exps)) {
                eval_state->ret_type = SCM_VALTYPE_AS_IS;
                return test;
            }

            /*
             * If the selected <clause> uses the => alternate form, then the
             * <expression> is evaluated. Its value must be a procedure that
             * accepts one argument; this procedure is then called on the value
             * of the <test> and the value returned by this procedure is
             * returned by the cond expression.
             */
            if (EQ(l_sym_yields, CAR(exps)) && LIST_2_P(exps)) {
                proc = EVAL(CADR(exps), env);
                if (!PROCEDUREP(proc))
                    ERR_OBJ("exp after => must be a procedure but got", proc);

                /*
                 * R5RS: 3.5 Proper tail recursion
                 *
                 * If a `cond' expression is in a tail context, and has a
                 * clause of the form `(<expression1> => <expression2>)' then
                 * the (implied) call to the procedure that results from the
                 * evaluation of <expression2> is in a tail
                 * context. <expression2> itself is not in a tail context.
                 */
                return LIST_2(proc, LIST_2(SYM_QUOTE, test));
            }

            return scm_s_begin(exps, eval_state);
        }
    }
    ASSERT_NO_MORE_ARG(clauses);

    /*
     * To distinguish unmatched status from SCM_UNDEF from a clause, pure
     * internal value SCM_INVALID is returned. Don't pass it to Scheme world.
     */
    eval_state->ret_type = SCM_VALTYPE_AS_IS;
    return SCM_INVALID;
}

SCM_EXPORT ScmObj
scm_s_cond(ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("cond", syntax_variadic_tailrec_0);

    ret = scm_s_cond_internal(clauses, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_s_case(ScmObj key, ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj clause, test, exps;
    DECLARE_FUNCTION("case", syntax_variadic_tailrec_1);

    /*
     * (case <expression>
     *   <case clause>+)
     *
     * (case <expression>
     *   <case clause>*
     *   (else <sequence>))
     *
     * <case clause> --> ((<datum>*) <sequence>)
     * <sequence> --> <command>* <expression>
     * <command> --> <expression>
     * <Datum> is what the read procedure (see section 6.6.2 Input)
     * successfully parses.
     */

    if (NO_MORE_ARG(clauses))
        ERR(ERRMSG_CLAUSE_REQUIRED);

    key = EVAL(key, eval_state->env);

    FOR_EACH (clause, clauses) {
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

        test = SCM_UNWRAP_SYNTAX(test);
        if (EQ(test, l_sym_else))
            ASSERT_NO_MORE_ARG(clauses);
        else
            test = scm_p_memv(key, test);

        if (TRUEP(test)) {
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
            eval_state->nest = SCM_NEST_COMMAND;
#endif
            return scm_s_begin(exps, eval_state);
        }
    }
    ASSERT_NO_MORE_ARG(clauses);

    return SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_s_and(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr, val, env;
    DECLARE_FUNCTION("and", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_TRUE;
    }
    env = FORBID_TOPLEVEL_DEFINITIONS(eval_state->env);

    FOR_EACH_BUTLAST (expr, args) {
        val = EVAL(expr, env);
        if (FALSEP(val)) {
            ASSERT_PROPER_ARG_LIST(args);
            eval_state->ret_type = SCM_VALTYPE_AS_IS;
            return SCM_FALSE;
        }
    }
    ASSERT_NO_MORE_ARG(args);

    return expr;
}

SCM_EXPORT ScmObj
scm_s_or(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr, val, env;
    DECLARE_FUNCTION("or", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_FALSE;
    }
    env = FORBID_TOPLEVEL_DEFINITIONS(eval_state->env);

    FOR_EACH_BUTLAST (expr, args) {
        val = EVAL(expr, env);
        if (TRUEP(val)) {
            ASSERT_PROPER_ARG_LIST(args);
            eval_state->ret_type = SCM_VALTYPE_AS_IS;
            return val;
        }
    }
    ASSERT_NO_MORE_ARG(args);

    return expr;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.2 Binding constructs
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_let(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env, named_let_sym, proc, binding;
    ScmObj formals, var, actuals, val, exp;
    ScmQueue varq, valq;
    DECLARE_FUNCTION("let", syntax_variadic_tailrec_1);

    env = eval_state->env;
    named_let_sym = SCM_FALSE;

    /*=======================================================================
      normal let:

        (let (<binding spec>*) <body>)

      named let:

        (let <variable> (<binding spec>*) <body>)

      <binding spec> --> (<variable> <expression>)
      <body> --> <definition>* <sequence>
      <definition> --> (define <variable> <expression>)
            | (define (<variable> <def formals>) <body>)
            | (begin <definition>*)
      <sequence> --> <command>* <expression>
      <command> --> <expression>
    =======================================================================*/

    /* named let */
    if (IDENTIFIERP(bindings)) {
        named_let_sym = bindings;

        if (!CONSP(body))
            ERR("invalid named let form");
        bindings = POP(body);
    }

    formals = actuals = SCM_NULL;
    SCM_QUEUE_POINT_TO(varq, formals);
    SCM_QUEUE_POINT_TO(valq, actuals);
    FOR_EACH (binding, bindings) {
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
            ERR_OBJ(ERRMSG_INVALID_BINDING, binding);
#if SCM_STRICT_ARGCHECK
        /* Optional check. Keeping variable name unique is user's
         * responsibility. R5RS: "It is an error for a <variable> to appear
         * more than once in the list of variables being bound." */
        if (TRUEP(scm_p_memq(var, formals)))
            ERR_OBJ(ERRMSG_DUPLICATE_VARNAME, var);
#endif
        exp = CADR(binding);
        val = EVAL(exp, env);
        if (SYNTAXP(val))
            ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, exp);

        SCM_QUEUE_ADD(varq, var);
        SCM_QUEUE_ADD(valq, val);
    }
    if (!NULLP(bindings))
        ERR_OBJ(ERRMSG_INVALID_BINDINGS, bindings);

    env = scm_extend_environment(formals, actuals, env);

    /* named let */
    if (IDENTIFIERP(named_let_sym)) {
        proc = MAKE_CLOSURE(CONS(formals, body), env);
        env = scm_add_environment(named_let_sym, proc, env);
        SCM_CLOSURE_SET_ENV(proc, env);
    }

    eval_state->env = env;
    return scm_s_body(body, eval_state);
}

SCM_EXPORT ScmObj
scm_s_letstar(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env, var, val, exp, binding;
    DECLARE_FUNCTION("let*", syntax_variadic_tailrec_1);

    env = eval_state->env;

    /*=======================================================================
      (let* (<binding spec>*) <body>)

      <binding spec> --> (<variable> <expression>)
      <body> --> <definition>* <sequence>
      <definition> --> (define <variable> <expression>)
            | (define (<variable> <def formals>) <body>)
            | (begin <definition>*)
      <sequence> --> <command>* <expression>
      <command> --> <expression>
    =======================================================================*/

    FOR_EACH (binding, bindings) {
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
            ERR_OBJ(ERRMSG_INVALID_BINDING, binding);

        exp = CADR(binding);
        val = EVAL(exp, env);
        if (SYNTAXP(val))
            ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, exp);

        /* extend env for each variable */
        env = scm_extend_environment(LIST_1(var), LIST_1(val), env);
    }
    if (!NULLP(bindings))
        ERR_OBJ(ERRMSG_INVALID_BINDINGS, bindings);

    eval_state->env = env;
    return scm_s_body(body, eval_state);
}

SCM_EXPORT ScmObj
scm_s_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj binding, formals, actuals, var, val, exp, env;
    DECLARE_FUNCTION("letrec", syntax_variadic_tailrec_1);

    /*=======================================================================
      (letrec (<binding spec>*) <body>)

      <binding spec> --> (<variable> <expression>)
      <body> --> <definition>* <sequence>
      <definition> --> (define <variable> <expression>)
            | (define (<variable> <def formals>) <body>)
            | (begin <definition>*)
      <sequence> --> <command>* <expression>
      <command> --> <expression>
    =======================================================================*/

    /* extend env by placeholder frame for subsequent lambda evaluations */
    env = scm_extend_environment(SCM_NULL, SCM_NULL, eval_state->env);

    formals = actuals = SCM_NULL;
    FOR_EACH (binding, bindings) {
        if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
            ERR_OBJ(ERRMSG_INVALID_BINDING, binding);
#if SCM_STRICT_ARGCHECK
        /* Optional check. Keeping variable name unique is user's
         * responsibility. R5RS: "It is an error for a <variable> to appear
         * more than once in the list of variables being bound." */
        if (TRUEP(scm_p_memq(var, formals)))
            ERR_OBJ(ERRMSG_DUPLICATE_VARNAME, var);
#endif
        exp = CADR(binding);
        val = EVAL(exp, env);
        if (SYNTAXP(val))
            ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, exp);

        /* construct formals and actuals list: any <init> must not refer a
         * <variable> at this time */
        formals = CONS(var, formals);
        actuals = CONS(val, actuals);
    }
    if (!NULLP(bindings))
        ERR_OBJ(ERRMSG_INVALID_BINDINGS, bindings);

    /* fill the placeholder frame */
    eval_state->env = scm_replace_environment(formals, actuals, env);

    return scm_s_body(body, eval_state);
}

/*
 * Valid placement for definitions
 *
 * Definitions on SigScheme is strictly conformed to the three rule specified
 * in R5RS (see below), when SCM_USE_INTERNAL_DEFINITIONS is enabled. All
 * conditions that are not specified by the rules cause syntax error.
 *
 * 5.2 Definitions
 *
 * Definitions are valid in some, but not all, contexts where expressions are
 * allowed. They are valid only at the top level of a <program> and at the
 * beginning of a <body>.
 *
 * 5.2.2 Internal definitions
 *
 * Definitions may occur at the beginning of a <body> (that is, the body of a
 * lambda, let, let*, letrec, let-syntax, or letrec-syntax expression or that
 * of a definition of an appropriate form).
 *
 * Wherever an internal definition may occur (begin <definition1> ...) is
 * equivalent to the sequence of definitions that form the body of the begin.
 *
 * 7.1.6 Programs and definitions
 *
 * <definition> --> (define <variable> <expression>)
 *       | (define (<variable> <def formals>) <body>)
 *       | (begin <definition>*)
 */

#if SCM_USE_INTERNAL_DEFINITIONS
static ScmObj
filter_definitions(ScmObj body, ScmObj *formals, ScmObj *actuals,
                   ScmQueue *def_expq)
{
    ScmObj exp, var, sym, begin_rest, lambda_formals, lambda_body;
    DECLARE_INTERNAL_FUNCTION("(body)");

    for (; CONSP(body); POP(body)) {
        exp = CAR(body);
        if (!CONSP(exp))
            break;
        sym = POP(exp);
        if (EQ(sym, l_sym_begin)) {
            begin_rest = filter_definitions(exp, formals, actuals, def_expq);
            if (!NULLP(begin_rest)) {
                /* no definitions found */
                if (begin_rest == exp)
                    return body;

                ERR_OBJ("definitions and expressions intermixed", CAR(body));
            }
            /* '(begin)' is a valid R5RS definition form */
        } else if (EQ(sym, l_sym_define)) {
            var = MUST_POP_ARG(exp);
            if (IDENTIFIERP(var)) {
                /* (define <variable> <expression>) */
                if (!LIST_1_P(exp))
                    ERR_OBJ(ERRMSG_BAD_DEFINE_FORM, CAR(body));
                exp = CAR(exp);
            } else if (CONSP(var)) {
                /* (define (<variable> . <formals>) <body>) */
                sym            = CAR(var);
                lambda_formals = CDR(var);
                lambda_body    = exp;

                ENSURE_SYMBOL(sym);
                var = sym;
                exp = CONS(l_syn_lambda, CONS(lambda_formals, lambda_body));
            } else {
                ERR_OBJ(ERRMSG_BAD_DEFINE_FORM, CAR(body));
            }
            *formals = CONS(var, *formals);
            *actuals = CONS(SCM_UNBOUND, *actuals);
            SCM_QUEUE_ADD(*def_expq, exp);
        } else {
            break;
        }
    }

    return body;
}

/* <body> part of let, let*, letrec and lambda. This function performs strict
 * form validation for internal definitions as specified in R5RS ("5.2.2
 * Internal definitions" and "7.1.6 Programs and definitions"). */
/* TODO: Introduce compilation phase and reorganize into compile-time syntax
 * transformer */
SCM_EXPORT ScmObj
scm_s_body(ScmObj body, ScmEvalState *eval_state)
{
    ScmQueue def_expq;
    ScmObj env, formals, actuals, def_exps, exp, val;
    DECLARE_INTERNAL_FUNCTION("(body)" /* , syntax_variadic_tailrec_0 */);

    if (CONSP(body)) {
        /* collect internal definitions */
        def_exps = formals = actuals = SCM_NULL;
        SCM_QUEUE_POINT_TO(def_expq, def_exps);
        body = filter_definitions(body, &formals, &actuals, &def_expq);

        if (!NULLP(def_exps)) {
            /* extend env with the unbound variables */
            env = scm_extend_environment(formals, actuals, eval_state->env);

            /* eval the definitions and fill the variables with the results as
             * if letrec */
            actuals = SCM_NULL;
            FOR_EACH (exp, def_exps) {
                val = EVAL(exp, env);
                if (SYNTAXP(val))
                    ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, exp);
                actuals = CONS(val, actuals);
            }
            eval_state->env = scm_update_environment(actuals, env);
        }
    }
    /* eval rest of the body */
    return scm_s_begin(body, eval_state);
}
#endif /* SCM_USE_INTERNAL_DEFINITIONS */

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr, env;
    DECLARE_FUNCTION("begin", syntax_variadic_tailrec_0);

    if (SCM_DEFINABLE_TOPLEVELP(eval_state)) {
        if (!CONSP(args)) {
            /* '(begin)' */
            ASSERT_NO_MORE_ARG(args);
            eval_state->ret_type = SCM_VALTYPE_AS_IS;
            return SCM_UNDEF;
        }
        env = eval_state->env;
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
        eval_state->nest = SCM_NEST_RETTYPE_BEGIN;
#endif
    } else {
        if (!CONSP(args))
            ERR(ERRMSG_EXPRESSION_REQUIRED);
        env = FORBID_TOPLEVEL_DEFINITIONS(eval_state->env);
    }

    FOR_EACH_BUTLAST (expr, args)
        EVAL(expr, env);
    ASSERT_NO_MORE_ARG(args);

    return expr;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.4 Iteration
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_do(ScmObj bindings, ScmObj test_exps, ScmObj commands,
         ScmEvalState *eval_state)
{
    ScmQueue stepq;
    ScmObj env, orig_env, rest, rest_commands, val, termp;
    ScmObj formals, actuals, steps;
    ScmObj binding, var, init, step;
    ScmObj test, exps, command;
    DECLARE_FUNCTION("do", syntax_variadic_tailrec_2);

    orig_env = eval_state->env;

    /*
     * (do ((<variable1> <init1> <step1>)
     *      (<variable2> <init2> <step2>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     */

    /* extract bindings ((<variable> <init> <step>) ...) */
    env = FORBID_TOPLEVEL_DEFINITIONS(orig_env);
    formals = actuals = steps = SCM_NULL;
    SCM_QUEUE_POINT_TO(stepq, steps);
    rest = bindings;
    FOR_EACH (binding, rest) {
        if (!CONSP(binding))
            goto err;
        var  = POP(binding);
        ENSURE_SYMBOL(var);
#if SCM_STRICT_ARGCHECK
        /* Optional check. Keeping variable name unique is user's
         * responsibility. R5RS: "It is an error for a <variable> to appear
         * more than once in the list of `do' variables.". */
        if (TRUEP(scm_p_memq(var, formals)))
            ERR_OBJ(ERRMSG_DUPLICATE_VARNAME, var);
#endif

        if (!CONSP(binding))
            goto err;
        init = POP(binding);

        step = (CONSP(binding)) ? POP(binding) : var;
        if (!NULLP(binding))
            goto err;

        init = EVAL(init, env);
        if (SYNTAXP(init))
            ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, init);
        formals = CONS(var, formals);
        actuals = CONS(init, actuals);
        SCM_QUEUE_ADD(stepq, step);
    }
    if (!NULLP(rest))
        goto err;

    /* (<test> <expression> ...) */
    if (!CONSP(test_exps))
        ERR_OBJ("invalid test form", test_exps);
    test = CAR(test_exps);
    exps = CDR(test_exps);

    /* iteration phase */
    rest_commands = commands;
    /* extend env by <init>s */
    env = scm_extend_environment(formals, actuals, orig_env);
    while (termp = EVAL(test, env), FALSEP(termp)) {
        rest_commands = commands;
        FOR_EACH (command, rest_commands)
            EVAL(command, env);
        ASSERT_NO_MORE_ARG(rest_commands);

        /* Update variables by <step>s: <step>s evaluation must be isolated
         * from the env for the next iteration. */
        actuals = SCM_NULL;
        rest = steps;
        FOR_EACH (step, rest) {
            val = EVAL(step, env);
            if (SYNTAXP(val))
                ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, val);
            actuals = CONS(val, actuals);
        }
        /* the envs for each iteration must be isolated and not be
         * overwritten */
        env = scm_extend_environment(formals, actuals, orig_env);
    }
#if SCM_STRICT_ARGCHECK
    /* no iteration occurred */
    if (rest_commands == commands)
        ENSURE_PROPER_ARG_LIST(commands);
#endif

    /* R5RS: If no <expression>s are present, then the value of the `do'
     * expression is unspecified. */
    eval_state->env = env;
    if (NULLP(exps)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_UNDEF;
    } else {
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
        eval_state->nest = SCM_NEST_COMMAND;
#endif
        return scm_s_begin(exps, eval_state);
    }

 err:
    ERR_OBJ(ERRMSG_INVALID_BINDINGS, bindings);
    /* NOTREACHED */
    return SCM_FALSE;
}

/*=======================================
  R5RS : 5.2 Definitions
=======================================*/
static void
define_internal(ScmObj var, ScmObj exp, ScmObj env)
{
    ScmObj val;
    DECLARE_INTERNAL_FUNCTION("define");

    SCM_ASSERT(SYMBOLP(var) || SYMBOLP(SCM_FARSYMBOL_SYM(var)));
    var = SCM_UNWRAP_KEYWORD(var);
    val = EVAL(exp, env);
    if (SYNTAXP(val))
        ERR_OBJ(ERRMSG_SYNTAX_AS_VALUE, exp);
    SCM_SYMBOL_SET_VCELL(var, val);
}

/* To test ScmNestState, scm_s_define() needs eval_state although this is not a
 * tail-recursive syntax */
SCM_EXPORT ScmObj
scm_s_define(ScmObj var, ScmObj rest, ScmEvalState *eval_state)
{
    ScmObj procname, body, formals, proc, env;
    DECLARE_FUNCTION("define", syntax_variadic_tailrec_1);

    /* internal definitions are handled as a virtual letrec in
     * scm_s_body() */
    if (!SCM_DEFINABLE_TOPLEVELP(eval_state)) {
#if SCM_STRICT_TOPLEVEL_DEFINITIONS
        if (scm_toplevel_environmentp(eval_state->env))
            ERR_OBJ("toplevel definition is not allowed here", var);
        else
#endif
            ERR_OBJ(ERRMSG_BAD_DEFINE_PLACEMENT, var);
    }
    env = eval_state->env;

    /*=======================================================================
      (define <variable> <expression>)
    =======================================================================*/
    if (IDENTIFIERP(var)) {
        if (!LIST_1_P(rest))
            goto err;

        define_internal(var, CAR(rest), env);
    }

    /*=======================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda <formals> <body>))
    =======================================================================*/
    else if (CONSP(var)) {
        procname = CAR(var);
        formals  = CDR(var);
        body     = rest;

        ENSURE_SYMBOL(procname);
        proc = scm_s_lambda(formals, body, env);
        define_internal(procname, proc, env);
    } else {
        goto err;
    }

    eval_state->ret_type = SCM_VALTYPE_AS_IS;
#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return var;
#endif

 err:
    ERR_OBJ(ERRMSG_BAD_DEFINE_FORM,
            CONS(l_sym_define, CONS(var, rest)));
    /* NOTREACHED */
    return SCM_FALSE;
}
