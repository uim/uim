/*===========================================================================
 *  FileName : syntax.c
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

#include "config.h"

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
#define ERRMSG_BAD_SPLICE_LIST      "bad splice list"
#define ERRMSG_BAD_DEFINE_PLACEMENT "bad define placement"

/*=======================================
  Variable Declarations
=======================================*/
#include "functable-r5rs-syntax.c"

SCM_DEFINE_EXPORTED_VARS(syntax);

SCM_GLOBAL_VARS_BEGIN(static_syntax);
#define static
static ScmObj l_sym_else, l_sym_yields;
#if SCM_STRICT_DEFINE_PLACEMENT
static ScmObj l_sym_define, l_sym_begin, l_syn_lambda;
#endif /* SCM_STRICT_DEFINE_PLACEMENT */
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
#if SCM_STRICT_DEFINE_PLACEMENT
static ScmObj filter_definitions(ScmObj body, ScmObj *formals, ScmObj *actuals,
                                 ScmQueue *def_expq);
#endif
static void define_internal(ScmObj var, ScmObj exp, ScmObj env);

/* Quasiquotation. */
static tr_param qquote_internal(ScmObj input, ScmObj env, scm_int_t nest);

/*=======================================
  Function Implementations
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

    l_sym_else   = scm_intern("else");
    l_sym_yields = scm_intern("=>");
#if SCM_STRICT_DEFINE_PLACEMENT
    l_sym_define = scm_intern("define");
    l_sym_begin  = scm_intern("begin");
    scm_gc_protect_with_init(&l_syn_lambda,
                             scm_symbol_value(scm_intern("lambda"),
                                              SCM_INTERACTION_ENV));
#endif
}

/*===========================================================================
  Utilities: Sequential Datum Translators
===========================================================================*/

#define RETURN_OBJECT(o)                        \
    do {                                        \
        translator_ret _ret;                    \
        _ret.object = (o);                      \
        return _ret;                            \
    } while (0)
#define RETURN_BOOLEAN(b)                       \
    do {                                        \
        translator_ret _ret;                    \
        _ret.boolean = (b);                     \
        return _ret;                            \
    } while (0)
/**
 * Performs (relatively) complex operations on a list translator.
 *
 * @see list_translator, tr_msg
 */
SCM_EXPORT translator_ret
scm_listran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    DECLARE_INTERNAL_FUNCTION("(list translator)");

    switch (msg) {
    case TR_MSG_NOP: /* for better performance */
        break;

    case TR_MSG_ENDP:
        RETURN_BOOLEAN(TRL_ENDP(*t));

    case TR_MSG_GET_ELM:
        RETURN_OBJECT(TRL_GET_ELM(*t));

    case TR_MSG_NEXT:
        TRL_NEXT(*t);
        break;

    case TR_MSG_REPLACE:
        obj = LIST_1(obj);
        /* Fall through. */
    case TRL_MSG_SET_SUBLS:
    case TR_MSG_SPLICE:

        /* Execute deferred copies. */
        while (!EQ(t->u.lst.src, t->u.lst.cur)) {
            SCM_QUEUE_ADD(t->u.lst.q, CAR(t->u.lst.src));
            t->u.lst.src = CDR(t->u.lst.src);
        }

        if (msg != TRL_MSG_SET_SUBLS) {
            SCM_QUEUE_APPEND(t->u.lst.q, obj);
#if SCM_STRICT_R5RS
            if (!NULLP(SCM_QUEUE_TERMINATOR(t->u.lst.q)))
                ERR_OBJ(ERRMSG_BAD_SPLICE_LIST, obj);
#endif
            t->u.lst.src = obj = CDR(t->u.lst.cur);
        }
        SCM_QUEUE_SLOPPY_APPEND(t->u.lst.q, obj);
        break;

    case TR_MSG_EXTRACT:
        RETURN_OBJECT(TRL_EXTRACT(*t));

    default:
        SCM_ASSERT(scm_false);
    }
    RETURN_OBJECT(SCM_INVALID);
}

#define REPLACED_INDEX(i) (i)
/* '- 1' allows zero as spliced index */
#define SPLICED_INDEX(i)  (-(i) - 1)

SCM_EXPORT translator_ret
scm_vectran(sequence_translator *t, tr_msg msg, ScmObj obj)
{
    scm_int_t splice_len;
    scm_int_t change_index;
    DECLARE_INTERNAL_FUNCTION("(vector translator)");

    switch (msg) {
    case TR_MSG_NOP: /* for better performance */
        break;

    case TR_MSG_GET_ELM:
        RETURN_OBJECT(TRV_GET_ELM(*t));

    case TR_MSG_NEXT:
        TRV_NEXT(*t);
        break;

    case TR_MSG_ENDP:
        RETURN_BOOLEAN(TRV_ENDP(*t));

    case TR_MSG_SPLICE:
        splice_len = scm_length(obj);
#if SCM_STRICT_R5RS
        if (!SCM_LISTLEN_PROPERP(splice_len))
            ERR_OBJ(ERRMSG_BAD_SPLICE_LIST, obj);
#endif
        t->u.vec.growth += splice_len - 1;
        change_index = SPLICED_INDEX(t->u.vec.index);
        goto record_change;

    case TR_MSG_REPLACE:
        change_index = REPLACED_INDEX(t->u.vec.index);

      record_change:
        SCM_QUEUE_ADD(t->u.vec.q, CONS(MAKE_INT(change_index), obj));
        break;

    case TR_MSG_EXTRACT:
        /* Create a new vector iff modifications have been recorded. */
        if (!NULLP(t->u.vec.diff)) {
            ScmObj *copy_buf, *src_buf;
            ScmObj diff, appendix, elm, ret;
            scm_int_t src_len, i, cpi;

            src_len = SCM_VECTOR_LEN(t->u.vec.src);
            src_buf = SCM_VECTOR_VEC(t->u.vec.src);
            copy_buf = scm_malloc((src_len + t->u.vec.growth)
                                  * sizeof(ScmObj));

            diff = t->u.vec.diff;
            change_index = SCM_INT_VALUE(CAAR(diff));
            for (i = cpi = 0; i < src_len; i++) {
                if (REPLACED_INDEX(i) == change_index) {
                    copy_buf[cpi++] = CDAR(diff);
                } else if (SPLICED_INDEX(i) == change_index) {
                    appendix = CDAR(diff);
                    FOR_EACH (elm, appendix)
                        copy_buf[cpi++] = elm;
                } else {
                    copy_buf[cpi++] = src_buf[i];
                    continue;
                }

                /* We replaced an element this round. */
                diff = CDR(diff);
                if (NULLP(diff))
                    /* Invalidate. */
                    change_index = src_len;
                else
                    change_index = SCM_INT_VALUE(CAAR(diff));
            }
            ret = MAKE_VECTOR(copy_buf, src_len + t->u.vec.growth);
            RETURN_OBJECT(ret);
        }
        RETURN_OBJECT(t->u.vec.src);

    default:
        SCM_ASSERT(scm_false);
    }
    RETURN_OBJECT(SCM_INVALID);
}

#undef REPLACED_INDEX
#undef SPLICED_INDEX
#undef RETURN_OBJECT
#undef RETURN_BOOLEAN

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
#else
    /* Crashless no-validation:
     * Regard any non-list object as symbol. Since the lookup operation search
     * for a variable by EQ, this is safe although loosely allows
     * R5RS-incompatible code. */
#endif
    if (!CONSP(body))
        ERR_OBJ("at least one expression required", body);

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

    if (test = EVAL(test, env), NFALSEP(test)) {
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
  R5RS : 4.1 Primitive expression types : 4.1.6 Assignment
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
    if (locally_bound == SCM_INVALID_REF) {
        /* Not found in the environment
           If symbol is not bound, error occurs */
        if (!SCM_SYMBOL_BOUNDP(sym))
            ERR_OBJ("unbound variable", sym);

        SCM_SYMBOL_SET_VCELL(sym, evaled);
    } else {
        /* found in the environment*/
        SET(locally_bound, evaled);
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
/* body of 'cond' and also invoked from 'case' and 'guard' of SRFI-34 */
SCM_EXPORT ScmObj
scm_s_cond_internal(ScmObj args, ScmObj case_key, ScmEvalState *eval_state)
{
    ScmObj env, clause, test, exps, proc;
    DECLARE_INTERNAL_FUNCTION("cond" /* , syntax_variadic_tailrec_0 */);

    env = eval_state->env;

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

    /* dirty hack to replace internal function name */
    if (VALIDP(case_key))
        SCM_MANGLE(name) = "case";

    if (NO_MORE_ARG(args))
        ERR("syntax error: at least one clause required");

    /* looping in each clause */
    FOR_EACH (clause, args) {
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

        if (VALIDP(case_key)) {
            test = SCM_UNWRAP_SYNTAX(test);
            if (EQ(test, l_sym_else)) {
                ASSERT_NO_MORE_ARG(args);
            } else {
                test = scm_p_memv(case_key, test);
                test = (NFALSEP(test)) ? case_key : SCM_FALSE;
            }
        } else if (EQ(test, l_sym_else)) {
            ASSERT_NO_MORE_ARG(args);
        } else {
            test = EVAL(test, env);
        }

        if (NFALSEP(test)) {
            /*
             * if the selected <clause> contains only the <test> and no
             * <expression>s, then the value of the <test> is returned as the
             * result.
             */
            if (NULLP(exps)) {
                if (EQ(test, l_sym_else)) {
                    ERR_OBJ("bad clause: else with no expressions", clause);
                } else {
                    eval_state->ret_type = SCM_VALTYPE_AS_IS;
                    return test;
                }
            }

            /*
             * Handle the case like follows.
             *
             * (case 1
             *   ((1) . 2))
             */
            if (!CONSP(exps))
                ERR_OBJ("bad dot clause", clause);

            /*
             * If the selected <clause> uses the => alternate form, then the
             * <expression> is evaluated. Its value must be a procedure that
             * accepts one argument; this procedure is then called on the value
             * of the <test> and the value returned by this procedure is
             * returned by the cond expression.
             */
            if (EQ(l_sym_yields, CAR(exps)) && CONSP(CDR(exps))
                && !EQ(test, l_sym_else))
            {
                if (!NULLP(CDDR(exps)))
                    ERR_OBJ("bad clause", clause);
                proc = EVAL(CADR(exps), env);
                if (!PROCEDUREP(proc))
                    ERR_OBJ("exp after => must be a procedure but got", proc);

                eval_state->ret_type = SCM_VALTYPE_AS_IS;
                return scm_call(proc, LIST_1(test));
            }

            return scm_s_begin(exps, eval_state);
        }
    }
    ASSERT_NO_MORE_ARG(args);

    /*
     * To distinguish unmatched status from SCM_UNDEF from a clause, pure
     * internal value SCM_INVALID is returned. Don't pass it to Scheme world.
     */
    eval_state->ret_type = SCM_VALTYPE_AS_IS;
    return SCM_INVALID;
}

SCM_EXPORT ScmObj
scm_s_cond(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("cond", syntax_variadic_tailrec_0);

    ret = scm_s_cond_internal(args, SCM_INVALID, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_s_case(ScmObj key, ScmObj clauses, ScmEvalState *eval_state)
{
    ScmObj ret;
    DECLARE_FUNCTION("case", syntax_variadic_tailrec_1);

    key = EVAL(key, eval_state->env);
    ret = scm_s_cond_internal(clauses, key, eval_state);
    return (VALIDP(ret)) ? ret : SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_s_and(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr, val;
    DECLARE_FUNCTION("and", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_TRUE;
    }

    FOR_EACH_BUTLAST (expr, args) {
        val = EVAL(expr, eval_state->env);
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
    ScmObj expr, val;
    DECLARE_FUNCTION("or", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_FALSE;
    }

    FOR_EACH_BUTLAST (expr, args) {
        val = EVAL(expr, eval_state->env);
        if (!FALSEP(val)) {
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
/*
 * Valid placement for definitions
 *
 * Definitions on SigScheme is strictly conformed to the three rule specified
 * in R5RS (see below), when SCM_STRICT_DEFINE_PLACEMENT is enabled. All
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
 */

#if SCM_STRICT_DEFINE_PLACEMENT
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
            if (CONSP(begin_rest))
                return CONS(CONS(l_sym_begin, begin_rest), CDR(body));
            ASSERT_NO_MORE_ARG(begin_rest);
        } else if (EQ(sym, l_sym_define)) {
            var = MUST_POP_ARG(exp);
            if (IDENTIFIERP(var)) {
                /* (define <variable> <expression>) */
                if (!LIST_1_P(exp))
                    ERR_OBJ("exactly 1 arg required but got", exp);
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
                ERR_OBJ("syntax error", var);
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
#endif

/* <body> part of let, let*, letrec and lambda. This function performs strict
 * form validation for internal definitions as specified in R5RS (5.2.2
 * Internal definitions). */
/* TODO: Reform as a read-time syntax translator */
SCM_EXPORT ScmObj
scm_s_body(ScmObj body, ScmEvalState *eval_state)
{
#if SCM_STRICT_DEFINE_PLACEMENT
    ScmQueue def_expq;
    ScmObj env, formals, actuals, def_exps, exp;
#endif
    DECLARE_INTERNAL_FUNCTION("(body)" /* , syntax_variadic_tailrec_0 */);

#if SCM_STRICT_DEFINE_PLACEMENT
    if (NO_MORE_ARG(body)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_UNDEF;
    }

    /* extend env by placeholder frame for subsequent internal definitions */
    env = scm_extend_environment(SCM_NULL, SCM_NULL, eval_state->env);

    /* collect internal definitions */
    def_exps = formals = actuals = SCM_NULL;
    SCM_QUEUE_POINT_TO(def_expq, def_exps);
    body = filter_definitions(body, &formals, &actuals, &def_expq);

    /* inject the unbound variables into the frame to make the variable
     * references invalid through the evaluation */
    env = scm_replace_environment(formals, actuals, env);

    /* eval the definitions and fill the placeholder frame with the results */
    actuals = SCM_NULL;
    FOR_EACH (exp, def_exps) {
        exp = EVAL(exp, env);
        actuals = CONS(exp, actuals);
    }
    eval_state->env = scm_update_environment(actuals, env);

    /* eval rest of the body */
#endif
    return scm_s_begin(body, eval_state);
}

/*
 * FIXME:
 * - Write the test for the named let spec:
 *   <init>s should be evaluated in an environment where <procname> is not
 *   bound to the closure.  <procname>'s scope must not penetrate to the
 *   surrounding environment.
 */
SCM_EXPORT ScmObj
scm_s_let(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj env, named_let_sym, proc, bindings, binding, body;
    ScmObj formals, var, actuals, val;
    ScmQueue varq, valq;
    DECLARE_FUNCTION("let", syntax_variadic_tailrec_0);

    env = eval_state->env;
    named_let_sym = SCM_FALSE;
    formals = SCM_NULL;
    actuals = SCM_NULL;

    /*=======================================================================
      normal let:

      (let <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    =======================================================================*/
    /*=======================================================================
      named let:

      (let <procname> <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    =======================================================================*/

    if (!CONSP(args))
        ERR("invalid form");
    bindings = POP(args);

    /* named let */
    if (IDENTIFIERP(bindings)) {
        named_let_sym = bindings;

        if (!CONSP(args))
            ERR("invalid named let form");
        bindings = POP(args);
    }

    body = args;

    SCM_QUEUE_POINT_TO(varq, formals);
    SCM_QUEUE_POINT_TO(valq, actuals);
    FOR_EACH (binding, bindings) {
#if SCM_COMPAT_SIOD_BUGS
        /* temporary solution. the inefficiency is not a problem */
        if (LIST_1_P(binding))
            binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

        if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
            ERR_OBJ("invalid binding form", binding);
        val = EVAL(CADR(binding), env);

        SCM_QUEUE_ADD(varq, var);
        SCM_QUEUE_ADD(valq, val);
    }
    if (!NULLP(bindings))
        ERR_OBJ("invalid bindings form", bindings);

    env = scm_extend_environment(formals, actuals, env);

    /* named let */
    if (IDENTIFIERP(named_let_sym)) {
        proc = MAKE_CLOSURE(CONS(formals, body), env);
        env = scm_add_environment(named_let_sym, proc, env);
    }

    eval_state->env = env;
    return scm_s_body(body, eval_state);
}

SCM_EXPORT ScmObj
scm_s_letstar(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj env, var, val, binding;
    DECLARE_FUNCTION("let*", syntax_variadic_tailrec_1);

    env = eval_state->env;

    /*=======================================================================
      (let* <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    =======================================================================*/

    if (CONSP(bindings)) {
        FOR_EACH (binding, bindings) {
#if SCM_COMPAT_SIOD_BUGS
            /* temporary solution. the inefficiency is not a problem */
            if (LIST_1_P(binding))
                binding = LIST_2(CAR(binding), SCM_FALSE);
#endif

            if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
                goto err;
            val = EVAL(CADR(binding), env);

            /* extend env for each variable */
            env = scm_extend_environment(LIST_1(var), LIST_1(val), env);
        }
        if (!NULLP(bindings))
            goto err;
    } else if (NULLP(bindings)) {
        env = scm_extend_environment(SCM_NULL, SCM_NULL, env);
    } else {
        goto err;
    }

    eval_state->env = env;

    return scm_s_body(body, eval_state);

 err:
    ERR_OBJ("invalid bindings form", bindings);
    /* NOTREACHED */
    return SCM_FALSE;
}

SCM_EXPORT ScmObj
scm_s_letrec(ScmObj bindings, ScmObj body, ScmEvalState *eval_state)
{
    ScmObj binding, formals, actuals, var, val;
    DECLARE_FUNCTION("letrec", syntax_variadic_tailrec_1);

    /*=======================================================================
      (letrec <bindings> <body>)
      <bindings> == ((<variable1> <init1>)
                     (<variable2> <init2>)
                     ...)
    =======================================================================*/

    if (!LISTP(bindings))
        goto err;

    /* extend env by placeholder frame for subsequent lambda evaluations */
    eval_state->env
        = scm_extend_environment(SCM_NULL, SCM_NULL, eval_state->env);

    formals = SCM_NULL;
    actuals = SCM_NULL;
    FOR_EACH (binding, bindings) {
        if (!LIST_2_P(binding) || !IDENTIFIERP(var = CAR(binding)))
            goto err;
        val = EVAL(CADR(binding), eval_state->env);

        /* construct formals and actuals list: any <init> must not refer a
         * <variable> at this time */
        formals = CONS(var, formals);
        actuals = CONS(val, actuals);
    }
    if (!NULLP(bindings))
        goto err;

    /* fill the placeholder frame */
    eval_state->env
        = scm_replace_environment(formals, actuals, eval_state->env);

    return scm_s_body(body, eval_state);

 err:
    ERR_OBJ("invalid bindings form", bindings);
    /* NOTREACHED */
    return SCM_FALSE;
}


/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.3 Sequencing
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_begin(ScmObj args, ScmEvalState *eval_state)
{
    ScmObj expr;
    DECLARE_FUNCTION("begin", syntax_variadic_tailrec_0);

    if (NO_MORE_ARG(args)) {
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return SCM_UNDEF;
    }

    FOR_EACH_BUTLAST (expr, args)
        EVAL(expr, eval_state->env);
    ASSERT_NO_MORE_ARG(args);

    /* Return tail expression. */
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
    ScmObj env, rest, rest_commands, val, termp;
    ScmObj formals, actuals, steps;
    ScmObj binding, var, init, step;
    ScmObj test, exps, command;
    DECLARE_FUNCTION("do", syntax_variadic_tailrec_2);

    env = eval_state->env;

    /*
     * (do ((<variable1> <init1> <step1>)
     *      (<variable2> <init2> <step2>)
     *      ...)
     *     (<test> <expression> ...)
     *   <command> ...)
     */

    /* extract bindings ((<variable> <init> <step>) ...) */
    formals = actuals = steps = SCM_NULL;
    SCM_QUEUE_POINT_TO(stepq, steps);
    rest = bindings;
    FOR_EACH (binding, rest) {
        if (!CONSP(binding))
            goto err;
        var  = POP(binding);
        ENSURE_SYMBOL(var);
        /* R5RS: It is an error for a <variable> to appear more than once in
         * the list of `do' variables. */
        if (NFALSEP(scm_p_memq(var, formals)))
            ERR_OBJ("duplicate variable", var);

        if (!CONSP(binding))
            goto err;
        init = POP(binding);

        step = (CONSP(binding)) ? POP(binding) : var;
        if (!NULLP(binding))
            goto err;

        init = EVAL(init, env);
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
    env = scm_extend_environment(formals, actuals, env);
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
            actuals = CONS(val, actuals);
        }
#if SCM_STRICT_DEFINE_PLACEMENT
        env = scm_update_environment(actuals, env);
#else
        /* silently discards new bindings from invalid internal definitions */
        env = scm_replace_environment(formals, actuals, env);
#endif
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
        return scm_s_begin(exps, eval_state);
    }

 err:
    ERR_OBJ("invalid bindings form", bindings);
    /* NOTREACHED */
    return SCM_FALSE;
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.5 Delayed evaluation
===========================================================================*/
SCM_EXPORT ScmObj
scm_s_delay(ScmObj expr, ScmObj env)
{
    DECLARE_FUNCTION("delay", syntax_fixed_1);

    /* (lambda () exp) */
    return MAKE_CLOSURE(SCM_LIST_2(SCM_NULL, expr), env);
}

/*===========================================================================
  R5RS : 4.2 Derived expression types : 4.2.6 Quasiquotation
===========================================================================*/

/**
 * Interpret a quasiquoted expression.
 */
static tr_param
qquote_internal(ScmObj input, ScmObj env, scm_int_t nest)
{
    ScmObj obj, form, args;
    sequence_translator tr;
    tr_param tmp_result;
    tr_param my_result;
    DECLARE_INTERNAL_FUNCTION("quasiquote");

    if (VECTORP(input)) {
        for (TRV_INIT(tr, input); !TRV_ENDP(tr); TRV_NEXT(tr)) {
            obj = TRV_GET_ELM(tr);
            tmp_result = qquote_internal(obj, env, nest);
            TRV_EXECUTE(tr, tmp_result);
        }
    } else if (CONSP(input)) {
        /* This implementation adopt "minimum mercy" interpretation depending
         * on the R5RS specification cited below, to simplify the code.
         *
         * 4.2.6 Quasiquotation
         * Unpredictable behavior can result if any of the symbols quasiquote,
         * unquote, or unquote-splicing appear in positions within a <qq
         * template> otherwise than as described above. */
        for (TRL_INIT(tr, input); !TRL_ENDP(tr); TRL_NEXT(tr)) {
            ScmObj unwrapped;
            form = TRL_GET_SUBLS(tr);
            obj = CAR(form);
            unwrapped = SCM_UNWRAP_KEYWORD(obj);
            if (EQ(unwrapped, SYM_QUASIQUOTE)) {
                /* FORM == `x */
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid quasiquote form", form);

                ++nest;
            } else if (EQ(unwrapped, SYM_UNQUOTE)) {
                /* FORM == ,x */
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid unquote form", form);

                if (--nest == 0) {
                    form = TRL_GET_SUBLS(tr);
                    obj = EVAL(CAR(args), env);
                    TRL_SET_SUBLS(tr, obj);
                    my_result.obj = TRL_EXTRACT(tr);
                    my_result.msg = TR_MSG_REPLACE;
                    return my_result;
                }
            } else if (EQ(unwrapped, SYM_UNQUOTE_SPLICING)) {
                /* FORM == ,@x */
                if (!EQ(form, input)) /* (a . ,@b) */
                    ERR_OBJ(",@ in invalid context", input);
                if (args = CDR(form), !LIST_1_P(args))
                    ERR_OBJ("invalid unquote-splicing form", form);

                if (--nest == 0) {
                    /* R5RS: 4.2.6 Quasiquotation
                     * If a comma appears followed immediately by an
                     * at-sign (@), then the following expression must
                     * evaluate to a list */
                    obj = EVAL(CAR(args), env);
                    if (!LISTP(obj))
                        ERR(",@<x> must evaluate to a list");

                    my_result.obj = obj;
                    my_result.msg = TR_MSG_SPLICE;
                    return my_result;
                }
            }
            tmp_result = qquote_internal(obj, env, nest);
            TRL_EXECUTE(tr, tmp_result);
        }
        /* Interpret the tail if an improper list. */
        if (!NULLP(TRL_GET_SUBLS(tr))) {
            tmp_result = qquote_internal(TRL_GET_SUBLS(tr), env, nest);
            if (tmp_result.msg != TR_MSG_NOP)
                TRL_SET_SUBLS(tr, tmp_result.obj);
        }
    } else {
        /* An atomic datum. */
#if SCM_USE_HYGIENIC_MACRO
        if (FARSYMBOLP(input)) {
            tmp_result.obj = SCM_UNWRAP_SYNTAX(input);
            tmp_result.msg = TR_MSG_REPLACE;
            return tmp_result;
        }
#endif
        tmp_result.obj = SCM_INVALID;
        tmp_result.msg = TR_MSG_NOP;
        return tmp_result;
    }

    my_result.obj = TR_EXTRACT(tr);
    my_result.msg = EQ(my_result.obj, input) ? TR_MSG_NOP : TR_MSG_REPLACE;
    return my_result;
}

SCM_EXPORT ScmObj
scm_s_quasiquote(ScmObj datum, ScmObj env)
{
    tr_param ret;
    DECLARE_FUNCTION("quasiquote", syntax_fixed_1);

    ret = qquote_internal(datum, env, 1);

    switch (ret.msg) {
    case TR_MSG_NOP:
        return datum;
    case TR_MSG_SPLICE:
        /* R5RS: 4.2.6 Quasiquotation
         * A comma at-sign should only appear within a list or vector <qq
         * template>. */
        ERR_OBJ(",@ in invalid context", datum);
        /* NOTREACHED */
    case TR_MSG_REPLACE:
        return ret.obj;
    default:
        SCM_ASSERT(scm_false);
    }
}

SCM_EXPORT ScmObj
scm_s_unquote(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote", syntax_fixed_1);

    ERR("unquote outside quasiquote");
    /* NOTREACHED */
    return SCM_FALSE;
}

SCM_EXPORT ScmObj
scm_s_unquote_splicing(ScmObj dummy, ScmObj env)
{
    DECLARE_FUNCTION("unquote-splicing", syntax_fixed_1);

    ERR("unquote-splicing outside quasiquote");
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

    val = EVAL(exp, env);
    if (scm_toplevel_environmentp(env)) {
        SCM_ASSERT(SYMBOLP(var) || SYMBOLP(SCM_FARSYMBOL_SYM(var)));
        SCM_SYMBOL_SET_VCELL(SCM_UNWRAP_KEYWORD(var), val);
    } else {
#if SCM_STRICT_DEFINE_PLACEMENT
        /* internal definitions are handled as a virtual letrec in
         * scm_s_body() */
        PLAIN_ERR(ERRMSG_BAD_DEFINE_PLACEMENT);
#else
        env = scm_add_environment(var, val, env);
#endif
    }
}

SCM_EXPORT ScmObj
scm_s_define(ScmObj var, ScmObj rest, ScmObj env)
{
    ScmObj procname, body, formals, proc;
    DECLARE_FUNCTION("define", syntax_variadic_1);

    /*=======================================================================
      (define <variable> <expression>)
    =======================================================================*/
    if (IDENTIFIERP(var)) {
        if (!LIST_1_P(rest))
            ERR_OBJ("exactly 1 arg required but got", rest);

        define_internal(var, CAR(rest), env);
    }

    /*=======================================================================
      (define (<variable> . <formals>) <body>)

      => (define <variable>
             (lambda (<formals>) <body>))
    =======================================================================*/
    else if (CONSP(var)) {
        procname = CAR(var);
        formals  = CDR(var);
        body     = rest;

        ENSURE_SYMBOL(procname);
        proc = scm_s_lambda(formals, body, env);
        define_internal(procname, proc, env);
    } else {
        ERR_OBJ("syntax error", var);
    }

#if SCM_STRICT_R5RS
    return SCM_UNDEF;
#else
    return var;
#endif
}
