/*===========================================================================
 *  FileName : operations-srfi34.c
 *  About    : New implementation of SRFI-34 exception handling for programs
 *
 *  Copyright (C) 2005      by YamaKen (yamaken AT bp.iij4u.or.jp)
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

/*
 * This file implements C-version of the reference implementation written in
 * the SRFI-34 specification. All parts are written in C since:
 *
 * - SigScheme doesn't have a hygienic-macros feature (yet)
 *
 * - To avoid namespace pollution (with-exception-handlers, guard-aux, etc),
 *   since SigScheme doesn't have a module or namespace feature (yet)
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
  File Local Macro Definitions
=======================================*/
#define USE_WITH_SIGSCHEME_FATAL_ERROR 1

#define ERRMSG_UNHANDLED_EXCEPTION "unhandled exception"
#define ERRMSG_HANDLER_RETURNED    "handler returned"
#define ERRMSG_FALLBACK_EXHAUSTED  "fallback handler exhausted"

#define DECLARE_PRIVATE_FUNCTION(func_name, type)                            \
    DECLARE_INTERNAL_FUNCTION(func_name)

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
static ScmObj current_exception_handlers;

/* error messages */
static ScmObj errmsg_unhandled_exception, errmsg_handler_returned;
static ScmObj errmsg_fallback_exhausted;

/* symbols */
static ScmObj sym_error, sym_raise;
static ScmObj sym_lex_env, sym_cond_catch, sym_body;
static ScmObj sym_condition, sym_guard_k, sym_handler_k;

/* procedures and syntaxes */
static ScmObj syn_apply, proc_values;
static ScmObj syn_set_cur_handlers, proc_fallback_handler;
static ScmObj proc_with_exception_handlers;
static ScmObj syn_guard_internal, syn_guard_handler, syn_guard_handler_body;
static ScmObj syn_guard_body;

static ScmObj *const global_var_list[] = {
    &current_exception_handlers,
    &errmsg_unhandled_exception, &errmsg_handler_returned,
    &errmsg_fallback_exhausted,
    &sym_error, &sym_raise,
    &sym_lex_env, &sym_cond_catch, &sym_body,
    &sym_condition, &sym_guard_k, &sym_handler_k,
    &syn_apply, &proc_values,
    &syn_set_cur_handlers, &proc_fallback_handler,
    &proc_with_exception_handlers,
    &syn_guard_internal, &syn_guard_handler, &syn_guard_handler_body,
    &syn_guard_body,
    NULL
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj set_cur_handlers(ScmObj handlers, ScmObj env);
static ScmObj with_exception_handlers(ScmObj new_handlers, ScmObj thunk);
static ScmObj guard_internal(ScmObj q_guard_k, ScmObj env);
static ScmObj guard_handler(ScmObj q_condition, ScmEvalState *eval_state);
static ScmObj delay(ScmObj evaled_obj, ScmObj env);
static ScmObj guard_handler_body(ScmObj q_handler_k, ScmObj env);
static ScmObj guard_body(ScmEvalState *eval_state);

/*=======================================
  Function Implementations
=======================================*/
void
scm_initialize_srfi34(void)
{
    ScmObj *const *var;

    scm_use("srfi-23");

    /* protect global variables */
    for (var = &global_var_list[0]; *var; var++)
        scm_gc_protect_with_init(*var, SCM_FALSE);

    errmsg_unhandled_exception
        = MAKE_IMMUTABLE_STRING_COPYING(ERRMSG_UNHANDLED_EXCEPTION);
    errmsg_handler_returned
        = MAKE_IMMUTABLE_STRING_COPYING(ERRMSG_HANDLER_RETURNED);
    errmsg_fallback_exhausted
        = MAKE_IMMUTABLE_STRING_COPYING(ERRMSG_FALLBACK_EXHAUSTED);

    sym_error      = scm_intern("error");
    sym_raise      = scm_intern("raise");

    sym_lex_env    = scm_intern("lex-env");
    sym_cond_catch = scm_intern("cond-catch");
    sym_body       = scm_intern("body");
    sym_condition  = scm_intern("condition");
    sym_guard_k    = scm_intern("guard-k");
    sym_handler_k  = scm_intern("handler-k");

    /* prepare procedures and syntaxes */
    syn_apply   = scm_symbol_value(scm_intern("apply"),  SCM_INTERACTION_ENV);
    proc_values = scm_symbol_value(scm_intern("values"), SCM_INTERACTION_ENV);
    /* FIXME: make registration type-safe */
    syn_set_cur_handlers = scm_make_func(SCM_SYNTAX_FIXED | 1,
                                       &set_cur_handlers);
    proc_with_exception_handlers = scm_make_func(SCM_PROCEDURE_FIXED | 2,
                                               &with_exception_handlers);
    syn_guard_internal = scm_make_func(SCM_SYNTAX_FIXED | 1,
                                     &guard_internal);
    syn_guard_handler = scm_make_func(SCM_SYNTAX_FIXED_TAIL_REC | 1,
                                    &guard_handler);
    syn_guard_handler_body = scm_make_func(SCM_SYNTAX_FIXED | 1,
                                         &guard_handler_body);
    syn_guard_body = scm_make_func(SCM_SYNTAX_FIXED_TAIL_REC | 0,
                                 &guard_body);

#if USE_WITH_SIGSCHEME_FATAL_ERROR
    proc_fallback_handler
        = scm_s_lambda(LIST_1(sym_condition),
                        LIST_1(LIST_4(scm_intern("if"),
                                      LIST_2(scm_intern("%%error-object?"),
                                             sym_condition),
                                      LIST_2(scm_intern("%%fatal-error"),
                                             sym_condition),
                                      LIST_3(sym_error,
                                             errmsg_unhandled_exception,
                                             sym_condition))),
                        SCM_INTERACTION_ENV);
#else /* USE_WITH_SIGSCHEME_FATAL_ERROR */
    /*
     * The 'error' procedure should not be invoked directly by
     * scm_p_srfi23_error(), to allow dynamic redifinition, and keep SRFI-23
     * implementation abstract.
     */
    proc_fallback_handler
        = scm_s_lambda(LIST_1(sym_condition),
                        LIST_1(LIST_3(sym_error,
                                      errmsg_unhandled_exception,
                                      sym_condition)),
                        SCM_INTERACTION_ENV);
#endif /* USE_WITH_SIGSCHEME_FATAL_ERROR */

    REGISTER_FUNC_TABLE(scm_new_srfi34_func_info_table);

    current_exception_handlers = LIST_1(proc_fallback_handler);
}

static ScmObj
set_cur_handlers(ScmObj handlers, ScmObj env)
{
    DECLARE_PRIVATE_FUNCTION("with_exception_handlers", SyntaxFixed1);

    current_exception_handlers = handlers;
    return SCM_UNDEF;
}

static ScmObj
with_exception_handlers(ScmObj new_handlers, ScmObj thunk)
{
    ScmObj prev_handlers, before, after;
    DECLARE_PRIVATE_FUNCTION("with_exception_handlers", ProcedureFixed2);

    prev_handlers = current_exception_handlers;
    before = scm_s_lambda(SCM_NULL,
                           LIST_1(LIST_2(syn_set_cur_handlers, new_handlers)),
                           SCM_INTERACTION_ENV);
    after = scm_s_lambda(SCM_NULL,
                          LIST_1(LIST_2(syn_set_cur_handlers, prev_handlers)),
                          SCM_INTERACTION_ENV);
    return scm_dynamic_wind(before, thunk, after);
}

/* with-exception-handler */

ScmObj
scm_p_srfi34_with_exception_handler(ScmObj handler, ScmObj thunk)
{
    ScmObj handlers;
    DECLARE_FUNCTION("with-exception-handler", procedure_fixed_2);

    ASSERT_PROCEDUREP(handler);
    ASSERT_PROCEDUREP(thunk);

    handlers = CONS(handler, current_exception_handlers);
    return with_exception_handlers(handlers, thunk);
}

/* raise */

ScmObj
scm_p_srfi34_raise(ScmObj obj)
{
    ScmObj handler, rest_handlers, thunk, err_obj;
    DECLARE_FUNCTION("raise", procedure_fixed_1);

    if (NULLP(current_exception_handlers)) {
        if (ERROBJP(obj))
            err_obj = obj;
        else
            err_obj = scm_make_error_obj(errmsg_fallback_exhausted, LIST_1(obj));
        scm_p_fatal_error(err_obj);
        /* NOTREACHED */
    }

    handler = CAR(current_exception_handlers);
    rest_handlers = CDR(current_exception_handlers);
    obj = LIST_2(SYM_QUOTE, obj);
    thunk = scm_s_lambda(SCM_NULL,
                          LIST_2(LIST_2(handler, obj),
                                 LIST_3(sym_error,
                                        errmsg_handler_returned, obj)),
                          SCM_INTERACTION_ENV);
    return with_exception_handlers(rest_handlers, thunk);
}

/* guard */

ScmObj scm_s_srfi34_guard(ScmObj cond_catch, ScmObj body,
                           ScmEvalState *eval_state)
{
    ScmObj lex_env, proc_guard_int, ret;
    DECLARE_FUNCTION("guard", syntax_variadic_tailrec_1);

    ASSERT_CONSP(cond_catch);
    ASSERT_CONSP(body);

    lex_env = eval_state->env;
    eval_state->env
        = scm_extend_environment(LIST_3(sym_lex_env, sym_cond_catch, sym_body),
                                LIST_3(lex_env, cond_catch, body),
                                lex_env);
    proc_guard_int = scm_s_lambda(LIST_1(sym_guard_k),
                                   LIST_1(LIST_2(syn_guard_internal, sym_guard_k)),
                                   eval_state->env);

    ret = scm_call_with_current_continuation(proc_guard_int, eval_state);
    eval_state->env      = lex_env;
    eval_state->ret_type = SCM_RETTYPE_AS_IS;
    return scm_call(ret, SCM_NULL);
}

static ScmObj
guard_internal(ScmObj q_guard_k, ScmObj env)
{
    ScmObj handler, body;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixed1);

    handler = scm_s_lambda(LIST_1(sym_condition),
                            LIST_1(LIST_2(syn_guard_handler, sym_condition)),
                            env);
    body = scm_s_lambda(SCM_NULL,
                         LIST_1(LIST_1(syn_guard_body)),
                         env);

    return scm_p_srfi34_with_exception_handler(handler, body);
}

static ScmObj
guard_handler(ScmObj q_condition, ScmEvalState *eval_state)
{
    ScmObj handler_body, ret;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixedTailRec1);

    handler_body
        = scm_s_lambda(LIST_1(sym_handler_k),
                        LIST_1(LIST_2(syn_guard_handler_body, sym_handler_k)),
                        eval_state->env);
    ret = scm_call_with_current_continuation(handler_body, eval_state);
    if (eval_state->ret_type == SCM_RETTYPE_NEED_EVAL) {
        ret = EVAL(ret, eval_state->env);
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
    }
    return scm_call(ret, SCM_NULL);
}

/* assumes that scm_s_delay() returns a closure */
static ScmObj
delay(ScmObj evaled_obj, ScmObj env)
{
    ScmObj vals;

    if (VALUEPACKETP(evaled_obj)) {
        vals = SCM_VALUEPACKET_VALUES(evaled_obj);
        return scm_s_delay(LIST_3(syn_apply,
                                   proc_values, LIST_2(SYM_QUOTE, vals)),
                            env);
    } else {
        return scm_s_delay(LIST_2(SYM_QUOTE, evaled_obj), env);
    }
}

/* assumes that scm_s_delay() returns a closure */
static ScmObj
guard_handler_body(ScmObj q_handler_k, ScmObj env)
{
    ScmEvalState eval_state;
    ScmObj lex_env, cond_env, condition, cond_catch, guard_k, handler_k;
    ScmObj sym_var, clauses, caught, reraise;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixed1);

    lex_env    = scm_symbol_value(sym_lex_env, env);
    condition  = scm_symbol_value(sym_condition, env);
    cond_catch = scm_symbol_value(sym_cond_catch, env);
    guard_k    = scm_symbol_value(sym_guard_k, env);
    handler_k  = EVAL(q_handler_k, env);

    /* eval cond-catch block */
    sym_var = CAR(cond_catch);
    clauses = CDR(cond_catch);
    ASSERT_SYMBOLP(sym_var);
    cond_env = scm_extend_environment(LIST_1(sym_var),
                                     LIST_1(condition),
                                     lex_env);
    eval_state.env = cond_env;
    eval_state.ret_type = SCM_RETTYPE_NEED_EVAL;
    caught = scm_s_cond_internal(clauses, SCM_INVALID, &eval_state);

    if (VALIDP(caught)) {
        if (eval_state.ret_type == SCM_RETTYPE_NEED_EVAL)
            caught = EVAL(caught, cond_env);
        scm_call_continuation(guard_k, delay(caught, cond_env));
    } else {
        reraise = scm_s_delay(LIST_2(sym_raise, LIST_2(SYM_QUOTE, condition)),
                               cond_env);
        scm_call_continuation(handler_k, reraise);
    }
    /* NOTREACHED */
    return SCM_UNDEF;
}

static ScmObj
guard_body(ScmEvalState *eval_state)
{
    ScmEvalState lex_eval_state;
    ScmObj lex_env, guard_k, body, result;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixedTailRec0);

    lex_env = scm_symbol_value(sym_lex_env, eval_state->env);
    guard_k = scm_symbol_value(sym_guard_k, eval_state->env);
    body    = scm_symbol_value(sym_body,    eval_state->env);

    /* evaluate the body */
    lex_eval_state.env      = lex_env;
    lex_eval_state.ret_type = SCM_RETTYPE_NEED_EVAL;
    result = scm_s_begin(body, &lex_eval_state);  /* always NEED_EVAL */
    result = EVAL(result, lex_env);
    eval_state->ret_type = SCM_RETTYPE_AS_IS;

    scm_call_continuation(guard_k, delay(result, lex_env));
    /* NOTREACHED */
    return SCM_UNDEF;
}
