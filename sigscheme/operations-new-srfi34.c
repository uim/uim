/*===========================================================================
 *  FileName : operations-new-srfi34.c
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
#define ERRMSG_UNHANDLED_EXCEPTION "unhandled exception"
#define ERRMSG_HANDLER_RETURNED    "handler returned"

#define MAKE_STR_COPYING Scm_NewStringCopying
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
static ScmObj guard_internal(ScmObj guard_k, ScmObj env);
static ScmObj guard_handler(ScmObj q_condition, ScmEvalState *eval_state);
static ScmObj guard_handler_body(ScmObj handler_k, ScmObj env);
static ScmObj guard_body(ScmEvalState *eval_state);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize_SRFI34(void)
{
    ScmObj *const *var;

    Scm_use("srfi-23");

    /* protect global variables */
    for (var = &global_var_list[0]; *var; var++) {
        **var = SCM_FALSE;
        SigScm_GC_Protect(*var);
    }

    errmsg_unhandled_exception = MAKE_STR_COPYING(ERRMSG_UNHANDLED_EXCEPTION);
    errmsg_handler_returned    = MAKE_STR_COPYING(ERRMSG_HANDLER_RETURNED);

    sym_error      = Scm_Intern("error");
    sym_raise      = Scm_Intern("raise");

    sym_lex_env    = Scm_Intern("lex-env");
    sym_cond_catch = Scm_Intern("cond-catch");
    sym_body       = Scm_Intern("body");
    sym_condition  = Scm_Intern("condition");
    sym_guard_k    = Scm_Intern("guard-k");
    sym_handler_k  = Scm_Intern("handler-k");

    /* prepare procedures and syntaxes */
    syn_apply   = Scm_SymbolValue(Scm_Intern("apply"),  SCM_INTERACTION_ENV);
    proc_values = Scm_SymbolValue(Scm_Intern("values"), SCM_INTERACTION_ENV);
    /* FIXME: make registration type-safe */
    syn_set_cur_handlers = Scm_NewFunc(SCM_SYNTAX_FIXED | 1,
                                       &set_cur_handlers);
    proc_with_exception_handlers = Scm_NewFunc(SCM_PROCEDURE_FIXED | 2,
                                               &with_exception_handlers);
    syn_guard_internal = Scm_NewFunc(SCM_SYNTAX_FIXED | 1,
                                     &guard_internal);
    syn_guard_handler = Scm_NewFunc(SCM_SYNTAX_FIXED_TAIL_REC | 1,
                                    &guard_handler);
    syn_guard_handler_body = Scm_NewFunc(SCM_SYNTAX_FIXED | 1,
                                         &guard_handler_body);
    syn_guard_body = Scm_NewFunc(SCM_SYNTAX_FIXED_TAIL_REC | 0,
                                 &guard_body);

    /*
     * The 'error' procedure should not be invoked directly by ScmOp_error(),
     * to allow dynamic redifinition, and keep SRFI-23 implementation abstract.
     */
    proc_fallback_handler
        = ScmExp_lambda(LIST_1(sym_condition),
                        LIST_1(LIST_3(sym_error,
                                      errmsg_unhandled_exception,
                                      sym_condition)),
                        SCM_INTERACTION_ENV);

    REGISTER_FUNC_TABLE(scm_new_srfi34_func_info_table);

    current_exception_handlers = LIST_1(proc_fallback_handler);
}

static ScmObj set_cur_handlers(ScmObj handlers, ScmObj env)
{
    DECLARE_PRIVATE_FUNCTION("with_exception_handlers", SyntaxFixed1);

    current_exception_handlers = handlers;
    return SCM_UNDEF;
}

static ScmObj with_exception_handlers(ScmObj new_handlers, ScmObj thunk)
{
    ScmObj prev_handlers, before, after;
    DECLARE_PRIVATE_FUNCTION("with_exception_handlers", ProcedureFixed2);

    prev_handlers = current_exception_handlers;
    before = ScmExp_lambda(SCM_NULL,
                           LIST_1(LIST_2(syn_set_cur_handlers, new_handlers)),
                           SCM_INTERACTION_ENV);
    after = ScmExp_lambda(SCM_NULL,
                          LIST_1(LIST_2(syn_set_cur_handlers, prev_handlers)),
                          SCM_INTERACTION_ENV);
    return ScmOp_dynamic_wind(before, thunk, after);
}

/* with-exception-handler */

ScmObj ScmOp_SRFI34_with_exception_handler(ScmObj handler, ScmObj thunk)
{
    ScmObj handlers;
    DECLARE_FUNCTION("with-exception-handler", ProcedureFixed2);

    ASSERT_PROCEDUREP(handler);
    ASSERT_PROCEDUREP(thunk);

    handlers = CONS(handler, current_exception_handlers);
    return with_exception_handlers(handlers, thunk);
}

/* raise */

ScmObj ScmOp_SRFI34_raise(ScmObj obj)
{
    ScmObj handler, rest_handlers, thunk;
    DECLARE_FUNCTION("raise", ProcedureFixed1);

    handler = CAR(current_exception_handlers);
    rest_handlers = CDR(current_exception_handlers);
    thunk = ScmExp_lambda(SCM_NULL,
                          LIST_2(LIST_2(handler, obj),
                                 LIST_3(sym_error,
                                        errmsg_handler_returned, obj)),
                          SCM_INTERACTION_ENV);
    return with_exception_handlers(rest_handlers, thunk);
}

/* guard */

ScmObj ScmExp_SRFI34_guard(ScmObj cond_catch, ScmObj body,
                           ScmEvalState *eval_state)
{
    ScmObj lex_env, proc_guard_int, ret;
    DECLARE_FUNCTION("guard", SyntaxVariadicTailRec1);

    ASSERT_CONSP(cond_catch);
    ASSERT_CONSP(body);

    lex_env = eval_state->env;
    eval_state->env
        = Scm_ExtendEnvironment(LIST_3(sym_lex_env, sym_cond_catch, sym_body),
                                LIST_3(lex_env, cond_catch, body),
                                lex_env);
    proc_guard_int = ScmExp_lambda(LIST_1(sym_guard_k),
                                   LIST_1(LIST_2(syn_guard_internal, sym_guard_k)),
                                   eval_state->env);
    
    ret = Scm_CallWithCurrentContinuation(proc_guard_int, eval_state);
    eval_state->env      = lex_env;
    eval_state->ret_type = SCM_RETTYPE_AS_IS;
    return Scm_call(ret, SCM_NULL);
}

static ScmObj guard_internal(ScmObj q_guard_k, ScmObj env)
{
    ScmObj handler, body;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixed1);

    handler = ScmExp_lambda(LIST_1(sym_condition),
                            LIST_1(LIST_2(syn_guard_handler, sym_condition)),
                            env);
    body = ScmExp_lambda(SCM_NULL,
                         LIST_1(LIST_1(syn_guard_body)),
                         env);

    return ScmOp_SRFI34_with_exception_handler(handler, body);
}

static ScmObj guard_handler(ScmObj q_condition, ScmEvalState *eval_state)
{
    ScmObj handler_body, ret;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixedTailRec1);

    handler_body
        = ScmExp_lambda(LIST_1(sym_handler_k),
                        LIST_1(LIST_2(syn_guard_handler_body, sym_handler_k)),
                        eval_state->env);
    ret = Scm_CallWithCurrentContinuation(handler_body, eval_state);
    if (eval_state->ret_type == SCM_RETTYPE_NEED_EVAL) {
        ret = EVAL(ret, eval_state->env);
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
    }
    return Scm_call(ret, SCM_NULL);
}

/* assumes that ScmExp_delay() returns a closure */
static ScmObj guard_handler_body(ScmObj handler_k, ScmObj env)
{
    ScmEvalState eval_state;
    ScmObj lex_env, cond_env, condition, cond_catch, guard_k;
    ScmObj sym_var, clauses, caught, reraise, ret;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixed1);

    lex_env    = Scm_SymbolValue(sym_lex_env, env);
    condition  = Scm_SymbolValue(sym_condition, env);
    cond_catch = Scm_SymbolValue(sym_cond_catch, env);
    guard_k    = Scm_SymbolValue(sym_guard_k, env);

    /* eval cond-catch block */
    sym_var = CAR(cond_catch);
    clauses = CDR(cond_catch);
    ASSERT_SYMBOLP(sym_var);
    condition = EVAL(condition, lex_env);
    cond_env = Scm_ExtendEnvironment(LIST_1(sym_var),
                                     LIST_1(condition),
                                     lex_env);
    eval_state.env = cond_env;
    eval_state.ret_type = SCM_RETTYPE_NEED_EVAL;
    caught = ScmExp_cond_internal(clauses, &eval_state);

    if (VALIDP(caught)) {
        if (eval_state.ret_type == SCM_RETTYPE_NEED_EVAL)
            caught = EVAL(caught, cond_env);
        ret = ScmExp_delay(LIST_2(SYM_QUOTE, caught), cond_env);
        Scm_CallContinuation(guard_k, ret);
    } else {
        reraise = ScmExp_delay(LIST_2(sym_raise, LIST_2(SYM_QUOTE, condition)),
                               cond_env);
        Scm_CallContinuation(handler_k, reraise);
    }
    /* NOTREACHED */
    return SCM_UNDEF;
}

/* assumes that ScmExp_delay() returns a closure */
static ScmObj guard_body(ScmEvalState *eval_state)
{
    ScmEvalState lex_eval_state;
    ScmObj lex_env, guard_k, body, result, vals, ret;
    DECLARE_PRIVATE_FUNCTION("guard", SyntaxFixedTailRec0);

    lex_env = Scm_SymbolValue(sym_lex_env, eval_state->env);
    guard_k = Scm_SymbolValue(sym_guard_k, eval_state->env);
    body    = Scm_SymbolValue(sym_body,    eval_state->env);

    /* evaluate the body */
    lex_eval_state.env      = lex_env;
    lex_eval_state.ret_type = SCM_RETTYPE_NEED_EVAL;
    result = ScmExp_begin(body, &lex_eval_state);  /* always NEED_EVAL */
    result = EVAL(result, lex_env);
    eval_state->ret_type = SCM_RETTYPE_AS_IS;

    if (VALUEPACKETP(result)) {
        vals = SCM_VALUEPACKET_VALUES(result);
        ret = ScmExp_delay(LIST_3(syn_apply,
                                  proc_values, LIST_2(SYM_QUOTE, vals)),
                           lex_env);
    } else {
        ret = ScmExp_delay(LIST_2(SYM_QUOTE, result), lex_env);
    }
    Scm_CallContinuation(guard_k, ret);
    /* NOTREACHED */
    return SCM_UNDEF;
}
