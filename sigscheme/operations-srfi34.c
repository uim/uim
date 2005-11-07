/*===========================================================================
 *  FileName : operations-srfi34.c
 *  About    : Exception Handling for Programs
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

/*=======================================
  System Include
=======================================*/
#include <setjmp.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Declarations
=======================================*/
/* FIXME: make internal representation hidden */
#define CONTINUATION_FRAME(cont)                                             \
    ((struct continuation_frame *)SCM_CONTINUATION_OPAQUE(cont))
#define CONTINUATION_SET_FRAME             SCM_CONTINUATION_SET_OPAQUE
#define CONTINUATION_JMPENV(cont)          (CONTINUATION_FRAME(cont)->env)
#define CONTINUATION_SET_JMPENV(cont, env) (CONTINUATION_JMPENV(cont) = (env))
#define CONTINUATION_DYNEXT(cont)          (CONTINUATION_FRAME(cont)->dyn_ext)
#define CONTINUATION_SET_DYNEXT(cont, dyn_ext)                               \
    ((CONTINUATION_DYNEXT(cont)) = (dyn_ext))

/*=======================================
  File Local Type Definitions
=======================================*/
/* FIXME: make internal representation hidden */
struct continuation_frame {
    jmp_buf *env;
    ScmObj dyn_ext;
};

/*=======================================
  Variable Declarations
=======================================*/
/* storage-continuation.c */
extern ScmObj scm_current_dynamic_extent;

ScmObj scm_exception_handlers      = NULL;
ScmObj scm_exception_continuations = NULL;

static ScmObj exception_thrown_obj = NULL;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmObj guard_handle_clauses(ScmObj clauses, ScmObj env);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_Initialize_SRFI34(void)
{
    /*=======================================================================
      SRFI-34 Procedure
    =======================================================================*/
#if SCM_USE_REGISTER_TABLE
    REGISTER_FUNC_TABLE(srfi34_func_info_table);
#else /* SCM_USE_REGISTER_TABLE */
    Scm_RegisterProcedureFixed2("with-exception-handler", ScmOp_SRFI34_with_exception_handler);
    Scm_RegisterSyntaxVariadic1("guard"                 , ScmExp_SRFI34_guard);
    Scm_RegisterProcedureFixed1("raise"                 , ScmOp_SRFI34_raise);
#endif /* SCM_USE_REGISTER_TABLE */

    scm_exception_handlers      = SCM_FALSE;
    scm_exception_continuations = SCM_FALSE;
    SigScm_GC_Protect(&scm_exception_handlers);
    SigScm_GC_Protect(&scm_exception_continuations);
}

/*
 * FIXME: Reimplement with dynamic-wind as "Reference Implementation" of
 * SRFI-34 does, without direct use of setjmp/longjmp
 */
ScmObj ScmOp_SRFI34_with_exception_handler(ScmObj handler, ScmObj thunk)
{
    jmp_buf jmpenv;
    ScmObj ret  = SCM_FALSE;
    ScmObj cont = Scm_NewContinuation();
    struct continuation_frame cont_frame;
    DECLARE_FUNCTION("with-exception-handler", ProcedureFixed2);

    ASSERT_PROCEDUREP(handler);
    ASSERT_PROCEDUREP(thunk);

    CONTINUATION_SET_FRAME(cont, &cont_frame);
    CONTINUATION_SET_JMPENV(cont, &jmpenv);
    CONTINUATION_SET_DYNEXT(cont, scm_current_dynamic_extent);
    if (setjmp(jmpenv)) {
        ret = Scm_call(CURRENT_EXCEPTION_HANDLER(), LIST_1(exception_thrown_obj));
        POP_EXCEPTION_CONTINUATION();
        POP_EXCEPTION_HANDLER();
        exception_thrown_obj = SCM_FALSE; /* make sweepable */
        return ret;
    }

    PUSH_EXCEPTION_HANDLER(handler);
    PUSH_EXCEPTION_CONTINUATION(cont);
    ret = Scm_call(thunk, SCM_NULL);
    POP_EXCEPTION_CONTINUATION();
    POP_EXCEPTION_HANDLER();

    return ret;
}

/*
 * FIXME: Reimplement with dynamic-wind, Scm_CallWithCurrentContinuation() and
 * Scm_CallContinuation() as "Reference Implementation" of SRFI-34 does,
 * without direct use of setjmp/longjmp
 */
ScmObj ScmExp_SRFI34_guard(ScmObj var_and_clauses, ScmObj body, ScmObj env)
{
    /* (guard (var clauses) body) */
    jmp_buf jmpenv;
    ScmObj var     = SCM_FALSE;
    ScmObj clauses = SCM_FALSE;
    ScmObj expr    = SCM_FALSE;
    ScmObj cont    = Scm_NewContinuation();
    struct continuation_frame cont_frame;
    DECLARE_FUNCTION("guard", SyntaxVariadic1);

    ASSERT_CONSP(var_and_clauses);

    var     = CAR(var_and_clauses);
    clauses = CDR(var_and_clauses);

    ASSERT_SYMBOLP(var);

    /* check if return from "raise" */
    CONTINUATION_SET_FRAME(cont, &cont_frame);
    CONTINUATION_SET_JMPENV(cont, &jmpenv);
    CONTINUATION_SET_DYNEXT(cont, scm_current_dynamic_extent);
    if (setjmp(jmpenv)) {
        POP_EXCEPTION_CONTINUATION();
        env = Scm_ExtendEnvironment(LIST_1(var), LIST_1(exception_thrown_obj), env);
        return guard_handle_clauses(clauses, env);
    }

    PUSH_EXCEPTION_CONTINUATION(cont);
    while (!NO_MORE_ARG(body)) {
        expr = POP_ARG(body);
        expr = EVAL(expr, env);
    }
    POP_EXCEPTION_CONTINUATION();

    return expr;
}

/* FIXME:
 * - Simplify with ScmExp_cond()
 */
static ScmObj guard_handle_clauses(ScmObj clauses, ScmObj env)
{
    ScmObj thrown  = exception_thrown_obj;
    ScmObj clause  = SCM_FALSE;
    ScmObj test    = SCM_FALSE;
    ScmObj exps    = SCM_FALSE;
    ScmObj proc    = SCM_FALSE;
    ScmObj ret     = SCM_FALSE;
    DECLARE_INTERNAL_FUNCTION("guard");

    /* make sweepable */
    exception_thrown_obj = SCM_FALSE;

    /* handle "cond" like clause */
    for (; !NULLP(clauses); clauses = CDR(clauses)) {
        clause = CAR(clauses);
        if (!CONSP(clause))
            ERR_OBJ("bad clause", clause);

        test = CAR(clause);
        exps = CDR(clause);

        /* evaluate test */
        test = EVAL(test, env);

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
             * returned by this procedure is returned by the guard expression.
             */
            /* FIXME: remove expensive Scm_Intern() */
            if (EQ(Scm_Intern("=>"), CAR(exps))) {
                proc = EVAL(CADR(exps), env);
                if (FALSEP(ScmOp_procedurep(proc)))
                    ERR_OBJ("the value of exp after => must be the procedure but got", proc);

                return Scm_call(proc, LIST_1(test));
            }

            for (; !NULLP(exps); exps = CDR(exps))
                ret = EVAL(CAR(exps), env);

            return ret;
        }
    }

    /* "reraise" exception */
    if (NULLP(CURRENT_EXCEPTION_CONTINUATION()))
        ERR("guard: cannot reraise exception");
    ScmOp_SRFI34_raise(thrown);

    /* never reaches here */
    return SCM_UNDEF;
}

/*
 * FIXME:
 * - Reimplement with dynamic-wind as "Reference Implementation" of SRFI-34
 *   does, without direct use of setjmp/longjmp
 * - Cause error when the current exception handler returns, as "Reference
 *   Implementation" of SRFI-34 does. current implementation allows writing
 *   unspecified behavior
 */
ScmObj ScmOp_SRFI34_raise(ScmObj obj)
{
    jmp_buf *env;
    DECLARE_FUNCTION("raise", ProcedureFixed1);

    exception_thrown_obj = obj;

    env = CONTINUATION_JMPENV(CURRENT_EXCEPTION_CONTINUATION());
    longjmp(*env, 1);

    /* never reaches here */
    return SCM_UNDEF;
}
