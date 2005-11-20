/*===========================================================================
 *  FileName : error.c
 *  About    : handling errors
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
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

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
#define SCM_ERR_HEADER "Error: "
#define SCM_BACKTRACE_HEADER "**** BACKTRACE ****\n"

#define ERRMSG_UNHANDLED_EXCEPTION "unhandled exception"

/*=======================================
  Variable Declarations
=======================================*/
static int srfi34_is_provided, fatal_err_looped;

static ScmObj err_obj_tag, str_srfi34;

/*=======================================
  File Local Function Declarations
=======================================*/
static int srfi34_providedp(void);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitError(void)
{
    SigScm_GC_Protect(&err_obj_tag);
    SigScm_GC_Protect(&str_srfi34);

    /* allocate a cons cell as unique ID */
    err_obj_tag = CONS(SCM_UNDEF, SCM_UNDEF);

    str_srfi34 = Scm_NewStringCopying("srfi-34");
    srfi34_is_provided = FALSE;

    fatal_err_looped = FALSE;

    REGISTER_FUNC_TABLE(scm_error_func_info_table);
}

#if SCM_USE_SRFI34
static int srfi34_providedp(void)
{
    if (!srfi34_is_provided) {
        /* expensive */
        srfi34_is_provided = NFALSEP(ScmOp_providedp(str_srfi34));
    }
    return srfi34_is_provided;
}
#endif

/* The name 'error?' should be reserved for SRFI-35 */
ScmObj ScmOp_sscm_error_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("%%error-object?", ProcedureFixed1);
    return (CONSP(obj) && EQ(CAR(obj), err_obj_tag)) ? SCM_TRUE : SCM_FALSE;
}

/* FIXME: make (pair? err-obj) #f */
ScmObj Scm_MakeErrorObj(ScmObj reason, ScmObj objs)
{
    DECLARE_INTERNAL_FUNCTION("Scm_MakeErrorObj");

    ASSERT_CONSP(objs);
#if 0
    /* should be string, but not forced. displayable is sufficient. */
    ASSERT_STRINGP(reason);
#endif

    return LIST_4(err_obj_tag, reason, objs, Scm_TraceStack());
}

void Scm_RaiseError(ScmObj err_obj)
{
    DECLARE_INTERNAL_FUNCTION("Scm_RaiseError");

    ASSERT_ERROBJP(err_obj);

#if SCM_USE_SRFI34
    if (srfi34_providedp()) {
        ScmOp_SRFI34_raise(err_obj);
        /* NOTREACHED */
    }
#endif
    ScmOp_sscm_fatal_error(err_obj);
}

ScmObj ScmOp_sscm_fatal_error(ScmObj err_obj)
{
    DECLARE_FUNCTION("%%fatal-error", ProcedureFixed1);

    if (!fatal_err_looped) {
        fatal_err_looped = TRUE;
        ASSERT_ERROBJP(err_obj);
        ScmOp_sscm_inspect_error(err_obj);
    }

#if 0
    if (cb_fatal_error)
        (*cb_fatal_error)();
#endif

    exit(EXIT_FAILURE);
    /* NOTREACHED */
}

ScmObj ScmOp_sscm_inspect_error(ScmObj err_obj)
{
    ScmObj rest, err_obj_tag, reason, objs, trace_stack;
    DECLARE_FUNCTION("%%inspect-error", ProcedureFixed1);

    if (ERROBJP(err_obj)) {
        rest = err_obj;
        err_obj_tag = MUST_POP_ARG(rest);
        reason      = MUST_POP_ARG(rest);
        objs        = MUST_POP_ARG(rest);
        trace_stack = MUST_POP_ARG(rest);
        ASSERT_NO_MORE_ARG(rest);
    }

    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();
        if (ERROBJP(err_obj)) {
            SigScm_DisplayToPort(scm_current_error_port, err_obj);
        } else {
            SCM_PORT_PRINT(scm_current_error_port, ERRMSG_UNHANDLED_EXCEPTION);
            SCM_PORT_PRINT(scm_current_error_port, ": ");
            SigScm_WriteToPort(scm_current_error_port, err_obj);
        }
        SigScm_ErrorNewline();
    }

    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE) {
        if (!ERROBJP(err_obj))
            trace_stack = Scm_TraceStack();
        SigScm_ShowBacktrace(trace_stack);
    }

    return SCM_UNDEF;
}

ScmObj ScmOp_sscm_backtrace(void)
{
    DECLARE_FUNCTION("%%backtrace", ProcedureFixed0);

    SigScm_ShowBacktrace(Scm_TraceStack());

    return SCM_UNDEF;
}

int SigScm_Die(const char *msg, const char *filename, int line)
{
    char *reason;
    ScmObj err_obj;

#if HAVE_ASPRINTF
    asprintf(&reason, "SigScheme Died : %s (file : %s, line : %d)",
             msg, filename, line);
#else /* HAVE_ASPRINTF */
    /* FIXME: provide replace asprintf */
    reason = strdup("SigScheme Died");
#endif /* HAVE_ASPRINTF */
    err_obj = Scm_MakeErrorObj(Scm_NewString(reason), LIST_1(SCM_UNDEF));
    ScmOp_sscm_fatal_error(err_obj);
    /* NOTREACHED */
    return 1;  /* dummy value for boolean expression */
}

void SigScm_Error(const char *msg, ...)
{
    va_list va;
    char *reason;
    ScmObj err_obj;

#if HAVE_VASPRINTF
    va_start(va, msg);
    vasprintf(&reason, msg, va);
    va_end(va);
#else /* HAVE_VASPRINTF */
    /* FIXME: provide replace vasprintf */
    reason = strdup(msg);
#endif /* HAVE_VASPRINTF */
    err_obj = Scm_MakeErrorObj(Scm_NewString(reason), LIST_1(SCM_UNDEF));
    Scm_RaiseError(err_obj);
    /* NOTREACHED */
}

/* Obsolete. */
void SigScm_ErrorObj(const char *msg, ScmObj obj)
{
    ScmObj err_obj;

    err_obj = Scm_MakeErrorObj(Scm_NewStringCopying(msg), LIST_1(obj));
    Scm_RaiseError(err_obj);
    /* NOTREACHED */
}

/* This function obsoletes SigScm_ErrorObj(). */
void Scm_ErrorObj(const char *func_name, const char *msg, ScmObj obj)
{
    char *reason;
    ScmObj err_obj;

#if HAVE_ASPRINTF
    asprintf(&reason, "in %s: %s", func_name, msg);
#else /* HAVE_ASPRINTF */
    /* FIXME: provide replace asprintf */
    reason = strdup(msg);
#endif /* HAVE_ASPRINTF */
    err_obj = Scm_MakeErrorObj(Scm_NewString(reason), LIST_1(obj));
    Scm_RaiseError(err_obj);
    /* NOTREACHED */
}

void SigScm_ShowBacktrace(ScmObj trace_stack)
{
#define UNBOUNDP(var, env)                                              \
    (NULLP(Scm_LookupEnvironment(var, env))                             \
     && !SCM_SYMBOL_BOUNDP(var))

#if SCM_DEBUG
    ScmObj top;
    ScmObj frame;
    ScmObj env;
    ScmObj obj;
    ScmObj proc;

    SigScm_ErrorPrintf(SCM_BACKTRACE_HEADER);

    /* show each frame's obj */
    for (top = trace_stack; !NULLP(top); top = CDR(top)) {
#if SCM_DEBUG_BACKTRACE_SEP
        SigScm_ErrorPrintf("------------------------------\n");
#endif /* SCM_DEBUG_BACKTRACE_SEP */

        frame = CAR(top);
        env = TRACE_FRAME_ENV(frame);
        obj = TRACE_FRAME_OBJ(frame);

#if SCM_USE_SRFI38
        SigScm_WriteToPortWithSharedStructure(scm_current_error_port, obj);
#else
        SigScm_WriteToPort(scm_current_error_port, obj);
#endif
        SigScm_ErrorNewline();

#if SCM_DEBUG_BACKTRACE_VAL
        switch (SCM_TYPE(obj)) {
        case ScmSymbol:
            if (UNBOUNDP(obj, env))
                break;
            SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(obj));
#if SCM_USE_SRFI38
            SigScm_WriteToPortWithSharedStructure(scm_current_error_port, Scm_SymbolValue(obj, env));
#else
            SigScm_WriteToPort(scm_current_error_port, Scm_SymbolValue(obj, env));
#endif
            SigScm_ErrorNewline();
            break;

        case ScmCons:
            for (; CONSP(obj); obj = CDR(obj)) {
                proc = CAR(obj);
                if (SYMBOLP(proc)) {
                    if (UNBOUNDP(proc, env))
                        break;
                    SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(proc));
#if SCM_USE_SRFI38
                    SigScm_WriteToPortWithSharedStructure(scm_current_error_port,
                                                          Scm_SymbolValue(proc, env));
#else
                    SigScm_WriteToPort(scm_current_error_port,
                                       Scm_SymbolValue(proc, env));
#endif
                    SigScm_ErrorNewline();
                }
            }
            if (SYMBOLP(obj)) {
                SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(proc));
#if SCM_USE_SRFI38
                SigScm_WriteToPortWithSharedStructure(scm_current_error_port,
                                                      Scm_SymbolValue(proc, env));
#else
                SigScm_WriteToPort(scm_current_error_port,
                                   Scm_SymbolValue(proc, env));
#endif
                SigScm_ErrorNewline();
            }
            break;

        default:
            break;
        }
#endif /* SCM_DEBUG_BACKTRACE_VAL */
    }
#if SCM_DEBUG_BACKTRACE_SEP
        SigScm_ErrorPrintf("------------------------------\n");
#endif /* SCM_DEBUG_BACKTRACE_SEP */
#endif /* SCM_DEBUG */
#undef UNBOUNDP
}

void SigScm_ShowErrorHeader(void)
{
    SigScm_ErrorPrintf(SCM_ERR_HEADER);
}
