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

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static void throw_exception(ScmObj errorobj) SCM_NORETURN;

/*=======================================
  Function Implementations
=======================================*/
static void throw_exception(ScmObj errorobj)
{
#if SCM_EXCEPTION_HANDLING
    if (FALSEP(CURRENT_EXCEPTION_CONTINUATION())) {
        /* outermost exception handler */
        if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
            SigScm_ShowBacktrace();

        exit(EXIT_FAILURE);
    } else {
        /* throw an exception */
        ScmOp_SRFI34_raise(errorobj);
    }
#else
    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
        SigScm_ShowBacktrace();
#endif    

    exit(EXIT_FAILURE);
}

int SigScm_Die(const char *msg, const char *filename, int line) {
    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();
        SigScm_ErrorPrintf("SigScheme Died : %s (file : %s, line : %d)\n",
                           msg, filename, line);
    }

    if (SigScm_DebugCategories() & SCM_DBG_BACKTRACE)
        SigScm_ShowBacktrace();

    exit(EXIT_FAILURE);
    /* NOTREACHED */
    return 1;  /* dummy value for boolean expression */
}

void SigScm_Error(const char *msg, ...)
{
    va_list va;

    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();

        va_start(va, msg);
        SigScm_VErrorPrintf(msg, va);
        va_end(va);

        SigScm_ErrorNewline();
    }

    /* FIXME: this errorobj is OK? */
    throw_exception(Scm_NewStringCopying("ERROR"));
}

/* Obsolete. */
void SigScm_ErrorObj(const char *msg, ScmObj obj)
{
    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();
        SigScm_ErrorPrintf(msg);
        SigScm_WriteToPort(scm_current_error_port, obj);
        SigScm_ErrorNewline();
    }

    /* FIXME: this errorobj is OK? */
    throw_exception(Scm_NewStringCopying("ERROR"));
}

/* This function obsoletes SigScm_ErrorObj(). */
void Scm_ErrorObj(const char *func_name, const char *msg, ScmObj obj)
{
    if (SigScm_DebugCategories() & SCM_DBG_ERRMSG) {
        SigScm_ShowErrorHeader();
        SigScm_ErrorPrintf("in %s: %s: ", func_name, msg);
        SigScm_WriteToPort(scm_current_error_port, obj);
        SigScm_ErrorNewline();
    }

    /* FIXME: this errorobj is OK? */
    throw_exception(Scm_NewStringCopying("ERROR"));
}

void SigScm_ShowBacktrace(void)
{
#define UNBOUNDP(var, env)                                              \
    (NULLP(Scm_LookupEnvironment(var, env))                             \
     && !SCM_SYMBOL_BOUNDP(var))

#if SCM_DEBUG
    struct trace_frame *f;
    ScmObj env;
    ScmObj obj;
    ScmObj proc;

    SigScm_ErrorPrintf(SCM_BACKTRACE_HEADER);

    /* show each frame's obj */
    for (f = scm_trace_root; f; f = f->prev) {
#if SCM_DEBUG_BACKTRACE_SEP
        SigScm_ErrorPrintf("------------------------------\n");
#endif /* SCM_DEBUG_BACKTRACE_SEP */

        env = f->env;
        obj = f->obj;

        SigScm_WriteToPort(scm_current_error_port, obj);
        SigScm_ErrorNewline();

#if SCM_DEBUG_BACKTRACE_VAL
        switch (SCM_TYPE(obj)) {
        case ScmSymbol:
            if (UNBOUNDP(obj, env))
                break;
            SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(obj));
            SigScm_WriteToPort(scm_current_error_port, Scm_SymbolValue(obj, env));
            SigScm_ErrorNewline();
            break;

        case ScmCons:
            for (; CONSP(obj); obj = CDR(obj)) {
                proc = CAR(obj);
                if (SYMBOLP(proc)) {
                    if (UNBOUNDP(proc, env))
                        break;
                    SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(proc));
                    SigScm_WriteToPort(scm_current_error_port,
                                       Scm_SymbolValue(proc, env));
                    SigScm_ErrorNewline();
                }
            }
            if (SYMBOLP(obj)) {
                SigScm_ErrorPrintf("  - [%s]: ", SCM_SYMBOL_NAME(proc));
                SigScm_WriteToPort(scm_current_error_port,
                                   Scm_SymbolValue(proc, env));
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

/* TODO: move to io.c */
void SigScm_PortPrintf(ScmObj port, const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    SigScm_VPortPrintf(port, fmt, args);
    va_end(args);
}

void SigScm_VPortPrintf(ScmObj port, const char *fmt, va_list args)
{
    if (!FALSEP(port)) {
        SCM_PORT_VPRINTF(port, fmt, args);
#if SCM_VOLATILE_OUTPUT
        SCM_PORT_FLUSH(port);
#endif
    }
}

void SigScm_PortNewline(ScmObj port)
{
    if (!FALSEP(port)) {
        SCM_PORT_PUTS(port, SCM_NEWLINE_STR);
    }
}

void SigScm_ErrorPrintf(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    SigScm_VErrorPrintf(fmt, args);
    va_end(args);
}

void SigScm_VErrorPrintf(const char *fmt, va_list args)
{
    SigScm_VPortPrintf(scm_current_error_port, fmt, args);
}

void SigScm_ErrorNewline(void)
{
    SigScm_PortNewline(scm_current_error_port);
}
