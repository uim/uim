/*===========================================================================
 *  FileName : error.c
 *  About    : handling errors
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
#define SCM_BACKTRACE_HEADER "**** BACKTRACE ****\n"
#define SCM_BACKTRACE_SEP    "------------------------------\n"

/*=======================================
  Variable Declarations
=======================================*/
static int debug_mask;
static scm_bool srfi34_is_provided, fatal_error_looped;
static void (*cb_fatal_error)(void);

static ScmObj err_obj_tag, str_srfi34;

/*=======================================
  File Local Function Declarations
=======================================*/
static scm_bool srfi34_providedp(void);
#if (SCM_DEBUG && SCM_DEBUG_BACKTRACE_VAL)
static void show_arg(ScmObj arg, ScmObj env);
#endif

/*=======================================
  Function Implementations
=======================================*/
void
scm_init_error(void)
{
    /* allocate a cons cell as unique ID */
    scm_gc_protect_with_init(&err_obj_tag, CONS(SCM_UNDEF, SCM_UNDEF));

    scm_gc_protect_with_init(&str_srfi34, CONST_STRING("srfi-34"));
    srfi34_is_provided = scm_false;

    cb_fatal_error = NULL;
    fatal_error_looped = scm_false;

    REGISTER_FUNC_TABLE(scm_error_func_info_table);
}

int
scm_debug_categories(void)
{
    return debug_mask;
}

void
scm_set_debug_categories(int categories)
{
    debug_mask = categories;
}

int
scm_predefined_debug_categories(void)
{
#if SCM_DEBUG
    return (SCM_DBG_DEVEL | SCM_DBG_COMPAT | SCM_DBG_OTHER
#if SCM_DEBUG_PARSER
            | SCM_DBG_PARSER
#endif
#if SCM_DEBUG_GC
            | SCM_DBG_GC
#endif
#if SCM_DEBUG_ENCODING
            | SCM_DBG_ENCODING
#endif
            );
#else /* SCM_DEBUG */
    return SCM_DBG_NONE;
#endif /* SCM_DEBUG */
}

void
scm_categorized_debug(int category, const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (debug_mask & category) {
        scm_port_vprintf(scm_err, msg, va);
        scm_port_newline(scm_err);
    }
    va_end(va);
}

void
scm_debug(const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (debug_mask & SCM_DBG_DEVEL) {
        scm_port_vprintf(scm_err, msg, va);
        scm_port_newline(scm_err);
    }
    va_end(va);
}

#if SCM_USE_SRFI34
static scm_bool
srfi34_providedp(void)
{
    if (!srfi34_is_provided) {
        /* expensive */
        srfi34_is_provided = NFALSEP(scm_p_providedp(str_srfi34));
    }
    return srfi34_is_provided;
}
#endif

/* The name 'error?' should be reserved for SRFI-35 */
ScmObj
scm_p_error_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("%%error-object?", procedure_fixed_1);

    return MAKE_BOOL(CONSP(obj) && EQ(CAR(obj), err_obj_tag));
}

/* FIXME: make (pair? err-obj) #f */
ScmObj
scm_make_error_obj(ScmObj reason, ScmObj objs)
{
    DECLARE_INTERNAL_FUNCTION("scm_make_error_obj");

    ENSURE_LIST(objs);
#if 0
    /* should be string, but not forced. displayable is sufficient. */
    ENSURE_STRING(reason);
#endif

    return LIST_4(err_obj_tag, reason, objs, scm_trace_stack());
}

void
scm_raise_error(ScmObj err_obj)
{
    DECLARE_INTERNAL_FUNCTION("scm_raise_error");

    ENSURE_ERROBJ(err_obj);

#if SCM_USE_SRFI34
    if (srfi34_providedp()) {
        scm_p_srfi34_raise(err_obj);
        /* NOTREACHED */
    }
#endif
    scm_p_fatal_error(err_obj);
}

void
scm_fatal_error(const char *msg)
{
    /* don't use Scheme-level ports here */
    if (msg) {
        fputs(SCM_ERR_HEADER, stderr);
        fputs(msg, stderr);
        fputs(SCM_NEWLINE_STR, stderr);
    }

    if (cb_fatal_error)
        (*cb_fatal_error)();

    exit(EXIT_FAILURE);
    /* NOTREACHED */
}

void scm_set_fatal_error_callback(void (*cb)(void))
{
    cb_fatal_error = cb;
}

ScmObj
scm_p_fatal_error(ScmObj err_obj)
{
    const char *msg;
    DECLARE_FUNCTION("%%fatal-error", procedure_fixed_1);

    if (fatal_error_looped) {
        /* to avoid infinite loop by implicit assertion, use no SCM macros */
        msg = "looped fatal error";
    } else {
        fatal_error_looped = scm_true;
        ENSURE_ERROBJ(err_obj);
        scm_p_inspect_error(err_obj);
        msg = NULL;
    }

    scm_fatal_error(msg);
    /* NOTREACHED */
}

ScmObj
scm_p_inspect_error(ScmObj err_obj)
{
    ScmObj rest, err_obj_tag, reason, objs, trace_stack;
    DECLARE_FUNCTION("%%inspect-error", procedure_fixed_1);

    if (ERROBJP(err_obj)) {
        rest = err_obj;
        err_obj_tag = MUST_POP_ARG(rest);
        reason      = MUST_POP_ARG(rest);
        objs        = MUST_POP_ARG(rest);
        trace_stack = MUST_POP_ARG(rest);
        ASSERT_NO_MORE_ARG(rest);
    } else {
        trace_stack = scm_trace_stack();
    }

    if (scm_debug_categories() & SCM_DBG_ERRMSG) {
        scm_port_printf(scm_err, SCM_ERR_HEADER);
        if (ERROBJP(err_obj)) {
            scm_display_to_port(scm_err, err_obj);
        } else {
            scm_port_puts(scm_err, SCM_ERRMSG_UNHANDLED_EXCEPTION);
            scm_port_puts(scm_err, ": ");
            scm_write_to_port(scm_err, err_obj);
        }
        scm_port_newline(scm_err);
    }

    if (scm_debug_categories() & SCM_DBG_BACKTRACE)
        scm_show_backtrace(trace_stack);

    return SCM_UNDEF;
}

ScmObj
scm_p_backtrace(void)
{
    DECLARE_FUNCTION("%%backtrace", procedure_fixed_0);

    scm_show_backtrace(scm_trace_stack());

    return SCM_UNDEF;
}

void
scm_die(const char *msg, const char *filename, int line)
{
    char *reason;
    ScmObj reason_holder;

    asprintf(&reason, "%s: (file: %s, line: %d)", msg, filename, line);
    ENSURE_ALLOCATED(reason);
    /* reason will implicitly be freed via the object on GC */
    reason_holder = MAKE_IMMUTABLE_STRING(reason, STRLEN_UNKNOWN);

    scm_fatal_error(reason);
    /* NOTREACHED */
}

void
scm_error(const char *msg, ...)
{
    va_list va;
    char *reason;
    ScmObj err_obj;

    va_start(va, msg);
    vasprintf(&reason, msg, va);
    va_end(va);
    ENSURE_ALLOCATED(reason);

    err_obj = scm_make_error_obj(MAKE_IMMUTABLE_STRING(reason, STRLEN_UNKNOWN),
                                 SCM_NULL);
    scm_raise_error(err_obj);
    /* NOTREACHED */
}

/* This function obsoletes scm_error_obj(). */
void
scm_error_obj(const char *func_name, const char *msg, ScmObj obj)
{
    char *reason;
    ScmObj err_obj;

    asprintf(&reason, "in %s: %s", func_name, msg);
    ENSURE_ALLOCATED(reason);

    err_obj = scm_make_error_obj(MAKE_IMMUTABLE_STRING(reason, STRLEN_UNKNOWN),
                                 LIST_1(obj));
    scm_raise_error(err_obj);
    /* NOTREACHED */
}

#if (SCM_DEBUG && SCM_DEBUG_BACKTRACE_VAL)
static void
show_arg(ScmObj arg, ScmObj env)
{
#define UNBOUNDP(var, env)                                              \
    (scm_lookup_environment(var, env) == SCM_INVALID_REF                 \
     && !SCM_SYMBOL_BOUNDP(var))

    if (SYMBOLP(arg) && !UNBOUNDP(arg, env)) {
        scm_port_printf(scm_err, "  - [%s]: ", SCM_SYMBOL_NAME(arg));
        SCM_WRITESS_TO_PORT(scm_err, scm_symbol_value(arg, env));
        scm_port_newline(scm_err);
    }

#undef UNBOUNDP
}
#endif /* (SCM_DEBUG && SCM_DEBUG_BACKTRACE_VAL) */

void
scm_show_backtrace(ScmObj trace_stack)
{
#if SCM_DEBUG
    ScmObj frame, env, obj, elm;
    DECLARE_INTERNAL_FUNCTION("scm_show_backtrace");

    scm_port_printf(scm_err, SCM_BACKTRACE_HEADER);

    /* show each frame's obj */
    FOR_EACH (frame, trace_stack) {
#if SCM_DEBUG_BACKTRACE_SEP
        scm_port_printf(scm_err, SCM_BACKTRACE_SEP);
#endif

        env = TRACE_FRAME_ENV(frame);
        obj = TRACE_FRAME_OBJ(frame);

        SCM_WRITESS_TO_PORT(scm_err, obj);
        scm_port_newline(scm_err);

#if SCM_DEBUG_BACKTRACE_VAL
        switch (SCM_TYPE(obj)) {
        case ScmSymbol:
            show_arg(obj, env);
            break;

        case ScmCons:
            FOR_EACH (elm, obj)
                show_arg(elm, env);
            /* dot list */
            if (SYMBOLP(obj))
                show_arg(obj, env);
            break;

        default:
            break;
        }
#endif /* SCM_DEBUG_BACKTRACE_VAL */
    }
#if SCM_DEBUG_BACKTRACE_SEP
    scm_port_printf(scm_err, SCM_BACKTRACE_SEP);
#endif /* SCM_DEBUG_BACKTRACE_SEP */
#endif /* SCM_DEBUG */
}
