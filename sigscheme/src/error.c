/*===========================================================================
 *  Filename : error.c
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

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define SCM_BACKTRACE_HEADER "**** BACKTRACE ****"
#define SCM_BACKTRACE_SEP    "------------------------------"

#define NO_ERR_OBJ l_err_obj_tag

#define UNBOUNDP(var, env)                                                   \
    (scm_lookup_environment(var, env) == SCM_INVALID_REF                     \
     && !SCM_SYMBOL_BOUNDP(var))

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
#if (!HAVE_C99_VARIADIC_MACRO && !HAVE_GNU_VARIADIC_MACRO)
SCM_DEFINE_EXPORTED_VARS(error);
#endif

SCM_GLOBAL_VARS_BEGIN(static_error);
#define static
static enum ScmDebugCategory l_debug_mask;
static scm_bool l_srfi34_is_provided, l_error_looped, l_fatal_error_looped;
static void (*l_cb_fatal_error)(void);

static ScmObj l_err_obj_tag;
#undef static
SCM_GLOBAL_VARS_END(static_error);
#define l_debug_mask         SCM_GLOBAL_VAR(static_error, l_debug_mask)
#define l_srfi34_is_provided SCM_GLOBAL_VAR(static_error, l_srfi34_is_provided)
#define l_error_looped       SCM_GLOBAL_VAR(static_error, l_error_looped)
#define l_fatal_error_looped SCM_GLOBAL_VAR(static_error, l_fatal_error_looped)
#define l_cb_fatal_error     SCM_GLOBAL_VAR(static_error, l_cb_fatal_error)
#define l_err_obj_tag        SCM_GLOBAL_VAR(static_error, l_err_obj_tag)
SCM_DEFINE_STATIC_VARS(static_error);

/*=======================================
  File Local Function Declarations
=======================================*/
static scm_bool srfi34_providedp(void);
static void scm_error_internal(const char *func_name, ScmObj obj,
                               const char *msg, va_list args) SCM_NORETURN;
#if (SCM_USE_BACKTRACE && SCM_DEBUG_BACKTRACE_VAL)
static void show_arg(ScmObj arg, ScmObj env);
#endif

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_init_error(void)
{
#if (!HAVE_C99_VARIADIC_MACRO && !HAVE_GNU_VARIADIC_MACRO)
    SCM_GLOBAL_VARS_INIT(error);
#endif
    SCM_GLOBAL_VARS_INIT(static_error);

    /* allocate a cons cell as unique ID */
    scm_gc_protect_with_init(&l_err_obj_tag, CONS(SCM_UNDEF, SCM_UNDEF));

    l_srfi34_is_provided = scm_false;
    l_fatal_error_looped = scm_false;
}

SCM_EXPORT enum ScmDebugCategory
scm_debug_categories(void)
{
    return l_debug_mask;
}

SCM_EXPORT void
scm_set_debug_categories(enum ScmDebugCategory categories)
{
    l_debug_mask = categories;
}

SCM_EXPORT enum ScmDebugCategory
scm_predefined_debug_categories(void)
{
    return (SCM_DBG_NONE
#if SCM_DEBUG
            | SCM_DBG_DEVEL | SCM_DBG_COMPAT | SCM_DBG_OTHER
#if SCM_DEBUG_PARSER
            | SCM_DBG_PARSER
#endif
#if SCM_DEBUG_GC
            | SCM_DBG_GC
#endif
#if SCM_DEBUG_ENCODING
            | SCM_DBG_ENCODING
#endif
#if SCM_DEBUG_MACRO
            | SCM_DBG_MACRO
#endif
#endif /* SCM_DEBUG */
            );
}

#if SCM_DEBUG
SCM_EXPORT void
scm_categorized_debug(enum ScmDebugCategory category, const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (l_debug_mask & category) {
        scm_vformat(scm_err, SCM_FMT_INTERNAL, msg, va);
        scm_port_newline(scm_err);
    }
    va_end(va);
}

SCM_EXPORT void
scm_debug(const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    if (l_debug_mask & SCM_DBG_DEVEL) {
        scm_vformat(scm_err, SCM_FMT_INTERNAL, msg, va);
        scm_port_newline(scm_err);
    }
    va_end(va);
}
#endif /* SCM_DEBUG */

#if SCM_USE_SRFI34
static scm_bool
srfi34_providedp(void)
{
    if (!l_srfi34_is_provided) {
        /* expensive */
        l_srfi34_is_provided = scm_providedp(CONST_STRING("srfi-34"));
    }
    return l_srfi34_is_provided;
}
#endif

/* The name 'error?' should be reserved for SRFI-35 */
SCM_EXPORT ScmObj
scm_p_error_objectp(ScmObj obj)
{
    DECLARE_FUNCTION("%%error-object?", procedure_fixed_1);

    return MAKE_BOOL(CONSP(obj) && EQ(CAR(obj), l_err_obj_tag));
}

/* FIXME: make (pair? err-obj) #f */
SCM_EXPORT ScmObj
scm_make_error_obj(ScmObj reason, ScmObj objs)
{
    DECLARE_INTERNAL_FUNCTION("scm_make_error_obj");

#if 0
    /* should be string, but not forced. displayable is sufficient. */
    ENSURE_STRING(reason);
#endif
    ENSURE_LIST(objs);

    return LIST_4(l_err_obj_tag, reason, objs, scm_trace_stack());
}

SCM_EXPORT void
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

SCM_EXPORT void
scm_fatal_error(const char *msg)
{
    /* don't use Scheme-level ports here */
    if (msg) {
        fputs(SCM_ERR_HEADER "fatal: ", stderr);
        fputs(msg, stderr);
        fputs(SCM_NEWLINE_STR, stderr);
    }

    if (l_cb_fatal_error)
        (*l_cb_fatal_error)();

    exit(EXIT_FAILURE);
    /* NOTREACHED */
}

SCM_EXPORT void
scm_set_fatal_error_callback(void (*cb)(void))
{
    l_cb_fatal_error = cb;
}

SCM_EXPORT ScmObj
scm_p_fatal_error(ScmObj err_obj)
{
    const char *msg;
    DECLARE_FUNCTION("%%fatal-error", procedure_fixed_1);

    if (l_fatal_error_looped) {
        /* to avoid infinite loop by implicit assertion, use no SCM macros */
        msg = "looped fatal error";
    } else {
        l_fatal_error_looped = scm_true;
        ENSURE_ERROBJ(err_obj);
        scm_p_inspect_error(err_obj);
        msg = NULL;
    }

    scm_fatal_error(msg);
    /* NOTREACHED */
}

SCM_EXPORT ScmObj
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
        scm_port_puts(scm_err, SCM_ERR_HEADER);
        if (ERROBJP(err_obj)) {
#if SCM_USE_SRFI38
            scm_display_errobj_ss(scm_err, err_obj);
#else
            scm_display(scm_err, err_obj);
#endif
        } else {
            scm_port_puts(scm_err, ERRMSG_UNHANDLED_EXCEPTION ": ");
            SCM_WRITE_SS(scm_err, err_obj);
        }
        scm_port_newline(scm_err);
    }

#if SCM_USE_BACKTRACE
    if (scm_debug_categories() & SCM_DBG_BACKTRACE)
        scm_show_backtrace(trace_stack);
#endif

    return SCM_UNDEF;
}

SCM_EXPORT ScmObj
scm_p_backtrace(void)
{
    DECLARE_FUNCTION("%%backtrace", procedure_fixed_0);

    scm_show_backtrace(scm_trace_stack());

    return SCM_UNDEF;
}

SCM_EXPORT void
scm_die(const char *msg, const char *filename, int line)
{
#if SCM_DEBUG
    ScmObj reason;

    /* reason will implicitly be freed via the object on GC */
    reason = scm_format(SCM_FALSE, SCM_FMT_RAW_C,
                        "~S: (file: ~S, line: ~D)", msg, filename, line);
    scm_fatal_error(SCM_STRING_STR(reason));
#else
    scm_fatal_error(msg);
#endif
    /* NOTREACHED */
}

static void
scm_error_internal(const char *func_name, ScmObj obj,
                   const char *msg, va_list args)
{
    ScmObj reason, err_obj;

    if (l_error_looped)
        scm_fatal_error("bug: double error on preparing error object");

    /* It is supposed that no continuation switching occurs on this guarded
     * duration. So the global variable based guard works properly. */
    l_error_looped = scm_true;
#if SCM_USE_FORMAT
    reason = scm_vformat(SCM_FALSE, SCM_FMT_INTERNAL, msg, args);
    if (func_name) {
        reason = scm_format(SCM_FALSE, SCM_FMT_RAW_C,
                            "in ~S: ~S~S",
                            func_name, SCM_STRING_STR(reason),
                            (EQ(obj, NO_ERR_OBJ) ? "" : ":"));
    }
#else
    reason = CONST_STRING(msg);
#endif

    err_obj = scm_make_error_obj(reason,
                                 (EQ(obj, NO_ERR_OBJ)) ? SCM_NULL : LIST_1(obj));
    l_error_looped = scm_false;

    scm_raise_error(err_obj);
    /* NOTREACHED */
}

SCM_EXPORT void
scm_plain_error(const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    scm_error_internal(NULL, NO_ERR_OBJ, msg, va);
    /* va_end(va); */
    /* NOTREACHED */
}

#if (!HAVE_C99_VARIADIC_MACRO && !HAVE_GNU_VARIADIC_MACRO)
SCM_EXPORT void
scm_error_with_implicit_func(const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    scm_error_internal(scm_err_funcname, NO_ERR_OBJ, msg, va);
    /* va_end(va); */
    /* NOTREACHED */
}
#endif /* (!HAVE_C99_VARIADIC_MACRO && !HAVE_GNU_VARIADIC_MACRO) */

SCM_EXPORT void
scm_error(const char *func_name, const char *msg, ...)
{
    va_list va;

    va_start(va, msg);
    scm_error_internal(func_name, NO_ERR_OBJ, msg, va);
    /* va_end(va); */
    /* NOTREACHED */
}

SCM_EXPORT void
scm_error_obj(const char *func_name, const char *msg, ScmObj obj)
{
    va_list dummy_va;

    scm_error_internal(func_name, obj, msg, dummy_va);
    /* NOTREACHED */
}

#if (SCM_USE_BACKTRACE && SCM_DEBUG_BACKTRACE_VAL)
static void
show_arg(ScmObj arg, ScmObj env)
{
    if (SYMBOLP(arg) && !UNBOUNDP(arg, env)) {
        scm_format(scm_err, SCM_FMT_RAW_C, "  - [~S]: ", SCM_SYMBOL_NAME(arg));
        SCM_WRITE_SS(scm_err, scm_symbol_value(arg, env));
        scm_port_newline(scm_err);
    }
}
#endif /* (SCM_USE_BACKTRACE && SCM_DEBUG_BACKTRACE_VAL) */

SCM_EXPORT void
scm_show_backtrace(ScmObj trace_stack)
{
#if SCM_USE_BACKTRACE
    ScmObj frame, env, obj, elm;
    DECLARE_INTERNAL_FUNCTION("scm_show_backtrace");

    if (NULLP(trace_stack))
        return;

    scm_port_puts(scm_err, SCM_BACKTRACE_HEADER);
    scm_port_newline(scm_err);

    /* show each frame's obj */
    FOR_EACH (frame, trace_stack) {
#if SCM_DEBUG_BACKTRACE_SEP
        scm_port_puts(scm_err, SCM_BACKTRACE_SEP);
        scm_port_newline(scm_err);
#endif

        env = TRACE_FRAME_ENV(frame);
        obj = TRACE_FRAME_OBJ(frame);

        SCM_WRITE_SS(scm_err, obj);
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
            if (IDENTIFIERP(obj))
                show_arg(obj, env);
            break;

        default:
            break;
        }
#endif /* SCM_DEBUG_BACKTRACE_VAL */
    }
#if SCM_DEBUG_BACKTRACE_SEP
    scm_port_puts(scm_err, SCM_BACKTRACE_SEP);
    scm_port_newline(scm_err);
#endif /* SCM_DEBUG_BACKTRACE_SEP */
#endif /* SCM_USE_BACKTRACE */
}
