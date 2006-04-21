/*===========================================================================
 *  FileName : continuation.c
 *  About    : A Continuation implementation with setjmp/longjmp
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

#include <stdlib.h>
#include <setjmp.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Declarations
=======================================*/
/* specifies whether the storage abstraction layer can only handle nested
 * (stacked) continuation or R5RS-conformant full implementation. But current
 * implementation only supports '1'.
 */
#define SCM_NESTED_CONTINUATION_ONLY 1

#define CONTINUATION_FRAME(cont)                                             \
    ((struct continuation_frame *)SCM_CONTINUATION_OPAQUE(cont))
#define CONTINUATION_SET_FRAME    SCM_CONTINUATION_SET_OPAQUE

/*=======================================
  File Local Type Definitions
=======================================*/
struct continuation_frame {
    /*
     * - To hint appropriate alignment on stack, a ScmObj is listed first
     * - GC marking for these ScmObj are implicitly performed by stack scanning
     */
    volatile ScmObj dyn_ext;
    volatile ScmObj ret_val;
#if SCM_DEBUG
    volatile ScmObj trace_stack;
#endif
    jmp_buf c_env;
};

/*=======================================
  Variable Declarations
=======================================*/
SCM_GLOBAL_VARS_BEGIN(static_continuation);
#define static
static volatile ScmObj l_current_dynamic_extent;
static volatile ScmObj l_continuation_stack;
static volatile ScmObj l_trace_stack;
#undef static
SCM_GLOBAL_VARS_END(static_continuation);
#define l_current_dynamic_extent                                             \
    SCM_GLOBAL_VAR(static_continuation, l_current_dynamic_extent)
#define l_continuation_stack                                                 \
    SCM_GLOBAL_VAR(static_continuation, l_continuation_stack)
#define l_trace_stack                                                        \
    SCM_GLOBAL_VAR(static_continuation, l_trace_stack)
SCM_DEFINE_STATIC_VARS(static_continuation);

/*=======================================
  File Local Function Declarations
=======================================*/
/* dynamic extent */
static void initialize_dynamic_extent(void);
static void finalize_dynamic_extent(void);
static void wind_onto_dynamic_extent(ScmObj before, ScmObj after);
static void unwind_dynamic_extent(void);
static void enter_dynamic_extent(ScmObj dest);
static void exit_dynamic_extent(ScmObj dest);

/* continuation */
static void initialize_continuation_env(void);
static void finalize_continuation_env(void);
static void continuation_stack_push(ScmObj cont);
static ScmObj continuation_stack_pop(void);
static ScmObj continuation_stack_unwind(ScmObj dest_cont);

/*=======================================
  Function Implementations
=======================================*/
SCM_EXPORT void
scm_init_continuation(void)
{
    SCM_GLOBAL_VARS_INIT(static_continuation);

    initialize_dynamic_extent();
    initialize_continuation_env();

    scm_gc_protect_with_init((ScmObj *)&l_trace_stack, SCM_NULL);
}

SCM_EXPORT void
scm_finalize_continuation(void)
{
    finalize_continuation_env();
    finalize_dynamic_extent();

    SCM_GLOBAL_VARS_FIN(static_continuation);
}

/*===========================================================================
  Dynamic Extent
===========================================================================*/
#define MAKE_DYNEXT_FRAME(before, after) CONS((before), (after))
#define DYNEXT_FRAME_BEFORE CAR
#define DYNEXT_FRAME_AFTER  CDR

static void
initialize_dynamic_extent(void)
{
    scm_gc_protect_with_init((ScmObj *)&l_current_dynamic_extent, SCM_NULL);
}

static void
finalize_dynamic_extent(void)
{
}

static void
wind_onto_dynamic_extent(ScmObj before, ScmObj after)
{
    l_current_dynamic_extent = CONS(MAKE_DYNEXT_FRAME(before, after),
                                  l_current_dynamic_extent);
}

static void
unwind_dynamic_extent(void)
{
    if (NULLP(l_current_dynamic_extent))
        PLAIN_ERR("corrupted dynamic extent");

    l_current_dynamic_extent = CDR(l_current_dynamic_extent);
}

/* enter a dynamic extent of another continuation (dest) */
static void
enter_dynamic_extent(ScmObj dest)
{
    ScmObj frame, unwound, retpath;
    DECLARE_INTERNAL_FUNCTION("enter_dynamic_extent");

    retpath = SCM_NULL;

    for (unwound = dest; !NULLP(unwound); unwound = CDR(unwound)) {
        if (EQ(unwound, l_current_dynamic_extent))
            break;
        frame = CAR(unwound);
        retpath = CONS(frame, retpath);
    }

    FOR_EACH (frame, retpath)
        scm_call(DYNEXT_FRAME_BEFORE(frame), SCM_NULL);
}

/* exit to a dynamic extent of another continuation (dest) */
static void
exit_dynamic_extent(ScmObj dest)
{
    ScmObj frame;

    for (;
         !NULLP(l_current_dynamic_extent);
         l_current_dynamic_extent = CDR(l_current_dynamic_extent))
    {
        if (EQ(l_current_dynamic_extent, dest))
            return;
        frame = CAR(l_current_dynamic_extent);
        scm_call(DYNEXT_FRAME_AFTER(frame), SCM_NULL);
    }
}

SCM_EXPORT ScmObj
scm_dynamic_wind(ScmObj before, ScmObj thunk, ScmObj after)
{
    ScmObj ret;

    scm_call(before, SCM_NULL);

    wind_onto_dynamic_extent(before, after);
    ret = scm_call(thunk, SCM_NULL);
    unwind_dynamic_extent();

    scm_call(after, SCM_NULL);

    return ret;
}

/*===========================================================================
  Continuation
===========================================================================*/
static void
initialize_continuation_env(void)
{
    scm_gc_protect_with_init((ScmObj *)&l_continuation_stack, SCM_NULL);
}

static void
finalize_continuation_env(void)
{
}

static void
continuation_stack_push(ScmObj cont)
{
    l_continuation_stack = CONS(cont, l_continuation_stack);
}

static ScmObj
continuation_stack_pop(void)
{
    ScmObj recentmost;

    if (!NULLP(l_continuation_stack)) {
        recentmost = CAR(l_continuation_stack);
        l_continuation_stack = CDR(l_continuation_stack);
    } else {
        recentmost = SCM_FALSE;
    }

    return recentmost;
}

/* expire all descendant continuations and dest_cont */
static ScmObj
continuation_stack_unwind(ScmObj dest_cont)
{
    ScmObj cont;

    do {
        cont = continuation_stack_pop();
        if (FALSEP(cont))
            return SCM_FALSE;
        CONTINUATION_SET_FRAME(cont, INVALID_CONTINUATION_OPAQUE);
    } while (!EQ(dest_cont, cont));

    return dest_cont;
}

SCM_EXPORT void
scm_destruct_continuation(ScmObj cont)
{
    /* no object to free(3) in this implementation */
}

SCM_EXPORT ScmObj
scm_call_with_current_continuation(ScmObj proc, ScmEvalState *eval_state)
{
    volatile ScmObj cont, ret;
    struct continuation_frame cont_frame;

    cont_frame.dyn_ext = l_current_dynamic_extent;
    cont_frame.ret_val = SCM_FALSE;
#if SCM_DEBUG
    cont_frame.trace_stack = l_trace_stack;
#endif
    cont = MAKE_CONTINUATION();
    CONTINUATION_SET_FRAME(cont, &cont_frame);
#if SCM_NESTED_CONTINUATION_ONLY
    continuation_stack_push(cont);
#endif

    if (setjmp(cont_frame.c_env)) {
        /* returned back to the original continuation */
        /*
         * Don't refer cont because it may already be invalidated by
         * continuation_stack_unwind().
         */
#if SCM_DEBUG
        l_trace_stack = cont_frame.trace_stack;
#endif

        enter_dynamic_extent(cont_frame.dyn_ext);

        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        return cont_frame.ret_val;
    } else {
#if SCM_NESTED_CONTINUATION_ONLY
        /* call proc with current continutation as (proc cont): This call must
         * not be scm_tailcall(), to preserve current stack until longjmp()
         * called.
         */
        eval_state->ret_type = SCM_VALTYPE_AS_IS;
        ret = scm_call(proc, LIST_1(cont));
#else
        /* ONLY FOR TESTING: This call is properly recursible, but all
         * continuations are broken and cannot be called, if the continuation
         * is implemented by longjmp().
         */
        ret = scm_tailcall(proc, LIST_1(cont), eval_state);
#endif

#if SCM_NESTED_CONTINUATION_ONLY
        /* the continuation expires when this function returned */
        continuation_stack_unwind(cont);
#endif
        return ret;
    }
}

SCM_EXPORT void
scm_call_continuation(ScmObj cont, ScmObj ret)
{
    struct continuation_frame *frame;
#if SCM_NESTED_CONTINUATION_ONLY
    ScmObj dst;
#endif
    DECLARE_INTERNAL_FUNCTION("scm_call_continuation");

    frame = CONTINUATION_FRAME(cont);

    if (frame != INVALID_CONTINUATION_OPAQUE
#if SCM_NESTED_CONTINUATION_ONLY
        /* assign to temporary var to avoid duplicate eval in the macro */
        && (dst = continuation_stack_unwind(cont), CONTINUATIONP(dst))
#endif
        )
    {
        if (VALUEPACKETP(ret))
            ERR_OBJ("continuations take exactly one value but got", ret);

        /*
         * Don't refer cont because it may already be invalidated by
         * continuation_stack_unwind().
         */
        exit_dynamic_extent(frame->dyn_ext);

        frame->ret_val = ret;
        longjmp(frame->c_env, 1);
        /* NOTREACHED */
    } else {
        ERR("called expired continuation");
    }
}

/*===========================================================================
  Trace Stack
===========================================================================*/
SCM_EXPORT void
scm_push_trace_frame(ScmObj obj, ScmObj env)
{
    ScmObj frame;

    frame = MAKE_TRACE_FRAME(obj, env);
    l_trace_stack = CONS(frame, l_trace_stack);
}

SCM_EXPORT void
scm_pop_trace_frame(void)
{
    l_trace_stack = CDR(l_trace_stack);
}

SCM_EXPORT ScmObj
scm_trace_stack(void)
{
    return l_trace_stack;
}
