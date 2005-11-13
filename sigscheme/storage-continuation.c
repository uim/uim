/*===========================================================================
 *  FileName : storage-continuation.c
 *  About    : A Continuation implementation with setjmp/longjmp
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
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>

/*=======================================
  Local Include
=======================================*/
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
#define CONTINUATION_SET_FRAME             SCM_CONTINUATION_SET_OPAQUE
#define CONTINUATION_JMPENV(cont)          (CONTINUATION_FRAME(cont)->env)
#define CONTINUATION_SET_JMPENV(cont, env) (CONTINUATION_JMPENV(cont) = (env))
#define CONTINUATION_DYNEXT(cont)          (CONTINUATION_FRAME(cont)->dyn_ext)
#define CONTINUATION_SET_DYNEXT(cont, dyn_ext)                               \
    ((CONTINUATION_DYNEXT(cont)) = (dyn_ext))

/*=======================================
  File Local Type Definitions
=======================================*/
struct continuation_frame {
    jmp_buf *env;
    ScmObj dyn_ext;
};

/*=======================================
  Variable Declarations
=======================================*/
/* dynamic extent */
/* FIXME: make static */
ScmObj scm_current_dynamic_extent = NULL;

/* temporary store for a object returned from a continuation */
static ScmObj continuation_thrown_obj = NULL;
static ScmObj continuation_stack = NULL;

static struct trace_frame *trace_stack = NULL;

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
void SigScm_InitContinuation(void)
{
    initialize_dynamic_extent();
    initialize_continuation_env();
    trace_stack = NULL;
}

void SigScm_FinalizeContinuation(void)
{
    finalize_continuation_env();
    finalize_dynamic_extent();
}

/*============================================================================
  Dynamic Extent
============================================================================*/
#define MAKE_DYNEXT_FRAME(before, after) (CONS(before, after))
#define DYNEXT_FRAME_BEFORE CAR
#define DYNEXT_FRAME_AFTER  CDR

static void initialize_dynamic_extent(void)
{
    scm_current_dynamic_extent = SCM_NULL;
    SigScm_GC_Protect(&scm_current_dynamic_extent);
}

static void finalize_dynamic_extent(void)
{
}

static void wind_onto_dynamic_extent(ScmObj before, ScmObj after)
{
    scm_current_dynamic_extent = CONS(MAKE_DYNEXT_FRAME(before, after),
                                      scm_current_dynamic_extent);
}

static void unwind_dynamic_extent(void)
{
    if (NULLP(scm_current_dynamic_extent))
        SigScm_Error("corrupted dynamic extent");

    scm_current_dynamic_extent = CDR(scm_current_dynamic_extent);
}

/* enter a dynamic extent of another continuation (dest) */
static void enter_dynamic_extent(ScmObj dest)
{
    ScmObj frame   = SCM_FALSE;
    ScmObj unwound = SCM_FALSE;
    ScmObj retpath = SCM_NULL;

    for (unwound = dest; !NULLP(unwound); unwound = CDR(unwound)) {
        if (EQ(unwound, scm_current_dynamic_extent))
            break;
        frame = CAR(unwound);
        retpath = CONS(frame, retpath);
    }

    /* assumes that (SCM_NULL != NULL) */
    while (SCM_SHIFT_RAW(frame, retpath)) {
        Scm_call(DYNEXT_FRAME_BEFORE(frame), SCM_NULL);
    }
}

/* exit to a dynamic extent of another continuation (dest) */
static void exit_dynamic_extent(ScmObj dest)
{
    ScmObj frame = SCM_FALSE;

    for (;
         !NULLP(scm_current_dynamic_extent);
         scm_current_dynamic_extent = CDR(scm_current_dynamic_extent))
    {
        if (EQ(scm_current_dynamic_extent, dest))
            return;
        frame = CAR(scm_current_dynamic_extent);
        Scm_call(DYNEXT_FRAME_AFTER(frame), SCM_NULL);
    }
}

ScmObj Scm_DynamicWind(ScmObj before, ScmObj thunk, ScmObj after)
{
    ScmObj ret   = SCM_FALSE;

    Scm_call(before, SCM_NULL);
    
    wind_onto_dynamic_extent(before, after);
    ret = Scm_call(thunk, SCM_NULL);
    unwind_dynamic_extent();

    Scm_call(after, SCM_NULL);

    return ret;
}

/*============================================================================
  Continuation
============================================================================*/
static void initialize_continuation_env(void)
{
    continuation_thrown_obj = SCM_FALSE;
    continuation_stack = SCM_NULL;
    SigScm_GC_Protect(&continuation_thrown_obj);
    SigScm_GC_Protect(&continuation_stack);
}

static void finalize_continuation_env(void)
{
}

static void continuation_stack_push(ScmObj cont)
{
    continuation_stack = CONS(cont, continuation_stack);
}

static ScmObj continuation_stack_pop(void)
{
    ScmObj recentmost = SCM_FALSE;

    if (!NULLP(continuation_stack)) {
        recentmost = CAR(continuation_stack);
        continuation_stack = CDR(continuation_stack);
    }

    return recentmost;
}

/* expire all descendant continuations and dest_cont */
static ScmObj continuation_stack_unwind(ScmObj dest_cont)
{
    ScmObj cont = SCM_FALSE;

    do {
        cont = continuation_stack_pop();
        if (FALSEP(cont))
            return SCM_FALSE;
        CONTINUATION_SET_FRAME(cont, INVALID_CONTINUATION_OPAQUE);
    } while (!EQ(dest_cont, cont));

    return dest_cont;
}

void Scm_DestructContinuation(ScmObj cont)
{
    /* no object to free(3) in this implementation */
}

ScmObj Scm_CallWithCurrentContinuation(ScmObj proc, ScmEvalState *eval_state)
{
    jmp_buf env;
    ScmObj cont = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;
    struct continuation_frame cont_frame;
    struct trace_frame *saved_trace_stack;

    cont = Scm_NewContinuation();
    CONTINUATION_SET_FRAME(cont, &cont_frame);
    CONTINUATION_SET_JMPENV(cont, &env);
    CONTINUATION_SET_DYNEXT(cont, scm_current_dynamic_extent);
#if SCM_NESTED_CONTINUATION_ONLY
    continuation_stack_push(cont);
#endif

    if (setjmp(env)) {
        /* returned from longjmp */
        /*
         * Don't refer cont because it may already be invalidated by
         * continuation_stack_unwind().
         */
        ret = continuation_thrown_obj;
        continuation_thrown_obj = SCM_FALSE;  /* make ret sweepable */
        trace_stack = saved_trace_stack;

        enter_dynamic_extent(cont_frame.dyn_ext);

        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        return ret;
    } else {
        saved_trace_stack = trace_stack;
#if SCM_NESTED_CONTINUATION_ONLY
        /* call proc with current continutation as (proc cont): This call must
         * not be Scm_tailcall(), to preserve current stack until longjmp()
         * called.
         */
        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        ret = Scm_call(proc, LIST_1(cont));
#else
        /* ONLY FOR TESTING: This call is properly recursible, but all
         * continuations are broken and cannot be called, if the continuation
         * is implemented by longjmp().
         */
        ret = Scm_tailcall(proc, LIST_1(cont), eval_state);
#endif

#if SCM_NESTED_CONTINUATION_ONLY
        /* the continuation expires when this function returned */
        continuation_stack_unwind(cont);
#endif
        return ret;
    }
}

void Scm_CallContinuation(ScmObj cont, ScmObj ret)
{
    struct continuation_frame *frame;

    frame = CONTINUATION_FRAME(cont);

    if (frame != INVALID_CONTINUATION_OPAQUE
#if SCM_NESTED_CONTINUATION_ONLY
        && CONTINUATIONP(continuation_stack_unwind(cont))
#endif
        )
    {
        /*
         * Don't refer cont because it may already be invalidated by
         * continuation_stack_unwind().
         */
        exit_dynamic_extent(frame->dyn_ext);

        continuation_thrown_obj = ret;
        longjmp(*frame->env, 1);
        /* NOTREACHED */
    } else {
        ERR("Scm_CallContinuation: called expired continuation");
    }
}

/*============================================================================
  Trace Stack
============================================================================*/
void Scm_PushTraceFrame(struct trace_frame *frame, ScmObj obj, ScmObj env)
{
    frame->prev = trace_stack;
    frame->obj  = obj;
    frame->env  = env;
    trace_stack = frame;
}

void Scm_PopTraceFrame(void)
{
    trace_stack = trace_stack->prev;
}

const struct trace_frame *Scm_TraceStack(void)
{
    return trace_stack;
}
