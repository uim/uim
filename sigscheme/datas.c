/*===========================================================================
 *  FileName : datas.c
 *  About    : Data Allocation
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
#include "encoding.h"

#if !SCM_OBJ_COMPACT
#if (SCM_CHARCELL_SIZE <= SCM_MB_MAX_LEN)
#error
#error "SCM_MB_MAX_LEN is exceeded design limit"
#endif
#endif /* !SCM_OBJ_COMPACT */

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/
/* specifies whether the storage abstraction layer can only handle nested
 * (stacked) continuation or R5RS-conformant full implementation. But current
 * implementation only supports '1'.
 */
#define SCM_NESTED_CONTINUATION_ONLY 1

#define INVALID_CONTINUATION_JMPENV  NULL

#define CONTINUATION_JMPENV          SCM_CONTINUATION_OPAQUE0
#define CONTINUATION_SET_JMPENV      SCM_CONTINUATION_SET_OPAQUE0
#define CONTINUATION_DYNEXT          SCM_CONTINUATION_OPAQUE1
#define CONTINUATION_SET_DYNEXT      SCM_CONTINUATION_SET_OPAQUE1

/*=======================================
  Variable Declarations
=======================================*/
/* multiple values */
#if SCM_USE_VALUECONS
ScmObj SigScm_null_values;
#endif

/* dynamic extent */
static ScmObj current_dynamic_extent = NULL;

/* temporary store for a object returned from a continuation */
static ScmObj continuation_thrown_obj = NULL;
static ScmObj continuation_stack = NULL;

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

#if !SCM_USE_NEWPORT
/* port */
static int  fileport_getc(ScmObj port);
static void fileport_print(ScmObj port, const char *str);
static int  stringport_getc(ScmObj port);
static void stringport_print(ScmObj port, const char *str);
#endif /* SCM_USE_NEWPORT */

/* continuation */
static void initialize_continuation_env(void);
static void finalize_continuation_env(void);
static void continuation_stack_push(ScmObj cont);
static ScmObj continuation_stack_pop(void);
static ScmObj continuation_stack_unwind(ScmObj dest_cont);

/*=======================================
  Function Implementations
=======================================*/

/* FIXME: should be renamed? */
void SigScm_InitStorage(void)
{
    scm_portbuffer = (char*)malloc(sizeof(char) * PORTBUFFER_SIZE + 1);

#if SCM_USE_VALUECONS
    /*
     * To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell
     */
    SigScm_null_values = CONS(SCM_NULL, SCM_NULL);
    SCM_ENTYPE_VALUEPACKET(SigScm_null_values);
    SigScm_GC_Protect(&SigScm_null_values);
#endif
    initialize_dynamic_extent();
    initialize_continuation_env();
}

void SigScm_FinalizeStorage(void)
{
    finalize_continuation_env();
    finalize_dynamic_extent();
    free(scm_portbuffer);
}

/*===========================================================================
  Object Allocators
===========================================================================*/
ScmObj Scm_NewCons(ScmObj a, ScmObj b)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_CONS(obj);
    SET_CAR(obj, a);
    SET_CDR(obj, b);

    return obj;
}

ScmObj Scm_NewInt(int val)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_INT(obj);
    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj Scm_NewSymbol(char *name, ScmObj v_cell)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_SYMBOL(obj);
    SCM_SYMBOL_SET_NAME(obj, name);
    SCM_SYMBOL_SET_VCELL(obj, v_cell);

    return obj;
}

ScmObj Scm_NewChar(char *ch)
{
    ScmObj obj = SigScm_NewObj();
    int len;

    len = Scm_mb_bare_c_strlen(ch);
    if (len > SCM_MB_MAX_LEN) {
        SigScm_Error("Scm_NewChar : invalid character ch = [%s], len = %d",
                     ch, len);
    }

    SCM_ENTYPE_CHAR(obj);
    SCM_CHAR_SET_VALUE(obj, ch);

    return obj;
}

ScmObj Scm_NewString(char *str)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, str ? Scm_mb_bare_c_strlen(str) : 0);

    return obj;
}

ScmObj Scm_NewStringCopying(const char *str)
{
    ScmObj obj = SigScm_NewObj();

    if (!str) str = "";

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, strdup(str));
    SCM_STRING_SET_LEN(obj, Scm_mb_bare_c_strlen(str));

    return obj;
}

ScmObj Scm_NewStringWithLen(char *str, int len)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_FUNC(obj);
    SCM_FUNC_SET_TYPECODE(obj, type);
    SCM_FUNC_SET_CFUNC(obj, func);

    return obj;
}

ScmObj Scm_NewClosure(ScmObj exp, ScmObj env)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_CLOSURE(obj);
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_CLOSURE_SET_ENV(obj, env);

    return obj;
}

ScmObj Scm_NewVector(ScmObj *vec, int len)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_VECTOR(obj);
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_VECTOR_SET_LEN(obj, len);

    return obj;
}

#if SCM_USE_NEWPORT
ScmObj Scm_NewPort(ScmCharPort *cport, enum ScmPortFlag flag)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_PORT(obj);

    if (flag & SCM_PORTFLAG_INPUT)
        flag |= SCM_PORTFLAG_LIVE_INPUT;
    if (flag & SCM_PORTFLAG_OUTPUT)
        flag |= SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(obj, flag);

    SCM_PORT_SET_IMPL(obj, cport);

    return obj;
}

#else /* SCM_USE_NEWPORT */

ScmObj Scm_NewFilePort(FILE *file, const char *filename,
                       enum ScmPortDirection pdirection)
{
    ScmObj obj = SigScm_NewObj();
    ScmPortInfo *pinfo = (ScmPortInfo *)malloc(sizeof(ScmPortInfo));

    SCM_ENTYPE_PORT(obj);
    SCM_PORT_SET_PORTDIRECTION(obj, pdirection);

    SCM_PORT_SET_PORTINFO(obj, pinfo);
    SCM_PORT_SET_PORTTYPE(obj, PORT_FILE);
    SCM_PORT_SET_FILE(obj, file);
    SCM_PORT_SET_FILENAME(obj, strdup(filename));
    SCM_PORT_SET_LINE(obj, 0);
    SCM_PORT_SET_GETC_FUNC(obj, fileport_getc);
    SCM_PORT_SET_PRINT_FUNC(obj, fileport_print);
    SCM_PORT_SET_UNGOTTENCHAR(obj, 0);

    return obj;
}

static int fileport_getc(ScmObj port)
{
    int c = SCM_PORT_UNGOTTENCHAR(port);
    if (!c) {
        c = fgetc(SCM_PORT_FILE(port));
        if (c == '\n')
            SCM_PORT_LINE(port)++;
    }

    SCM_PORT_SET_UNGOTTENCHAR(port, 0);
    return c;
}

static void fileport_print(ScmObj port, const char *str)
{
    fputs(str, SCM_PORT_FILE(port));
}

ScmObj Scm_NewStringPort(const char *str, enum ScmPortDirection pdirection)
{
    ScmObj obj = SigScm_NewObj();
    ScmPortInfo *pinfo = (ScmPortInfo *)malloc(sizeof(ScmPortInfo));

    SCM_ENTYPE_PORT(obj);
    SCM_PORT_SET_PORTDIRECTION(obj, pdirection);
    
    SCM_PORT_SET_PORTINFO(obj, pinfo);
    SCM_PORT_SET_PORTTYPE(obj, PORT_STRING);
    if (str)
        SCM_PORT_SET_STR(obj, strdup(str));
    else
        SCM_PORT_SET_STR(obj, NULL);
    SCM_PORT_SET_STR_CURRENTPOS(obj, SCM_PORT_STR(obj));
    SCM_PORT_SET_GETC_FUNC(obj, stringport_getc);
    SCM_PORT_SET_PRINT_FUNC(obj, stringport_print);
    SCM_PORT_SET_UNGOTTENCHAR(obj, 0);

    return obj;
}

static int stringport_getc(ScmObj port)
{
    int c = SCM_PORT_UNGOTTENCHAR(port);
    if (!c) {
        c = (*SCM_PORT_STR_CURRENTPOS(port));
        if (c == '\0')
            c = EOF;
        SCM_PORT_STR_CURRENTPOS(port)++;
    }

    SCM_PORT_SET_UNGOTTENCHAR(port, 0);
    return c;
}

static void stringport_print(ScmObj port, const char *str)
{
    char *p = NULL;
    char *str_start = SCM_PORT_STR(port);
    int len_delta  = strlen(str);
    int old_len    = 0;
    int new_len    = 0;

    if (str_start)
        old_len = SCM_PORT_STR_CURRENTPOS(port) - str_start;
    else
        old_len = 0;

    new_len = old_len + len_delta;

    p = (char *)realloc(str_start, new_len + 1);
    memcpy(p + old_len, str, len_delta + 1); /* Copy '\0' as well. */

    SCM_PORT_SET_STR(port, p);
    SCM_PORT_SET_STR_CURRENTPOS(port, p + new_len);
}
#endif /* SCM_USE_NEWPORT */

ScmObj Scm_NewContinuation(void)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_CONTINUATION(obj);
    CONTINUATION_SET_JMPENV(obj, INVALID_CONTINUATION_JMPENV);
    CONTINUATION_SET_DYNEXT(obj, current_dynamic_extent);

    return obj;
}

#if !SCM_USE_VALUECONS
ScmObj Scm_NewValuePacket(ScmObj values)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_VALUEPACKET_SET_VALUES(obj, values);

    return obj;
}
#endif

#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_C_POINTER(obj);
    SCM_C_POINTER_SET_VALUE(obj, data);

    return obj;
}

ScmObj Scm_NewCFuncPointer(ScmCFunc func)
{
    ScmObj obj = SigScm_NewObj();

    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_C_FUNCPOINTER_SET_VALUE(obj, func);

    return obj;
}
#endif /* SCM_USE_NONSTD_FEATURES */

/*============================================================================
  Dynamic Extent
============================================================================*/
#define MAKE_DYNEXT_FRAME(before, after) (CONS(before, after))
#define DYNEXT_FRAME_BEFORE CAR
#define DYNEXT_FRAME_AFTER  CDR

static void initialize_dynamic_extent(void)
{
    current_dynamic_extent = SCM_NULL;
    SigScm_GC_Protect(&current_dynamic_extent);
}

static void finalize_dynamic_extent(void)
{
}

static void wind_onto_dynamic_extent(ScmObj before, ScmObj after)
{
    current_dynamic_extent = CONS(MAKE_DYNEXT_FRAME(before, after),
                                  current_dynamic_extent);
}

static void unwind_dynamic_extent(void)
{
    if (NULLP(current_dynamic_extent))
        SigScm_Error("corrupted dynamic extent");

    current_dynamic_extent = CDR(current_dynamic_extent);
}

/* enter a dynamic extent of another continuation (dest) */
static void enter_dynamic_extent(ScmObj dest)
{
    ScmObj frame   = SCM_FALSE;
    ScmObj unwound = SCM_FALSE;
    ScmObj retpath = SCM_NULL;

    for (unwound = dest; !NULLP(unwound); unwound = CDR(unwound)) {
        if (EQ(unwound, current_dynamic_extent))
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
         !NULLP(current_dynamic_extent);
         current_dynamic_extent = CDR(current_dynamic_extent))
    {
        if (EQ(current_dynamic_extent, dest))
            return;
        frame = CAR(current_dynamic_extent);
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
        CONTINUATION_SET_JMPENV(cont, INVALID_CONTINUATION_JMPENV);
    } while (!EQ(dest_cont, cont));

    return dest_cont;
}

ScmObj Scm_CallWithCurrentContinuation(ScmObj proc, ScmEvalState *eval_state)
{
    jmp_buf env;
    ScmObj cont = SCM_FALSE;
    ScmObj ret  = SCM_FALSE;

    cont = Scm_NewContinuation();
    CONTINUATION_SET_JMPENV(cont, &env);
#if SCM_NESTED_CONTINUATION_ONLY
    continuation_stack_push(cont);
#endif

    if (setjmp(env)) {
        /* returned from longjmp */
        ret = continuation_thrown_obj;
        continuation_thrown_obj = SCM_FALSE;  /* make ret sweepable */

        enter_dynamic_extent(CONTINUATION_DYNEXT(cont));

        eval_state->ret_type = SCM_RETTYPE_AS_IS;
        return ret;
    } else {
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
    jmp_buf *env;

    env = CONTINUATION_JMPENV(cont);

    if (env != INVALID_CONTINUATION_JMPENV
#if SCM_NESTED_CONTINUATION_ONLY
        && CONTINUATIONP(continuation_stack_unwind(cont))
#endif
        )
    {
        exit_dynamic_extent(CONTINUATION_DYNEXT(cont));

        continuation_thrown_obj = ret;
        longjmp(*env, 1);
        /* NOTREACHED */
    } else {
        ERR("Scm_CallContinuation: called expired continuation");
    }
}

