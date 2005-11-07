/*===========================================================================
 *  FileName : storage.c
 *  About    : scheme storage layer
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
#if !SCM_OBJ_COMPACT
/* special constant initialization */
#define SCM_CONSTANT_BIND_SUBSTANCE(obj, cell)                          \
    do {                                                                \
        (obj) = &(cell);                                                \
        SCM_ENTYPE((obj), ScmConstant);                                 \
    } while(/* CONSTCOND */ 0)
#endif /* SCM_OBJ_COMPACT */

/*=======================================
  Variable Declarations
=======================================*/
/* multiple values */
#if SCM_USE_VALUECONS
ScmObj SigScm_null_values;
#endif

#if !SCM_OBJ_COMPACT
/* SCM_OBJ_COMPACT MUST NOT refer these variables. Use SCM_NULL and so on. */

/* constants */
ScmObj SigScm_null, SigScm_true, SigScm_false, SigScm_eof;
ScmObj SigScm_unbound, SigScm_undef;
static ScmCell null_cell, true_cell, false_cell, eof_cell;
static ScmCell unbound_cell, undef_cell;
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_special_constants(void);

/*=======================================
  Function Implementations
=======================================*/
void SigScm_InitStorage(void)
{
    initialize_special_constants();

    SigScm_InitGC();

#if 0 && (SCM_COMPAT_SIOD_BUGS && !SCM_OBJ_COMPACT)
    SigScm_GC_Protect(&SigScm_true);
    SigScm_true = Scm_NewInt(1);
#endif

#if SCM_USE_VALUECONS
    /*
     * To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell
     */
    SigScm_null_values = CONS(SCM_NULL, SCM_NULL);
    SCM_ENTYPE_VALUEPACKET(SigScm_null_values);
    SigScm_GC_Protect(&SigScm_null_values);
#endif

    SigScm_InitContinuation();
    SigScm_InitSymbol();
}

void SigScm_FinalizeStorage(void)
{
    SigScm_FinalizeSymbol();
    SigScm_FinalizeContinuation();
    SigScm_FinalizeGC();
}

/*===========================================================================
  Scheme Constants
===========================================================================*/
/*
 * To keep storage representation abstract, the special constants
 * initialization is encapsulated in this file. Upper layers must only use
 * abstract interfaces such as SCM_NULL and SCM_NULLP().
 */
static void initialize_special_constants(void)
{
#if !SCM_OBJ_COMPACT
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_null,    null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_true,    true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_false,   false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_eof,     eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_unbound, unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(SigScm_undef,   undef_cell);

#if SCM_COMPAT_SIOD_BUGS
    SigScm_false = SigScm_null;
#endif /* SCM_COMPAT_SIOD_BUGS */
#endif /* !SCM_OBJ_COMPACT */
}

/*===========================================================================
  Object Allocators
===========================================================================*/
ScmObj Scm_NewCons(ScmObj a, ScmObj b)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_CONS(obj);
    SET_CAR(obj, a);
    SET_CDR(obj, b);

    return obj;
}

ScmObj Scm_NewInt(int val)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_INT(obj);
    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj Scm_NewSymbol(char *name, ScmObj v_cell)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_SYMBOL(obj);
    SCM_SYMBOL_SET_NAME(obj, name);
    SCM_SYMBOL_SET_VCELL(obj, v_cell);

    return obj;
}

ScmObj Scm_NewChar(char *ch)
{
    ScmObj obj = SigScm_NewObjFromHeap();
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
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, str ? Scm_mb_bare_c_strlen(str) : 0);

    return obj;
}

ScmObj Scm_NewStringCopying(const char *str)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    if (!str) str = "";

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, strdup(str));
    SCM_STRING_SET_LEN(obj, Scm_mb_bare_c_strlen(str));

    return obj;
}

ScmObj Scm_NewStringWithLen(char *str, int len)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewFunc(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_FUNC(obj);
    SCM_FUNC_SET_TYPECODE(obj, type);
    SCM_FUNC_SET_CFUNC(obj, func);

    return obj;
}

ScmObj Scm_NewClosure(ScmObj exp, ScmObj env)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_CLOSURE(obj);
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_CLOSURE_SET_ENV(obj, env);

    return obj;
}

ScmObj Scm_NewVector(ScmObj *vec, int len)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_VECTOR(obj);
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_VECTOR_SET_LEN(obj, len);

    return obj;
}

ScmObj Scm_NewPort(ScmCharPort *cport, enum ScmPortFlag flag)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_PORT(obj);

    if (flag & SCM_PORTFLAG_INPUT)
        flag |= SCM_PORTFLAG_LIVE_INPUT;
    if (flag & SCM_PORTFLAG_OUTPUT)
        flag |= SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(obj, flag);

    SCM_PORT_SET_IMPL(obj, cport);

    return obj;
}

ScmObj Scm_NewContinuation(void)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_CONTINUATION(obj);
    SCM_CONTINUATION_SET_OPAQUE(obj, INVALID_CONTINUATION_OPAQUE);
    SCM_CONTINUATION_SET_TAG(obj, 0);

    return obj;
}

#if !SCM_USE_VALUECONS
ScmObj Scm_NewValuePacket(ScmObj values)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_VALUEPACKET_SET_VALUES(obj, values);

    return obj;
}
#endif

#if SCM_USE_NONSTD_FEATURES
ScmObj Scm_NewCPointer(void *data)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_C_POINTER(obj);
    SCM_C_POINTER_SET_VALUE(obj, data);

    return obj;
}

ScmObj Scm_NewCFuncPointer(ScmCFunc func)
{
    ScmObj obj = SigScm_NewObjFromHeap();

    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_C_FUNCPOINTER_SET_VALUE(obj, func);

    return obj;
}
#endif /* SCM_USE_NONSTD_FEATURES */
