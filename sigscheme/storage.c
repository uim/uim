/*===========================================================================
 *  FileName : storage.c
 *  About    : scheme storage layer
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
#include <string.h>
#include <stdlib.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "encoding.h"

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
ScmObj scm_null_values;
#endif

#if !SCM_OBJ_COMPACT
/* SCM_OBJ_COMPACT MUST NOT refer these variables. Use SCM_NULL and so on. */

/* constants */
ScmObj scm_const_null, scm_const_true, scm_const_false, scm_const_eof;
ScmObj scm_const_unbound, scm_const_undef;
static ScmCell null_cell, true_cell, false_cell, eof_cell;
static ScmCell unbound_cell, undef_cell;
#endif

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_special_constants(void);
static ScmObj scm_make_string_internal(char *str, scm_bool is_immutable);

/*=======================================
  Function Implementations
=======================================*/
void
scm_init_storage(size_t heap_size, size_t heap_alloc_threshold,
                 int n_heaps_max, int n_heaps_init)
{
    initialize_special_constants();

    scm_init_gc(heap_size, heap_alloc_threshold, n_heaps_max, n_heaps_init);

#if 0 && (SCM_COMPAT_SIOD_BUGS && !SCM_OBJ_COMPACT)
    scm_gc_protect_with_init(&scm_const_true, MAKE_INT(1));
#endif

#if SCM_USE_VALUECONS
    /*
     * To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell
     */
    scm_gc_protect_with_init(&scm_null_values, CONS(SCM_NULL, SCM_NULL));
    SCM_ENTYPE_VALUEPACKET(scm_null_values);
#endif

    scm_init_continuation();
    scm_init_symbol();
}

void
scm_finalize_storage(void)
{
    scm_finalize_symbol();
    scm_finalize_continuation();
    scm_finalize_gc();
}

/*===========================================================================
  Scheme Constants
===========================================================================*/
/*
 * To keep storage representation abstract, the special constants
 * initialization is encapsulated in this file. Upper layers must only use
 * abstract interfaces such as SCM_NULL and SCM_NULLP().
 */
static void
initialize_special_constants(void)
{
#if !SCM_OBJ_COMPACT
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_null,    null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_true,    true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_false,   false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_eof,     eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_unbound, unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_undef,   undef_cell);

#if SCM_COMPAT_SIOD_BUGS
    scm_const_false = scm_const_null;
#endif /* SCM_COMPAT_SIOD_BUGS */
#endif /* !SCM_OBJ_COMPACT */
}

/*===========================================================================
  Object Allocators
===========================================================================*/
ScmObj
scm_make_cons(ScmObj kar, ScmObj kdr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_CONS(obj);
    SET_CAR(obj, kar);
    SET_CDR(obj, kdr);

    return obj;
}

ScmObj
scm_make_int(int val)
{
    ScmObj obj;

#if !SCM_OBJ_COMPACT
    obj = scm_alloc_cell();
#endif
    SCM_ENTYPE_INT(obj);
    SCM_INT_SET_VALUE(obj, val);

    return obj;
}

ScmObj
scm_make_symbol(char *name, ScmObj val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_SYMBOL(obj);
    SCM_SYMBOL_SET_NAME(obj, name);
    SCM_SYMBOL_SET_VCELL(obj, val);

    return obj;
}

ScmObj
scm_make_char(int val)
{
    ScmObj obj;

#if !SCM_OBJ_COMPACT
    obj = scm_alloc_cell();
#endif
    SCM_ENTYPE_CHAR(obj);
    SCM_CHAR_SET_VALUE(obj, val);

    return obj;
}

static ScmObj
scm_make_string_internal(char *str, scm_bool is_immutable)
{
    ScmObj obj;

    SCM_ASSERT(str);

    obj = scm_alloc_cell();
    SCM_ENTYPE_STRING(obj);
    SCM_STRING_SET_STR(obj, str);
    SCM_STRING_SET_LEN(obj, (*str) ? scm_mb_bare_c_strlen(str) : 0);

    if (is_immutable)
        SCM_STRING_SET_IMMUTABLE(obj);
    else
        SCM_STRING_SET_MUTABLE(obj);

    return obj;
}

ScmObj
scm_make_immutable_string(char *str)
{
    return scm_make_string_internal(str, scm_true);
}

ScmObj
scm_make_immutable_string_copying(const char *str)
{
    return scm_make_string_internal(strdup(str), scm_true);
}

ScmObj
scm_make_string(char *str)
{
    return scm_make_string_internal(str, scm_false);
}

ScmObj
scm_make_string_copying(const char *str)
{
    return scm_make_string_internal(strdup(str), scm_false);
}

ScmObj
scm_make_func(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_FUNC(obj);
    SCM_FUNC_SET_TYPECODE(obj, type);
    SCM_FUNC_SET_CFUNC(obj, func);

    return obj;
}

ScmObj
scm_make_closure(ScmObj exp, ScmObj env)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_CLOSURE(obj);
    SCM_CLOSURE_SET_EXP(obj, exp);
    SCM_CLOSURE_SET_ENV(obj, env);

    return obj;
}

ScmObj
scm_make_vector(ScmObj *vec, int len)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_VECTOR(obj);
    SCM_VECTOR_SET_VEC(obj, vec);
    SCM_VECTOR_SET_LEN(obj, len);

    return obj;
}

ScmObj
scm_make_port(ScmCharPort *cport, enum ScmPortFlag flag)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_PORT(obj);

    if (flag & SCM_PORTFLAG_INPUT)
        flag |= SCM_PORTFLAG_LIVE_INPUT;
    if (flag & SCM_PORTFLAG_OUTPUT)
        flag |= SCM_PORTFLAG_LIVE_OUTPUT;
    SCM_PORT_SET_FLAG(obj, flag);

    SCM_PORT_SET_IMPL(obj, cport);

    return obj;
}

ScmObj
scm_make_continuation(void)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_CONTINUATION(obj);
    SCM_CONTINUATION_SET_OPAQUE(obj, INVALID_CONTINUATION_OPAQUE);
    SCM_CONTINUATION_SET_TAG(obj, 0);

    return obj;
}

#if !SCM_USE_VALUECONS
ScmObj
scm_make_value_packet(ScmObj values)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_VALUEPACKET(obj);
    SCM_VALUEPACKET_SET_VALUES(obj, values);

    return obj;
}
#endif

#if SCM_USE_NONSTD_FEATURES
ScmObj
scm_make_cpointer(void *ptr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_C_POINTER(obj);
    SCM_C_POINTER_SET_VALUE(obj, ptr);

    return obj;
}

ScmObj
scm_make_cfunc_pointer(ScmCFunc ptr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_ENTYPE_C_FUNCPOINTER(obj);
    SCM_C_FUNCPOINTER_SET_VALUE(obj, ptr);

    return obj;
}
#endif /* SCM_USE_NONSTD_FEATURES */

#if SCM_OBJ_COMPACT
enum ScmObjType
scm_type(ScmObj obj)
{
    unsigned int tag = SCM_TAG(obj);
    switch (tag) {
    case SCM_TAG_CONS:
        return ScmCons;

    case SCM_TAG_CLOSURE:
        return ScmClosure;

    case SCM_TAG_OTHERS:
        if (SYMBOLP(obj))
            return ScmSymbol;
        else if (STRINGP(obj))
            return ScmString;
        else if (VECTORP(obj))
            return ScmVector;
        else if (VALUEPACKETP(obj))
            return ScmValuePacket;
        else if (FUNCP(obj))
            return ScmFunc;
        else if (PORTP(obj))
            return ScmPort;
        else if (CONTINUATIONP(obj))
            return ScmContinuation;
        else if (SCM_CONSTANTP(obj))
            return ScmConstant;
        else if (C_POINTERP(obj))
            return ScmCPointer;
        else if (C_FUNCPOINTERP(obj))
            return ScmCFuncPointer;
        else if (FREECELLP(obj))
            return ScmFreeCell;
        ERR("invalid others object: ptr = %p", (void*)obj);

    case SCM_TAG_IMM:
        if (INTP(obj))
            return ScmInt;
        else if (CHARP(obj))
            return ScmChar;
        else if (SCM_CONSTANTP(obj))
            return ScmConstant;
        ERR("invalid imm object: ptr = %p", (void*)obj);

    default:
        ERR("invalid object: ptr = %p", (void*)obj);
    }

    /* NOTREACHED */
}
#endif /* SCM_OBJ_COMPACT */
