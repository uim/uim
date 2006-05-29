/*===========================================================================
 *  Filename : storage.c
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

#if !SCM_USE_MULTIBYTE_CHAR
#include <string.h>
#endif
#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#include "encoding.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#if SCM_USE_STORAGE_FATTY
/* special constant initialization */
#define SCM_CONSTANT_BIND_SUBSTANCE(obj, cell)                               \
    do {                                                                     \
        (obj) = &(cell);                                                     \
        SCM_ENTYPE((obj), ScmConstant);                                      \
    } while (/* CONSTCOND */ 0)
#endif /* SCM_USE_STORAGE_FATTY */

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Definitions
=======================================*/
/* multiple values */
#if SCM_USE_VALUECONS
SCM_DEFINE_EXPORTED_VARS(storage);
#endif

#if SCM_USE_STORAGE_FATTY
/* constants */
SCM_DEFINE_EXPORTED_VARS(storage_fatty);

/* constant substances */
SCM_GLOBAL_VARS_BEGIN(static_storage_fatty);
#define static
static ScmCell l_null_cell, l_true_cell, l_false_cell, l_eof_cell;
static ScmCell l_unbound_cell, l_undef_cell;
#undef static
SCM_GLOBAL_VARS_END(static_storage_fatty);
#define l_null_cell    SCM_GLOBAL_VAR(static_storage_fatty, l_null_cell)
#define l_true_cell    SCM_GLOBAL_VAR(static_storage_fatty, l_true_cell)
#define l_false_cell   SCM_GLOBAL_VAR(static_storage_fatty, l_false_cell)
#define l_eof_cell     SCM_GLOBAL_VAR(static_storage_fatty, l_eof_cell)
#define l_unbound_cell SCM_GLOBAL_VAR(static_storage_fatty, l_unbound_cell)
#define l_undef_cell   SCM_GLOBAL_VAR(static_storage_fatty, l_undef_cell)
SCM_DEFINE_STATIC_VARS(static_storage_fatty);
#endif /* SCM_USE_STORAGE_FATTY */

static const ScmStorageConf default_storage_conf = {
    SCM_DEFAULT_HEAP_SIZE,
    SCM_DEFAULT_HEAP_ALLOC_THRESHOLD,
    SCM_DEFAULT_N_HEAPS_MAX,
    SCM_DEFAULT_N_HEAPS_INIT,
    SCM_DEFAULT_SYMBOL_HASH_SIZE
};

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_special_constants(void);
static ScmObj scm_make_string_internal(char *str, scm_int_t len,
                                       scm_bool is_immutable);

/*=======================================
  Function Definitions
=======================================*/
SCM_EXPORT void
scm_init_storage(const ScmStorageConf *conf)
{
#if SCM_USE_VALUECONS
    SCM_GLOBAL_VARS_INIT(storage);
#endif
#if SCM_USE_STORAGE_FATTY
    SCM_GLOBAL_VARS_INIT(storage_fatty);
    SCM_GLOBAL_VARS_INIT(static_storage_fatty);
#endif

    if (!conf)
        conf = &default_storage_conf;

    initialize_special_constants();

    scm_init_gc(conf);

#if 0 && (SCM_COMPAT_SIOD_BUGS && SCM_USE_STORAGE_FATTY)
    scm_gc_protect_with_init(&scm_const_true, MAKE_INT(1));
#endif

#if SCM_USE_VALUECONS
    /*
     * To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell
     */
    scm_gc_protect_with_init(&scm_null_values, CONS(SCM_NULL, SCM_NULL));
    SCM_ENTYPE(scm_null_values, ScmValuePacket);
#endif

    scm_init_continuation();
    scm_init_symbol(conf);
}

SCM_EXPORT void
scm_fin_storage(void)
{
    scm_fin_symbol();
    scm_fin_continuation();
    scm_fin_gc();

    SCM_GLOBAL_VARS_FIN(storage);
#if SCM_USE_STORAGE_FATTY
    SCM_GLOBAL_VARS_FIN(storage_fatty);
    SCM_GLOBAL_VARS_FIN(static_storage_fatty);
#endif
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
#if SCM_USE_STORAGE_FATTY
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_null,    l_null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_true,    l_true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_false,   l_false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_eof,     l_eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_unbound, l_unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_undef,   l_undef_cell);
#endif /* SCM_USE_STORAGE_FATTY */
}

/*===========================================================================
  Object Allocators
===========================================================================*/
SCM_EXPORT ScmObj
scm_make_cons(ScmObj kar, ScmObj kdr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CONS_INIT(obj, kar, kdr);
    return obj;
}

#if SCM_SAL_HAS_IMMUTABLE_CONS
SCM_EXPORT ScmObj
scm_make_immutable_cons(ScmObj kar, ScmObj kdr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_IMMUTABLE_CONS_INIT(obj, kar, kdr);
    return obj;
}
#endif /* has immutable cons */

#if !SCM_SAL_HAS_IMMEDIATE_INT_ONLY
SCM_EXPORT ScmObj
scm_make_int(scm_int_t val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_INT_INIT(obj, val);
    return obj;
}
#endif /* not SCM_SAL_HAS_IMMEDIATE_INT_ONLY */

SCM_EXPORT ScmObj
scm_make_symbol(char *name, ScmObj val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_SYMBOL_INIT(obj, name, val);
    return obj;
}

#if !SCM_SAL_HAS_IMMEDIATE_CHAR_ONLY
SCM_EXPORT ScmObj
scm_make_char(scm_ichar_t val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CHAR_INIT(obj, val);
    return obj;
}
#endif /* not SCM_SAL_HAS_IMMEDIATE_INT_ONLY */

#if SCM_USE_HYGIENIC_MACRO
SCM_EXPORT ScmObj
scm_make_hmacro(ScmObj rules, ScmObj env)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_HMACRO_INIT(obj, rules, scm_pack_env(env));
    return obj;
}

SCM_EXPORT ScmObj
scm_make_farsymbol(ScmObj sym, ScmPackedEnv env)
{
    ScmObj obj;

#if !SCM_USE_SYNTAX_CASE
    if (SCM_FARSYMBOLP(sym) && SCM_FARSYMBOL_ENV(sym) > env)
        scm_macro_bad_scope(sym);
#endif
    obj = scm_alloc_cell();
    SCM_FARSYMBOL_INIT(obj, sym, env);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_subpat(ScmObj x, scm_int_t meta)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_SUBPAT_INIT(obj, x, meta);
    return obj;
}
#endif /* SCM_USE_HYGIENIC_MACRO */

static ScmObj
scm_make_string_internal(char *str, scm_int_t len, scm_bool is_immutable)
{
    ScmObj obj;

    SCM_ASSERT(str);

    if (len == STRLEN_UNKNOWN) {
#if SCM_USE_MULTIBYTE_CHAR
        len = scm_mb_bare_c_strlen(scm_current_char_codec, str);
#else
        len = strlen(str);
#endif
    }

    obj = scm_alloc_cell();
    SCM_STRING_INIT(obj, str, len, !is_immutable);

    return obj;
}

SCM_EXPORT ScmObj
scm_make_immutable_string(char *str, scm_int_t len)
{
    return scm_make_string_internal(str, len, scm_true);
}

SCM_EXPORT ScmObj
scm_make_immutable_string_copying(const char *str, scm_int_t len)
{
    return scm_make_string_internal(scm_strdup(str), len, scm_true);
}

SCM_EXPORT ScmObj
scm_make_string(char *str, scm_int_t len)
{
    return scm_make_string_internal(str, len, scm_false);
}

SCM_EXPORT ScmObj
scm_make_string_copying(const char *str, scm_int_t len)
{
    return scm_make_string_internal(scm_strdup(str), len, scm_false);
}

SCM_EXPORT ScmObj
scm_make_func(enum ScmFuncTypeCode type, ScmFuncType func)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_FUNC_INIT(obj, type, func);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_closure(ScmObj exp, ScmObj env)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CLOSURE_INIT(obj, exp, env);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_vector(ScmObj *vec, scm_int_t len)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_MUTABLE_VECTOR_INIT(obj, vec, len);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_immutable_vector(ScmObj *vec, scm_int_t len)
{
    ScmObj obj;

    /* Since this function is rarely used, the inefficiency is not a problem */
    obj = scm_make_vector(vec, len);
    SCM_VECTOR_SET_IMMUTABLE(obj);

    return obj;
}

SCM_EXPORT ScmObj
scm_make_port(ScmCharPort *cport, enum ScmPortFlag flag)
{
    ScmObj obj;

    obj = scm_alloc_cell();

    if (flag & SCM_PORTFLAG_INPUT)
        flag |= SCM_PORTFLAG_LIVE_INPUT;
    if (flag & SCM_PORTFLAG_OUTPUT)
        flag |= SCM_PORTFLAG_LIVE_OUTPUT;

    SCM_PORT_INIT(obj, cport, flag);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_continuation(void)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CONTINUATION_INIT(obj, INVALID_CONTINUATION_OPAQUE, 0);
    return obj;
}

#if !SCM_USE_VALUECONS
SCM_EXPORT ScmObj
scm_make_value_packet(ScmObj values)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_VALUEPACKET_INIT(obj, values);
    return obj;
}
#endif

#if SCM_USE_SSCM_EXTENSIONS
SCM_EXPORT ScmObj
scm_make_cpointer(void *ptr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_C_POINTER_INIT(obj, ptr);
    return obj;
}

SCM_EXPORT ScmObj
scm_make_cfunc_pointer(ScmCFunc ptr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_C_FUNCPOINTER_INIT(obj, ptr);
    return obj;
}
#endif /* SCM_USE_SSCM_EXTENSIONS */

#if SCM_USE_STORAGE_COMPACT
SCM_EXPORT enum ScmObjType
scm_type(ScmObj obj)
{
    switch (SCM_PTAG(obj)) {
    case SCM_PTAG_CONS:
        return ScmCons;

    case SCM_PTAG_CLOSURE:
        return ScmClosure;

    case SCM_PTAG_MISC:
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
        PLAIN_ERR(" invalid others object: ptr = ~P", (void *)obj);

    case SCM_PTAG_IMM:
        if (INTP(obj))
            return ScmInt;
        else if (CHARP(obj))
            return ScmChar;
        else if (SCM_CONSTANTP(obj))
            return ScmConstant;
        PLAIN_ERR("invalid imm object: ptr = ~P", (void *)obj);

    default:
        PLAIN_ERR("invalid object: ptr = ~P", (void *)obj);
    }

    /* NOTREACHED */
}
#endif /* SCM_USE_STORAGE_COMPACT */
