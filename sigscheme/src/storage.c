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

#include <stdlib.h>

#include "sigscheme.h"
#include "sigschemeinternal.h"
#if SCM_USE_MULTIBYTE_CHAR
#include "encoding.h"
#else
#include "encoding-dummy.h"
#endif

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

/* SCM_*_INIT() macros should not be a SAL interface because it taints the SAL
 * abstraction model with an unwanted restriction. It should be an internal
 * interface of storage-compact and storage-fatty.
 *
 * Since the INIT() interface assumes that any cell of a storage is generic and
 * non-colored, and requires 'all-purpose' cell allocation ability. The
 * assumption prevents various optimized implementations such as type-by-type
 * heap or freelist, and preallocated constant objects sharing. Even if the
 * examples sound not so attractive, we should keep SAL interface abstract to
 * allow implementing any experimental ideas as best as possible. Since one of
 * the most crucial development motivations of SigScheme and SAL is gaining
 * maximum storage optimization capability based on platform-by-platform system
 * characteristics (especially on embedded platforms), the restriction is
 * difficult to accept.  -- YamaKen 2006-05-30  */
/* SCM_SAL_*_INIT()s are renamed to SCM_ISAL_*_INIT() to distinguish them from
 * SAL. ISAL stands for "Internal SAL".  -- YamaKen 2006-06-24 */
#define SCM_CONS_INIT(obj, kar, kdr)                    \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_CONS_INIT,         \
                            (ScmObj, ScmObj, ScmObj),   \
                            ((obj), (kar), (kdr)))

#define SCM_IMMUTABLE_CONS_INIT(obj, kar, kdr)                  \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_IMMUTABLE_CONS_INIT,       \
                            (ScmObj, ScmObj, ScmObj),           \
                            ((obj), (kar), (kdr)))

#define SCM_CLOSURE_INIT(obj, exp, env)                 \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_CLOSURE_INIT,      \
                            (ScmObj, ScmObj, ScmObj),   \
                            ((obj), (exp), (env)))

#if !SCM_HAS_IMMEDIATE_CHAR_ONLY
#define SCM_CHAR_INIT(obj, val)                         \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_CHAR_INIT,         \
                            (ScmObj, scm_ichar_t),      \
                            ((obj), (val)))
#endif

#if !SCM_HAS_IMMEDIATE_INT_ONLY
#define SCM_INT_INIT(obj, val)                          \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_INT_INIT,          \
                            (ScmObj, scm_int_t),        \
                            ((obj), (val)))
#endif

#define SCM_SYMBOL_INIT(obj, name, val)                 \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_SYMBOL_INIT,       \
                            (ScmObj, char *, ScmObj),   \
                            ((obj), (name), (val)))

#define SCM_STRING_INIT(obj, str, len, mutp)                            \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_STRING_INIT,                       \
                            (ScmObj, char *, scm_int_t, scm_bool),      \
                            ((obj), (str), (len), (mutp)))

#define SCM_MUTABLE_STRING_INIT(obj, str, len)                  \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_MUTABLE_STRING_INIT,       \
                            (ScmObj, char *, scm_int_t),        \
                            ((obj), (str), (len)))

#define SCM_IMMUTABLE_STRING_INIT(obj, str, len)                \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_IMMUTABLE_STRING_INIT,     \
                            (ScmObj, char *, scm_int_t),        \
                            ((obj), (str), (len)))

#define SCM_FUNC_INIT(obj, type, func)                                   \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_FUNC_INIT,                          \
                            (ScmObj, enum ScmFuncTypeCode, ScmFuncType), \
                            ((obj), (type), (func)))

#define SCM_VECTOR_INIT(obj, vec, len)                          \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_VECTOR_INIT,               \
                            (ScmObj, ScmObj *, scm_int_t),      \
                            ((obj), (vec), (len)))

#define SCM_MUTABLE_VECTOR_INIT(obj, vec, len)                  \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_MUTABLE_VECTOR_INIT,       \
                            (ScmObj, ScmObj *, scm_int_t),      \
                            ((obj), (vec), (len)))

#define SCM_IMMUTABLE_VECTOR_INIT(obj, vec, len)                \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_IMMUTABLE_VECTOR_INIT,     \
                            (ScmObj, ScmObj *, scm_int_t),      \
                            ((obj), (vec), (len)))

#define SCM_PORT_INIT(obj, cport, flag)                         \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_PORT_INIT,                 \
                            (ScmObj, struct ScmCharPort_ *,     \
                             enum ScmPortFlag),                 \
                            ((obj), (cport), (flag)))

#define SCM_CONTINUATION_INIT(obj, opaque, tag)          \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_CONTINUATION_INIT,  \
                            (ScmObj, void *, scm_int_t), \
                            ((obj), (opaque), (tag)))

#if SCM_USE_SSCM_EXTENSIONS
/* SCM_C_POINTER_INIT(obj, void *ptr) */
#define SCM_C_POINTER_INIT(obj, ptr)                    \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_C_POINTER_INIT,    \
                            (ScmObj, void *),           \
                            ((obj), (ptr)))
/* SCM_C_FUNCPOINTER_INIT(obj, ScmCFunc ptr) */
#define SCM_C_FUNCPOINTER_INIT(obj, ptr)                 \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_C_FUNCPOINTER_INIT, \
                            (ScmObj, ScmCFunc),          \
                            ((obj), (ptr)))
#endif /* SCM_USE_SSCM_EXTENSIONS */
#define SCM_VALUEPACKET_INIT(obj, vals)                 \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_VALUEPACKET_INIT,  \
                            (ScmObj, ScmObj),           \
                            ((obj), (vals)))

#define SCM_HMACRO_INIT(obj, r, e)                              \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_HMACRO_INIT,               \
                            (ScmObj, ScmObj, ScmPackedEnv),     \
                            ((obj), (r), (e)))

#define SCM_FARSYMBOL_INIT(obj, s, e)                           \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_FARSYMBOL_INIT,            \
                            (ScmObj, ScmObj, ScmPackedEnv),     \
                            ((obj), (s), (e)))

#define SCM_SUBPAT_INIT(obj, x, m)                              \
    SCM_TYPESAFE_MACRO_VOID(SCM_ISAL_SUBPAT_INIT,               \
                            (ScmObj, ScmObj, scm_int_t),        \
                            ((obj), (x), (m)))

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
#if SCM_USE_STORAGE_FATTY
static void initialize_special_constants(void);
#endif
#if SCM_USE_STRING
static ScmObj scm_make_string_internal(char *str, scm_int_t len,
                                       scm_bool immutablep);
#endif

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

    initialize_special_constants();
#endif

    if (!conf)
        conf = &default_storage_conf;
    scm_init_gc(conf);

#if SCM_USE_VALUECONS
    /* To keep storage model abstract, the cell is allocated from a heap
     * instead of directly construct ScmCell. */
    scm_gc_protect_with_init(&scm_null_values, CONS(SCM_NULL, SCM_NULL));
    SCM_ENTYPE(scm_null_values, ScmValuePacket);
#endif

#if SCM_USE_CONTINUATION
    scm_init_continuation();
#endif
    scm_init_symbol(conf);
}

SCM_EXPORT void
scm_fin_storage(void)
{
    scm_fin_symbol();
#if SCM_USE_CONTINUATION
    scm_fin_continuation();
#endif
    scm_fin_gc();

#if SCM_USE_STORAGE_FATTY
    SCM_GLOBAL_VARS_FIN(static_storage_fatty);
    SCM_GLOBAL_VARS_FIN(storage_fatty);
#endif
    SCM_GLOBAL_VARS_FIN(storage);
}

/*===========================================================================
  Scheme Constants
===========================================================================*/
/*
 * To keep storage representation abstract, the special constants
 * initialization is encapsulated in this file. Upper layers must only use
 * abstract interfaces such as SCM_NULL and SCM_NULLP().
 */
#if SCM_USE_STORAGE_FATTY
static void
initialize_special_constants(void)
{
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_null,    l_null_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_true,    l_true_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_false,   l_false_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_eof,     l_eof_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_unbound, l_unbound_cell);
    SCM_CONSTANT_BIND_SUBSTANCE(scm_const_undef,   l_undef_cell);
}
#endif /* SCM_USE_STORAGE_FATTY */

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

#if SCM_HAS_IMMUTABLE_CONS
SCM_EXPORT ScmObj
scm_make_immutable_cons(ScmObj kar, ScmObj kdr)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_IMMUTABLE_CONS_INIT(obj, kar, kdr);
    return obj;
}
#endif /* has immutable cons */

#if (SCM_USE_INT && !SCM_HAS_IMMEDIATE_INT_ONLY)
SCM_EXPORT ScmObj
scm_make_int(scm_int_t val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_INT_INIT(obj, val);
    return obj;
}
#endif /* (SCM_USE_INT && !SCM_HAS_IMMEDIATE_INT_ONLY) */

SCM_EXPORT ScmObj
scm_make_symbol(char *name, ScmObj val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_SYMBOL_INIT(obj, name, val);
    return obj;
}

#if (SCM_USE_CHAR && !SCM_HAS_IMMEDIATE_CHAR_ONLY)
SCM_EXPORT ScmObj
scm_make_char(scm_ichar_t val)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CHAR_INIT(obj, val);
    return obj;
}
#endif /* (SCM_USE_CHAR && !SCM_HAS_IMMEDIATE_CHAR_ONLY) */

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

#if SCM_USE_STRING
static ScmObj
scm_make_string_internal(char *str, scm_int_t len, scm_bool immutablep)
{
    ScmObj obj;

    SCM_ASSERT(str);

    if (len == STRLEN_UNKNOWN)
        len = scm_mb_bare_c_strlen(scm_current_char_codec, str);

    obj = scm_alloc_cell();
    SCM_STRING_INIT(obj, str, len, !immutablep);

    return obj;
}

#if SCM_HAS_IMMUTABLE_STRING
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
#endif /* SCM_HAS_IMMUTABLE_STRING */

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
#endif /* SCM_USE_STRING */

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

#if SCM_USE_VECTOR
SCM_EXPORT ScmObj
scm_make_vector(ScmObj *vec, scm_int_t len)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_MUTABLE_VECTOR_INIT(obj, vec, len);
    return obj;
}

#if SCM_HAS_IMMUTABLE_VECTOR
SCM_EXPORT ScmObj
scm_make_immutable_vector(ScmObj *vec, scm_int_t len)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_IMMUTABLE_VECTOR_INIT(obj, vec, len);
    return obj;
}
#endif /* SCM_HAS_IMMUTABLE_VECTOR */
#endif /* SCM_USE_VECTOR */

#if SCM_USE_PORT
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
#endif /* SCM_USE_PORT */

#if SCM_USE_CONTINUATION
SCM_EXPORT ScmObj
scm_make_continuation(void)
{
    ScmObj obj;

    obj = scm_alloc_cell();
    SCM_CONTINUATION_INIT(obj, INVALID_CONTINUATION_OPAQUE, 0);
    return obj;
}
#endif /* SCM_USE_CONTINUATION */

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
#if SCM_USE_STRING
        else if (STRINGP(obj))
            return ScmString;
#endif
#if SCM_USE_VECTOR
        else if (VECTORP(obj))
            return ScmVector;
#endif
        else if (VALUEPACKETP(obj))
            return ScmValuePacket;
        else if (FUNCP(obj))
            return ScmFunc;
#if SCM_USE_PORT
        else if (PORTP(obj))
            return ScmPort;
#endif
#if SCM_USE_CONTINUATION
        else if (CONTINUATIONP(obj))
            return ScmContinuation;
#endif
        else if (SCM_CONSTANTP(obj))
            return ScmConstant;
#if SCM_USE_SSCM_EXTENSIONS
        else if (C_POINTERP(obj))
            return ScmCPointer;
        else if (C_FUNCPOINTERP(obj))
            return ScmCFuncPointer;
#endif
#if SCM_USE_HYGIENIC_MACRO
        else if (HMACROP(obj))
            return ScmMacro;
        else if (FARSYMBOLP(obj))
            return ScmFarsymbol;
        else if (SUBPATP(obj))
            return ScmSubpat;
#endif
        else if (FREECELLP(obj))
            return ScmFreeCell;
        PLAIN_ERR("invalid misc object: ptr = ~P", (void *)obj);

    case SCM_PTAG_IMM:
#if SCM_USE_INT
        if (INTP(obj))
            return ScmInt;
#endif
#if SCM_USE_CHAR
        if (CHARP(obj))
            return ScmChar;
#endif
        if (SCM_CONSTANTP(obj))
            return ScmConstant;
        PLAIN_ERR("invalid imm object: ptr = ~P", (void *)obj);

    default:
        PLAIN_ERR("invalid object: ptr = ~P", (void *)obj);
    }

    /* NOTREACHED */
}
#endif /* SCM_USE_STORAGE_COMPACT */
