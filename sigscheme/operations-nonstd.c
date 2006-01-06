/*===========================================================================
 *  FileName : operations-nonstd.c
 *  About    : SigScheme specific non standard operations
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

/*=======================================
  System Include
=======================================*/
#include <stddef.h>
#include <stdio.h>

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
/* io.c */
extern const char *scm_lib_path;

/*=======================================
  File Local Function Declarations
=======================================*/
static void scm_require_internal(const char *filename);
static ScmObj make_loaded_str(const char *filename);

/*=======================================
  Function Implementations
=======================================*/
void
scm_initialize_nonstd_features(void)
{
    REGISTER_FUNC_TABLE(nonstd_func_info_table);

    scm_define_alias("call/cc", "call-with-current-continuation");
}

/*
 * TODO:
 * - describe compatibility with de facto standard of other Scheme
 *   implementations (accept env as optional arg, etc)
 *
 * NOTE: Gauche 0.8.6 has deprecated symbol-bound? and is going to replace the
 * procedure with global-variable-bound?.
 */
/* The implementation is fully compatible with SIOD */
ScmObj
scm_p_symbol_boundp(ScmObj sym, ScmObj rest)
{
    ScmObj env;
    ScmRef ref;
    DECLARE_FUNCTION("symbol-bound?", procedure_variadic_1);

    ENSURE_SYMBOL(sym);

    env = POP_ARG(rest);
    if (VALIDP(env))
        ENSURE_ENV(env);
    else
        env = SCM_INTERACTION_ENV;
    ref = scm_lookup_environment(sym, env);

    return MAKE_BOOL(ref != SCM_INVALID_REF || SCM_SYMBOL_BOUNDP(sym));
}

/* SIOD compatible */
ScmObj
scm_p_load_path(void)
{
    DECLARE_FUNCTION("load-path", procedure_fixed_0);

    return MAKE_IMMUTABLE_STRING_COPYING(scm_lib_path);
}

void
scm_require(const char *filename)
{
#if !SCM_GCC4_READY_GC
    ScmObj stack_start;
#endif

#if SCM_GCC4_READY_GC
    SCM_GC_PROTECTED_CALL_VOID(scm_require_internal, (filename));
#else
    scm_gc_protect_stack(&stack_start);

    scm_require_internal(filename);

    scm_gc_unprotect_stack(&stack_start);
#endif
}

static void
scm_require_internal(const char *filename)
{
    ScmObj loaded_str;

    loaded_str = make_loaded_str(filename);
    if (!scm_providedp(loaded_str)) {
        scm_load(filename);
        scm_provide(loaded_str);
    }
}

ScmObj
scm_p_require(ScmObj filename)
{
#if SCM_COMPAT_SIOD
    ScmObj loaded_str, retsym;
#endif
    DECLARE_FUNCTION("require", procedure_fixed_1);

    ENSURE_STRING(filename);

    scm_require_internal(SCM_STRING_STR(filename));

#if SCM_COMPAT_SIOD
    loaded_str = make_loaded_str(SCM_STRING_STR(filename));
    retsym = scm_intern(SCM_STRING_STR(loaded_str));
    SCM_SYMBOL_SET_VCELL(retsym, SCM_TRUE);

    return retsym;
#else
    return SCM_TRUE;
#endif
}

static ScmObj
make_loaded_str(const char *filename)
{
    char *loaded_str;
    size_t size;

    size = strlen(filename) + sizeof("*-loaded*");
    loaded_str = scm_malloc(size);
    snprintf(loaded_str, size, "*%s-loaded*", filename);

    return MAKE_IMMUTABLE_STRING(loaded_str);
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj
scm_p_provide(ScmObj feature)
{
    DECLARE_FUNCTION("provide", procedure_fixed_1);

    ENSURE_STRING(feature);

    scm_provide(feature);

    return SCM_TRUE;
}

/*
 * TODO: replace original specification with a SRFI standard or other de facto
 * standard
 */
ScmObj
scm_p_providedp(ScmObj feature)
{
    DECLARE_FUNCTION("provided?", procedure_fixed_1);

    ENSURE_STRING(feature);

    return MAKE_BOOL(scm_providedp(feature));
}

/*
 * TODO: describe compatibility with de facto standard of other Scheme
 * implementations
 */
ScmObj
scm_p_file_existsp(ScmObj filepath)
{
    FILE *f;
    DECLARE_FUNCTION("file-exists?", procedure_fixed_1);

    ENSURE_STRING(filepath);

    f = fopen(SCM_STRING_STR(filepath), "r");
    if (!f)
        return SCM_FALSE;
    fclose(f);

    return SCM_TRUE;
}

/* TODO: remove to ensure security */
ScmObj
scm_p_delete_file(ScmObj filepath)
{
    DECLARE_FUNCTION("delete-file", procedure_fixed_1);

    ENSURE_STRING(filepath);

    if (remove(SCM_STRING_STR(filepath)) == -1)
        ERR_OBJ("delete failed. file = ", filepath);

    return SCM_TRUE;
}

/* to avoid being typo of length+, this procedure did not name as length++ */
/* FIXME: replace with a SRFI or de facto standard equivalent if exist */
ScmObj
scm_p_lengthstar(ScmObj lst)
{
    int len;
    DECLARE_FUNCTION("length*", procedure_fixed_1);

    len = scm_length(lst);
    if (!SCM_LISTLEN_PROPERP(len)) { /* make fast path for proper list */
        if (SCM_LISTLEN_DOTTEDP(len))
            len = -SCM_LISTLEN_DOTTED(len);
        else if (SCM_LISTLEN_CIRCULARP(len))
            return SCM_FALSE;
    }

    return MAKE_INT(len);
}
