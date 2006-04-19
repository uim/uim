/*===========================================================================
 *  FileName : symbol.c
 *  About    : Scheme Symbol handling
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

#include <string.h>
#include <stdlib.h>

#include "my-stdint.h"
#include "sigscheme.h"
#include "sigschemeinternal.h"

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
SCM_DEFINE_EXPORTED_VARS(symbol);

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_symbol_hash(const ScmStorageConf *conf);
static void finalize_symbol_hash(void);
static uint32_t symbol_name_hash(const char *name);

/*=======================================
  Function Implementations
=======================================*/
SCM_EXPORT ScmObj
scm_intern(const char *name)
{
    ScmObj sym, lst, rest;
    uint32_t hash;
    DECLARE_INTERNAL_FUNCTION("scm_intern");

#if (SCM_USE_SRFI75 && SCM_STRICT_ARGCHECK)
#if 0
    /* FIXME: detect error correctly */
    if (scm_mb_bare_c_strlen(scm_identifier_codec, name) < 0)
        ERR("invalid string for identifier: ~S", name);
#endif
#endif

    hash = symbol_name_hash(name);
    lst = scm_symbol_hash[hash];

    rest = lst;
    FOR_EACH (sym, rest) {
        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0)
            return sym;
    }

    /* if not found, allocate new symbol object and prepend it into the list */
    sym = MAKE_SYMBOL(scm_strdup(name), SCM_UNBOUND);
    scm_symbol_hash[hash] = CONS(sym, lst);

    return sym;
}

/* lookup the symbol bound to an obj reversely */
SCM_EXPORT ScmObj
scm_symbol_bound_to(ScmObj obj)
{
    ScmObj lst, sym, val;
    size_t i;
    DECLARE_INTERNAL_FUNCTION("scm_symbol_bound_to");

    for (i = 0; i < scm_symbol_hash_size; i++) {
        lst = scm_symbol_hash[i];
        FOR_EACH (sym, lst) {
            val = SCM_SYMBOL_VCELL(sym);
            if (!EQ(val, SCM_UNBOUND) && EQ(val, obj))
                return sym;
        }
    }

    return SCM_FALSE;
}

SCM_EXPORT void
scm_init_symbol(const ScmStorageConf *conf)
{
    SCM_GLOBAL_VARS_INIT(symbol);

    initialize_symbol_hash(conf);
}

SCM_EXPORT void
scm_finalize_symbol(void)
{
    finalize_symbol_hash();
}

/*===========================================================================
  Symbol table
===========================================================================*/
static void
initialize_symbol_hash(const ScmStorageConf *conf)
{
    size_t i;

    scm_symbol_hash_size = conf->symbol_hash_size;
    scm_symbol_hash      = scm_malloc(sizeof(ScmObj) * scm_symbol_hash_size);

    for (i = 0; i < scm_symbol_hash_size; i++)
        scm_symbol_hash[i] = SCM_NULL;
}

static void
finalize_symbol_hash(void)
{
    free(scm_symbol_hash);
}

static uint32_t
symbol_name_hash(const char *name)
{
    uint32_t hash, c;

    for (hash = 0; (c = *(const scm_byte_t *)name); name++)
        hash = ((hash * 17) ^ c) % scm_symbol_hash_size;

    return hash;
}
