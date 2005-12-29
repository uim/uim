/*===========================================================================
 *  FileName : storage-symbol.c
 *  About    : Scheme Symbol handling
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

/*=======================================
  File Local Struct Declarations
=======================================*/

/*=======================================
  File Local Macro Declarations
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/
ScmObj *scm_symbol_hash;

/*=======================================
  File Local Function Declarations
=======================================*/
static void initialize_symbol_hash(void);
static void finalize_symbol_hash(void);
static int  symbol_name_hash(const char *name);

/*=======================================
  Function Implementations
=======================================*/
ScmObj
scm_intern(const char *name)
{
    ScmObj sym, lst, rest;
    int hash;

    hash = symbol_name_hash(name);
    lst = scm_symbol_hash[hash];

    for (rest = lst; CONSP(rest); rest = CDR(rest)) {
        sym = CAR(rest);
        if (strcmp(SCM_SYMBOL_NAME(sym), name) == 0)
            return sym;
    }

    /* if not found, allocate new symbol object and prepend it into the list */
    sym = MAKE_SYMBOL(strdup(name), SCM_UNBOUND);
    scm_symbol_hash[hash] = CONS(sym, lst);

    return sym;
}

/* lookup the symbol bound to an obj reversely */
ScmObj
scm_symbol_bound_to(ScmObj obj)
{
    int i;
    ScmObj lst, sym, val;

    for (i = 0; i < NAMEHASH_SIZE; i++) {
        for (lst = scm_symbol_hash[i]; CONSP(lst); lst = CDR(lst)) {
            sym = CAR(lst);
            val = SCM_SYMBOL_VCELL(sym);
            if (!EQ(val, SCM_UNBOUND) && EQ(val, obj))
                return sym;
        }
    }

    return SCM_FALSE;
}

void
scm_init_symbol(void)
{
    initialize_symbol_hash();
}

void
scm_finalize_symbol(void)
{
    finalize_symbol_hash();
}

/*============================================================================
  Symbol table
============================================================================*/
static void
initialize_symbol_hash(void)
{
    int i;

    scm_symbol_hash = scm_malloc(sizeof(ScmObj) * NAMEHASH_SIZE);

    for (i = 0; i < NAMEHASH_SIZE; i++)
        scm_symbol_hash[i] = SCM_NULL;
}

static void
finalize_symbol_hash(void)
{
    free(scm_symbol_hash);
}

static int
symbol_name_hash(const char *name)
{
    int hash, c;

    for (hash = 0; (c = *name); name++)
        hash = ((hash * 17) ^ c) % NAMEHASH_SIZE;

    return hash;
}
